# Daily presence check (2026-11-13; plan: claude-notes/2026-11-09_daily-presence-check-plan.md,
# scope: 2026-11-12_twice-weekly-and-daily-presence-scope.md; feeds:
# 2026-11-13_presence-redrover-opi-wsba.md).
#
# Each day, every board that gives a posting a stable key is read once:
#   - registry boards on platforms with per-posting links (AppliTrack,
#     SchoolSpring, TedK12, Tyler Portico, Red Rover), list pages only, with
#     the same fetch_* call the weekly run makes for that district
#     (resolve_scraper_call()); the key is k12_link_posting_id() of the link;
#   - statewide feeds a state passes as presence_feed()s (MT's OPI, WY's
#     WSBA), keyed by a fingerprint of what the feed shows (opi_posting_key(),
#     wsba_vacancy_key()), since neither gives a posting ID.
# Two files, kept on a side branch (`presence`), never main:
#   - presence_checks.csv: one row per board per day (Date, Board, Platform,
#     Status ok/empty/error, N_Postings, N_Keyed, Seconds, Error). A posting
#     missing from a board is read as gone only on a day its board answered.
#   - presence_sightings.csv: one row per unbroken stretch of sightings of a
#     posting on a board (Board, Platform, District, Link_Key, List_Key,
#     First_Day, Last_Day, Days_Seen, Title, URL). A stretch ends when the
#     board answered a check (ok or empty) without the posting; a day the
#     board errored, or the job didn't run, doesn't break it. For a district's
#     own board, Board is the district; for a feed, the feed, and District is
#     the posting's district. List_Key: what a feed's list page shows of the
#     posting, so a later day can reuse its key without its detail page (OPI).
# The weekly archives give the same keys (the same link, or the same
# fingerprint from the archived columns), so phase 1 can join the two.

#' A per-posting ID from a K-12 posting's link
#'
#' Keyed by platform and number (plus the tenant where numbers are per tenant),
#' so a host or path change keeps the ID: AppliTrack `applitrack:<tenant>:<n>`,
#' SchoolSpring `schoolspring:<n>`, Red Rover `redrover:<org>:<n>`, TedK12
#' `tedk12:<subdomain>:<n>`, Tyler Portico `tylerportico:<tenant>:<id>` (a GUID
#' or a number). A board page or any other link is NA. Moved from WY's
#' `k12_he_classification.R` in 0.13.0 (Tyler Portico added).
#' @param url Character vector of posting links.
#' @return Character vector, NA where the link isn't a per-posting one.
#' @export
k12_link_posting_id <- function(url) {
  if (is.null(url)) return(character(0))
  u <- tolower(trimws(fix_title_encoding(as.character(url)))); u[is.na(u)] <- ""
  out <- rep(NA_character_, length(u))
  pick <- function(pattern, fmt) {
    hit <- is.na(out) & grepl(pattern, u, perl = TRUE)
    out[hit] <<- sub(paste0("^.*?", pattern, ".*$"), fmt, u[hit], perl = TRUE)
  }
  pick("applitrack\\.com/([^/]+)/.*applitrackjobid=(\\d+)", "applitrack:\\1:\\2")
  pick("([a-z0-9-]+)\\.schoolspring\\.com/jobs/(\\d+)", "schoolspring:\\2")
  pick("redroverk12\\.com/org/([^/]+)/opening/(\\d+)", "redrover:\\1:\\2")
  pick("([a-z0-9-]+)\\.tedk12\\.com/hire/viewjob\\.aspx\\?jobid=(\\d+)", "tedk12:\\1:\\2")
  pick("([a-z0-9-]+)\\.tylerportico\\.com/.*/job-list/([0-9a-f-]+)", "tylerportico:\\1:\\2")  # a GUID or a number
  out
}

# ---------------------------------------------------------------------------
# Fingerprint keys for feeds without posting IDs
# ---------------------------------------------------------------------------

#' A fingerprint part's normal form
#'
#' Lower case, runs of whitespace as one space, trimmed, NA as `""`.
#' @param x Character vector.
#' @export
presence_norm <- function(x) {
  x <- tolower(gsub("[[:space:]]+", " ", trimws(as.character(x))))
  x[is.na(x)] <- ""
  x
}

#' A posted date as YYYY-MM-DD, from either form the feeds use
#'
#' `"11/12/2026"` (OPI's list page, WSBA's page) and `"2026-11-12"` (the
#' archives, after `standardize_date()`) both give `"2026-11-12"`; anything
#' else gives `""`.
#' @param x Character vector.
#' @export
presence_iso_date <- function(x) {
  x <- trimws(as.character(x))
  us <- suppressWarnings(as.Date(x, format = "%m/%d/%Y"))
  iso <- suppressWarnings(as.Date(substr(x, 1, 10), format = "%Y-%m-%d"))
  out <- ifelse(!is.na(us) & grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", x), format(us),
                ifelse(!is.na(iso) & grepl("^\\d{4}-\\d{2}-\\d{2}", x), format(iso), ""))
  out[is.na(x)] <- ""
  out
}

presence_hash <- function(parts) {
  vapply(parts, function(p) substr(unname(tools::md5sum(bytes = charToRaw(enc2utf8(p)))), 1, 16), character(1),
         USE.NAMES = FALSE)
}

#' Number repeated keys within one day's list
#'
#' `c("a", "b", "a")` -> `c("a#1", "b#1", "a#2")`, in list order. True double
#' listings (the same posting listed twice) share a fingerprint; the suffix
#' keeps them apart without claiming which is which.
#' @param base Character vector of fingerprints.
#' @export
presence_occurrence_keys <- function(base) {
  if (length(base) == 0) return(character(0))
  paste0(base, "#", stats::ave(seq_along(base), base, FUN = seq_along))
}

#' Fingerprint of an OPI posting
#'
#' OPI has no posting ID anywhere (the detail view is a postback, its
#' ViewState is encrypted, there is no export). Its detail page's district,
#' school, the list's title, the posted date and the description identify a
#' posting: on MT's archives, repeated within a week only for true double
#' listings (8 on 11-03 and on 11-10), and unchanged for all 324 postings
#' listed both weeks. The description is the archived excerpt (whitespace
#' squeezed, first 600 characters; applying it again changes nothing), so the
#' weekly archive's `OPI_District`, `OPI_School`, `title`, `date_posted`,
#' `OPI_Description` give the same key. Without an occurrence suffix: see
#' [presence_occurrence_keys()].
#' @param district,school,title,posted,description Character vectors.
#' @return `opi:<16 hex>`, or NA where district and description are both blank
#'   (the detail page wasn't read).
#' @export
opi_posting_key <- function(district, school, title, posted, description) {
  excerpt <- substr(gsub("\\s+", " ", ifelse(is.na(description), "", as.character(description))), 1, 600)
  d <- presence_norm(district); e <- presence_norm(excerpt)
  key <- paste0("opi:", presence_hash(paste(d, presence_norm(school), presence_norm(title), presence_iso_date(posted),
                                            e, sep = "\u001f")))
  key[!nzchar(d) & !nzchar(e)] <- NA_character_
  key
}

#' Fingerprint of a WSBA vacancy
#'
#' WSBA's list links district pages and files, never a posting. District,
#' title and WSBA's own posted date: repeated 2 times in 47 on 2026-11-13.
#' Without an occurrence suffix: see [presence_occurrence_keys()].
#' @param district,title,posted Character vectors.
#' @return `wsba:<16 hex>`.
#' @export
wsba_vacancy_key <- function(district, title, posted) {
  paste0("wsba:", presence_hash(paste(presence_norm(district), presence_norm(title), presence_iso_date(posted),
                                      sep = "\u001f")))
}

# ---------------------------------------------------------------------------
# The check
# ---------------------------------------------------------------------------

PRESENCE_CHECKS_EMPTY <- data.frame(Date = as.Date(character(0)), Board = character(0), Platform = character(0),
                                    Status = character(0), N_Postings = integer(0), N_Keyed = integer(0),
                                    Seconds = numeric(0), Error = character(0), stringsAsFactors = FALSE)
PRESENCE_SIGHTINGS_EMPTY <- data.frame(Board = character(0), Platform = character(0), District = character(0),
                                       Link_Key = character(0), List_Key = character(0),
                                       First_Day = as.Date(character(0)), Last_Day = as.Date(character(0)),
                                       Days_Seen = integer(0), Title = character(0), URL = character(0),
                                       stringsAsFactors = FALSE)
PRESENCE_SEEN_EMPTY <- data.frame(Board = character(0), Platform = character(0), District = character(0),
                                  Link_Key = character(0), List_Key = character(0), title = character(0),
                                  url = character(0), stringsAsFactors = FALSE)

#' Today's date in Mountain time
#'
#' GitHub starts a scheduled job hours late; the day a check counts for is the
#' day in the boards' own time zone, not UTC.
#' @param tz Time zone.
#' @export
presence_today <- function(tz = "America/Denver") as.Date(format(Sys.time(), tz = tz))

#' One board's fetch result as title, link and key
#'
#' Reads the columns the platform fetchers return: `title`/`Title`, and
#' `url`/`Link`, or AppliTrack's `job_id` (made into the posting link the
#' weekly run stores, [applitrack_posting_url()], with the tenant from the
#' call's first argument). Rows whose link isn't a per-posting one keep an NA
#' `Link_Key`; a key repeated on the board is kept once.
#' @param result A `fetch_*` result.
#' @param call The `resolve_scraper_call()` it came from.
#' @return data.frame(title, url, Link_Key).
#' @export
presence_rows <- function(result, call) {
  empty <- data.frame(title = character(0), url = character(0), Link_Key = character(0), stringsAsFactors = FALSE)
  if (is.null(result) || nrow(result) == 0) return(empty)
  pick <- function(cols) { hit <- intersect(cols, names(result)); if (length(hit)) as.character(result[[hit[1]]]) else rep(NA_character_, nrow(result)) }
  title <- pick(c("title", "Title"))
  url <- if ("job_id" %in% names(result)) applitrack_posting_url(as.character(call$args[[1]]), as.character(result$job_id))
         else pick(c("url", "Link"))
  out <- data.frame(title = title, url = url, Link_Key = k12_link_posting_id(url), stringsAsFactors = FALSE)
  out[is.na(out$Link_Key) | !duplicated(out$Link_Key), , drop = FALSE]
}

#' A statewide feed for the daily check
#'
#' @param board The feed's name, used as its `Board` ("OPI Jobs for Teachers
#'   (statewide)").
#' @param platform Its `Platform` ("OPI").
#' @param fetch `function(known)` reading the feed once. `known` is the feed's
#'   rows of `presence_sightings.csv` so far (to reuse keys without detail
#'   pages). Returns a data frame with `District`, `title`, `url`, `Link_Key`
#'   (NA when no key could be made) and optionally `List_Key`; an error is the
#'   feed's error for the day.
#' @return A list of class `edjobs_presence_feed`.
#' @export
presence_feed <- function(board, platform, fetch) {
  stopifnot(is.character(board), length(board) == 1, nzchar(board), is.function(fetch))
  structure(list(board = board, platform = platform, fetch = fetch), class = "edjobs_presence_feed")
}

presence_check_row <- function(today, board, platform, rows, secs, error = NULL) {
  if (!is.null(error)) {
    return(data.frame(Date = today, Board = board, Platform = platform, Status = "error", N_Postings = NA_integer_,
                      N_Keyed = NA_integer_, Seconds = secs,
                      Error = substr(gsub("[\r\n]+", " ", conditionMessage(error)), 1, 300), stringsAsFactors = FALSE))
  }
  data.frame(Date = today, Board = board, Platform = platform, Status = if (nrow(rows) == 0) "empty" else "ok",
             N_Postings = nrow(rows), N_Keyed = sum(!is.na(rows$Link_Key)), Seconds = secs, Error = NA_character_,
             stringsAsFactors = FALSE)
}

#' Read every board on the given platforms, and every feed, once
#'
#' @param platforms Registry platform names to check (as the state's registry
#'   spells them: "AppliTrack" in MT, "Applitrack" in WY).
#' @param feeds List of [presence_feed()]s.
#' @param today The day the check counts for.
#' @param sources The state's scraper sources (`state_config(scraper_sources = )`).
#' @param sightings The stretches so far (each feed gets its own as `known`).
#' @param delay Seconds between boards.
#' @param envir Where the state's own `fetch_*` functions are defined.
#' @return list(checks = one row per board, seen = one row per keyed posting:
#'   Board, Platform, District, Link_Key, List_Key, title, url).
#' @export
run_presence_check <- function(platforms, feeds = list(), today = presence_today(), sources = state_scraper_sources(),
                               sightings = PRESENCE_SIGHTINGS_EMPTY, delay = 1, envir = globalenv()) {
  stopifnot(all(vapply(feeds, inherits, logical(1), "edjobs_presence_feed")))
  boards <- dplyr::bind_rows(lapply(sources, function(s) {
    if (!identical(s$kind, "registry")) return(NULL)
    # A registry built by code the daily job doesn't load (WY's misc_district_registry) can't hold
    # these platforms' boards; skip it, but say so.
    t <- tryCatch(scraper_source_table(s), error = function(e) {
      warning("run_presence_check(): skipped ", s$where, ": ", conditionMessage(e), call. = FALSE)
      NULL
    })
    if (is.null(t)) return(NULL)
    t <- t[t[[s$platform_col]] %in% platforms, , drop = FALSE]
    data.frame(District = as.character(t[[s$name_col]]), Platform = as.character(t[[s$platform_col]]),
               stringsAsFactors = FALSE)
  }))
  if (nrow(boards)) boards <- boards[!duplicated(boards$District), , drop = FALSE]
  if (nrow(boards) == 0 && length(feeds) == 0) {
    stop("run_presence_check(): no registry rows on platforms ", paste(platforms, collapse = ", "), " and no feeds")
  }

  checks <- list()
  seen <- list()
  for (i in seq_len(nrow(boards))) {
    if (i > 1 && delay > 0) Sys.sleep(delay)
    d <- boards$District[i]
    started <- Sys.time()
    call <- resolve_scraper_call(d, sources)
    rows <- tryCatch({
      if (is.null(call)) stop("no scraper call resolves for this district")
      if (isTRUE(call$session)) stop("this platform's scraper needs a browser session; not checked daily")
      presence_rows(run_scraper_call(call, envir = envir), call)
    }, error = function(e) e)
    secs <- round(as.numeric(difftime(Sys.time(), started, units = "secs")), 1)
    if (inherits(rows, "error")) {
      checks[[length(checks) + 1]] <- presence_check_row(today, d, boards$Platform[i], NULL, secs, rows)
      next
    }
    checks[[length(checks) + 1]] <- presence_check_row(today, d, boards$Platform[i], rows, secs)
    keyed <- rows[!is.na(rows$Link_Key), , drop = FALSE]
    if (nrow(keyed)) seen[[length(seen) + 1]] <- data.frame(Board = d, Platform = boards$Platform[i], District = d,
                                                            Link_Key = keyed$Link_Key, List_Key = NA_character_,
                                                            title = keyed$title, url = keyed$url,
                                                            stringsAsFactors = FALSE)
  }

  for (f in feeds) {
    if (delay > 0 && length(checks)) Sys.sleep(delay)
    started <- Sys.time()
    known <- sightings[sightings$Board == f$board, , drop = FALSE]
    rows <- tryCatch({
      r <- f$fetch(known)
      miss <- setdiff(c("District", "title", "url", "Link_Key"), names(r))
      if (length(miss)) stop("feed returned no column(s) ", paste(miss, collapse = ", "))
      if (!"List_Key" %in% names(r)) r$List_Key <- NA_character_
      r[is.na(r$Link_Key) | !duplicated(r$Link_Key), , drop = FALSE]
    }, error = function(e) e)
    secs <- round(as.numeric(difftime(Sys.time(), started, units = "secs")), 1)
    if (inherits(rows, "error")) {
      checks[[length(checks) + 1]] <- presence_check_row(today, f$board, f$platform, NULL, secs, rows)
      next
    }
    checks[[length(checks) + 1]] <- presence_check_row(today, f$board, f$platform, rows, secs)
    keyed <- rows[!is.na(rows$Link_Key), , drop = FALSE]
    if (nrow(keyed)) seen[[length(seen) + 1]] <- data.frame(Board = f$board, Platform = f$platform,
                                                            District = as.character(keyed$District),
                                                            Link_Key = keyed$Link_Key,
                                                            List_Key = as.character(keyed$List_Key),
                                                            title = as.character(keyed$title),
                                                            url = as.character(keyed$url), stringsAsFactors = FALSE)
  }
  seen <- dplyr::bind_rows(seen)
  if (nrow(seen) == 0) seen <- PRESENCE_SEEN_EMPTY
  list(checks = dplyr::bind_rows(checks), seen = seen)
}

#' Add one day's sightings to the stretches
#'
#' A posting seen today extends its latest stretch on that board when the
#' stretch reached the board's previous answered check (ok or empty);
#' otherwise, including when the board answered a check without it, a new
#' stretch starts. Days the board errored or wasn't checked are skipped over.
#' @param sightings Existing stretches (`presence_sightings.csv`).
#' @param seen Today's postings (`run_presence_check()$seen`).
#' @param checks Every check before today (`presence_checks.csv`).
#' @param today The day of `seen`.
#' @return The updated stretches, with attributes `extended` and `started`
#'   (counts).
#' @export
update_presence_sightings <- function(sightings, seen, checks, today) {
  if (nrow(seen) == 0) return(structure(sightings, extended = 0L, started = 0L))
  answered <- checks[checks$Status %in% c("ok", "empty") & checks$Date < today, , drop = FALSE]
  prev <- if (nrow(answered)) tapply(answered$Date, answered$Board, max) else numeric(0)
  key <- paste(sightings$Board, sightings$Link_Key, sep = "\u001f")
  latest <- if (length(key)) tapply(seq_along(key), key, function(ix) ix[which.max(sightings$Last_Day[ix])]) else integer(0)
  extended <- 0L
  new_rows <- vector("list", nrow(seen))
  for (j in seq_len(nrow(seen))) {
    k <- paste(seen$Board[j], seen$Link_Key[j], sep = "\u001f")
    i <- if (k %in% names(latest)) latest[[k]] else NA_integer_
    p <- if (seen$Board[j] %in% names(prev)) as.Date(prev[[seen$Board[j]]], origin = "1970-01-01") else as.Date(NA)
    if (!is.na(i) && sightings$Last_Day[i] < today && (is.na(p) || sightings$Last_Day[i] >= p)) {
      sightings$Last_Day[i] <- today
      sightings$Days_Seen[i] <- sightings$Days_Seen[i] + 1L
      sightings$District[i] <- seen$District[j]
      sightings$List_Key[i] <- seen$List_Key[j]
      sightings$Title[i] <- seen$title[j]
      sightings$URL[i] <- seen$url[j]
      extended <- extended + 1L
    } else if (is.na(i) || sightings$Last_Day[i] < today) {
      new_rows[[j]] <- data.frame(Board = seen$Board[j], Platform = seen$Platform[j], District = seen$District[j],
                                  Link_Key = seen$Link_Key[j], List_Key = seen$List_Key[j], First_Day = today,
                                  Last_Day = today, Days_Seen = 1L, Title = seen$title[j], URL = seen$url[j],
                                  stringsAsFactors = FALSE)
    }
  }
  new_rows <- dplyr::bind_rows(new_rows)
  out <- dplyr::bind_rows(sightings, new_rows)
  out <- out[order(out$Board, out$Link_Key, out$First_Day), , drop = FALSE]
  rownames(out) <- NULL
  structure(out, extended = extended, started = nrow(new_rows))
}

read_presence_file <- function(path, empty) {
  if (!file.exists(path)) return(empty)
  x <- utils::read.csv(path, colClasses = "character", na.strings = c("", "NA"))
  for (col in names(empty)) {
    if (!col %in% names(x)) stop(path, ": missing column ", col)
    x[[col]] <- switch(class(empty[[col]])[1], Date = as.Date(x[[col]]), integer = as.integer(x[[col]]),
                       numeric = as.numeric(x[[col]]), x[[col]])
  }
  x[, names(empty), drop = FALSE]
}

PRESENCE_README <- c(
  "# Daily presence check",
  "",
  "Written by the daily presence workflow (bostickmike/edjobs, `.github/workflows/presence.yml`); never edit by hand.",
  "Each day, every board on a platform with stable posting IDs is read once (list pages only), with the same call",
  "the weekly run makes, and so is each statewide feed (MT's OPI, WY's WSBA), keyed by a fingerprint.",
  "",
  "- `presence_checks.csv`: one row per board per day. `Status`: ok, empty (answered with no postings) or error.",
  "  `N_Keyed`: postings with a key (`Link_Key`).",
  "- `presence_sightings.csv`: one row per unbroken stretch of sightings of a posting on a board. A stretch ends",
  "  when its board answered a check without it; days the board errored or wasn't checked don't break it.",
  "  `Link_Key`: `edjobs::k12_link_posting_id()` of the posting's link, or for a feed `edjobs::opi_posting_key()` /",
  "  `wsba_vacancy_key()` with an occurrence suffix. `District` is the posting's district; `Board` the board or feed.")

#' Run the daily presence check and update its files
#'
#' Reads `presence_checks.csv` and `presence_sightings.csv` in `dir` (absent
#' on the first day), checks every board and feed once, and writes both back,
#' plus a README the first time. A second run on the same day does nothing.
#' @param dir The folder holding the files (the `presence` branch's checkout).
#' @param platforms,feeds,sources,delay,envir As for [run_presence_check()].
#' @param today The day the check counts for.
#' @param summary_path Optional file to append a Markdown summary to
#'   (`$GITHUB_STEP_SUMMARY`).
#' @return Invisibly, the summary lines (NULL when today was already checked).
#' @export
refresh_presence <- function(dir, platforms, feeds = list(), today = presence_today(),
                             sources = state_scraper_sources(), delay = 1, envir = globalenv(), summary_path = NULL) {
  checks_path <- file.path(dir, "presence_checks.csv")
  sightings_path <- file.path(dir, "presence_sightings.csv")
  checks <- read_presence_file(checks_path, PRESENCE_CHECKS_EMPTY)
  sightings <- read_presence_file(sightings_path, PRESENCE_SIGHTINGS_EMPTY)
  if (any(checks$Date == today)) {
    message("Presence check: ", format(today), " already checked; nothing to do.")
    return(invisible(NULL))
  }
  run <- run_presence_check(platforms, feeds = feeds, today = today, sources = sources, sightings = sightings,
                            delay = delay, envir = envir)
  sightings <- update_presence_sightings(sightings, run$seen, checks, today)
  stats <- c(extended = attr(sightings, "extended"), started = attr(sightings, "started"))
  attr(sightings, "extended") <- NULL; attr(sightings, "started") <- NULL
  checks <- dplyr::bind_rows(checks, run$checks)
  checks <- checks[order(checks$Date, checks$Board), , drop = FALSE]
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(checks, checks_path, row.names = FALSE, na = "")
  utils::write.csv(sightings, sightings_path, row.names = FALSE, na = "")
  readme <- file.path(dir, "README.md")
  if (!file.exists(readme)) writeLines(PRESENCE_README, readme)

  lines <- presence_summary_markdown(run$checks, stats, today)
  message(paste(lines, collapse = "\n"))
  if (!is.null(summary_path) && nzchar(summary_path)) cat(lines, file = summary_path, sep = "\n", append = TRUE)
  invisible(lines)
}

#' Markdown summary of one day's check
#' @param day_checks That day's rows of `presence_checks.csv`.
#' @param stats Named counts `extended`, `started`.
#' @param today The day.
#' @export
presence_summary_markdown <- function(day_checks, stats, today) {
  n <- function(s) sum(day_checks$Status == s)
  postings <- sum(day_checks$N_Postings, na.rm = TRUE)
  keyed <- sum(day_checks$N_Keyed, na.rm = TRUE)
  lines <- c(sprintf("## Presence check %s", format(today)), "",
             sprintf("Boards: %d (%d ok, %d empty, %d error), %.0f s in all.", nrow(day_checks), n("ok"), n("empty"),
                     n("error"), sum(day_checks$Seconds, na.rm = TRUE)),
             sprintf("Postings: %d, %d with a key; stretches extended %d, started %d.", postings, keyed,
                     stats[["extended"]], stats[["started"]]))
  err <- day_checks[day_checks$Status == "error", , drop = FALSE]
  if (nrow(err)) lines <- c(lines, "", "Errors:", sprintf("- %s (%s): %s", err$Board, err$Platform, err$Error))
  unkeyed <- day_checks[day_checks$Status == "ok" & day_checks$N_Keyed < day_checks$N_Postings, , drop = FALSE]
  if (nrow(unkeyed)) lines <- c(lines, "", "Postings without a key:",
                                sprintf("- %s (%s): %d of %d", unkeyed$Board, unkeyed$Platform,
                                        unkeyed$N_Postings - unkeyed$N_Keyed, unkeyed$N_Postings))
  lines
}
