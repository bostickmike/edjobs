# Daily presence check (2026-11-13; plan: claude-notes/2026-11-09_daily-presence-check-plan.md,
# scope: 2026-11-12_twice-weekly-and-daily-presence-scope.md).
#
# Each day, the ATS boards that give every posting a stable ID (AppliTrack,
# SchoolSpring, TedK12, Tyler Portico; Red Rover after phase 0) are read once,
# list pages only, with the same fetch_* call the weekly run makes for that
# district (resolve_scraper_call()). Two files, kept on a side branch
# (`presence`), never main:
#   - presence_checks.csv: one row per board per day (Date, District,
#     Platform, Status ok/empty/error, N_Postings, N_Keyed, Seconds, Error).
#     A posting missing from a board is read as gone only on a day its board
#     answered.
#   - presence_sightings.csv: one row per unbroken stretch of sightings of a
#     posting (District, Platform, Link_Key, First_Day, Last_Day, Days_Seen,
#     Title, URL). A stretch ends when the board answered a check (ok or
#     empty) without the posting; a day the board errored, or the job didn't
#     run, doesn't break it.
# Link_Key is k12_link_posting_id() of the posting's link: WY's Posting_ID
# itself, and for MT (whose ID is the lowercased link) the same key computed
# from its panel's url. Phase 0 only writes these files; nothing reads them.

#' A per-posting ID from a K-12 posting's link
#'
#' Keyed by platform and number (plus the tenant where numbers are per tenant),
#' so a host or path change keeps the ID: AppliTrack `applitrack:<tenant>:<n>`,
#' SchoolSpring `schoolspring:<n>`, Red Rover `redrover:<org>:<n>`, TedK12
#' `tedk12:<subdomain>:<n>`, Tyler Portico `tylerportico:<tenant>:<id>` (a GUID or a number). A board
#' page or any other link is NA. Moved from WY's `k12_he_classification.R` in
#' 0.13.0 (Tyler Portico added).
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

PRESENCE_CHECKS_EMPTY <- data.frame(Date = as.Date(character(0)), District = character(0), Platform = character(0),
                                    Status = character(0), N_Postings = integer(0), N_Keyed = integer(0),
                                    Seconds = numeric(0), Error = character(0), stringsAsFactors = FALSE)
PRESENCE_SIGHTINGS_EMPTY <- data.frame(District = character(0), Platform = character(0), Link_Key = character(0),
                                       First_Day = as.Date(character(0)), Last_Day = as.Date(character(0)),
                                       Days_Seen = integer(0), Title = character(0), URL = character(0),
                                       stringsAsFactors = FALSE)

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

#' Read every board on the given platforms once
#'
#' @param platforms Registry platform names to check (as the state's registry
#'   spells them: "AppliTrack" in MT, "Applitrack" in WY).
#' @param today The day the check counts for.
#' @param sources The state's scraper sources (`state_config(scraper_sources = )`).
#' @param delay Seconds between boards.
#' @param envir Where the state's own `fetch_*` functions are defined.
#' @return list(checks = one row per board, seen = one row per keyed posting:
#'   District, Platform, Link_Key, title, url).
#' @export
run_presence_check <- function(platforms, today = presence_today(), sources = state_scraper_sources(),
                               delay = 1, envir = globalenv()) {
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
  boards <- boards[!duplicated(boards$District), , drop = FALSE]
  if (nrow(boards) == 0) stop("run_presence_check(): no registry rows on platforms ", paste(platforms, collapse = ", "))

  checks <- vector("list", nrow(boards))
  seen <- vector("list", nrow(boards))
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
      checks[[i]] <- data.frame(Date = today, District = d, Platform = boards$Platform[i], Status = "error",
                                N_Postings = NA_integer_, N_Keyed = NA_integer_, Seconds = secs,
                                Error = substr(gsub("[\r\n]+", " ", conditionMessage(rows)), 1, 300),
                                stringsAsFactors = FALSE)
      next
    }
    keyed <- rows[!is.na(rows$Link_Key), , drop = FALSE]
    checks[[i]] <- data.frame(Date = today, District = d, Platform = boards$Platform[i],
                              Status = if (nrow(rows) == 0) "empty" else "ok", N_Postings = nrow(rows),
                              N_Keyed = nrow(keyed), Seconds = secs, Error = NA_character_, stringsAsFactors = FALSE)
    if (nrow(keyed)) seen[[i]] <- data.frame(District = d, Platform = boards$Platform[i], Link_Key = keyed$Link_Key,
                                             title = keyed$title, url = keyed$url, stringsAsFactors = FALSE)
  }
  seen <- dplyr::bind_rows(seen)
  if (nrow(seen) == 0) seen <- data.frame(District = character(0), Platform = character(0), Link_Key = character(0),
                                          title = character(0), url = character(0), stringsAsFactors = FALSE)
  list(checks = dplyr::bind_rows(checks), seen = seen)
}

#' Add one day's sightings to the stretches
#'
#' A posting seen today extends its latest stretch when the stretch reached
#' the board's previous answered check (ok or empty); otherwise, including
#' when the board answered a check without it, a new stretch starts. Days the
#' board errored or wasn't checked are skipped over.
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
  prev <- if (nrow(answered)) tapply(answered$Date, answered$District, max) else numeric(0)
  key <- paste(sightings$District, sightings$Link_Key, sep = "\u001f")
  latest <- if (length(key)) tapply(seq_along(key), key, function(ix) ix[which.max(sightings$Last_Day[ix])]) else integer(0)
  extended <- 0L
  new_rows <- vector("list", nrow(seen))
  for (j in seq_len(nrow(seen))) {
    k <- paste(seen$District[j], seen$Link_Key[j], sep = "\u001f")
    i <- if (k %in% names(latest)) latest[[k]] else NA_integer_
    p <- if (seen$District[j] %in% names(prev)) as.Date(prev[[seen$District[j]]], origin = "1970-01-01") else as.Date(NA)
    if (!is.na(i) && sightings$Last_Day[i] < today && (is.na(p) || sightings$Last_Day[i] >= p)) {
      sightings$Last_Day[i] <- today
      sightings$Days_Seen[i] <- sightings$Days_Seen[i] + 1L
      sightings$Title[i] <- seen$title[j]
      sightings$URL[i] <- seen$url[j]
      extended <- extended + 1L
    } else if (is.na(i) || sightings$Last_Day[i] < today) {
      new_rows[[j]] <- data.frame(District = seen$District[j], Platform = seen$Platform[j], Link_Key = seen$Link_Key[j],
                                  First_Day = today, Last_Day = today, Days_Seen = 1L, Title = seen$title[j],
                                  URL = seen$url[j], stringsAsFactors = FALSE)
    }
  }
  new_rows <- dplyr::bind_rows(new_rows)
  out <- dplyr::bind_rows(sightings, new_rows)
  out <- out[order(out$District, out$Link_Key, out$First_Day), , drop = FALSE]
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
  "the weekly run makes.",
  "",
  "- `presence_checks.csv`: one row per board per day. `Status`: ok, empty (answered with no postings) or error.",
  "  `N_Keyed`: postings with a per-posting link (`Link_Key`).",
  "- `presence_sightings.csv`: one row per unbroken stretch of sightings of a posting. A stretch ends when its",
  "  board answered a check without it; days the board errored or wasn't checked don't break it.",
  "  `Link_Key` is `edjobs::k12_link_posting_id()` of the posting's link.")

#' Run the daily presence check and update its files
#'
#' Reads `presence_checks.csv` and `presence_sightings.csv` in `dir` (absent
#' on the first day), checks every board once, and writes both back, plus a
#' README the first time. A second run on the same day does nothing.
#' @param dir The folder holding the files (the `presence` branch's checkout).
#' @param platforms,sources,delay,envir As for [run_presence_check()].
#' @param today The day the check counts for.
#' @param summary_path Optional file to append a Markdown summary to
#'   (`$GITHUB_STEP_SUMMARY`).
#' @return Invisibly, the summary lines (NULL when today was already checked).
#' @export
refresh_presence <- function(dir, platforms, today = presence_today(), sources = state_scraper_sources(),
                             delay = 1, envir = globalenv(), summary_path = NULL) {
  checks_path <- file.path(dir, "presence_checks.csv")
  sightings_path <- file.path(dir, "presence_sightings.csv")
  checks <- read_presence_file(checks_path, PRESENCE_CHECKS_EMPTY)
  sightings <- read_presence_file(sightings_path, PRESENCE_SIGHTINGS_EMPTY)
  if (any(checks$Date == today)) {
    message("Presence check: ", format(today), " already checked; nothing to do.")
    return(invisible(NULL))
  }
  run <- run_presence_check(platforms, today = today, sources = sources, delay = delay, envir = envir)
  sightings <- update_presence_sightings(sightings, run$seen, checks, today)
  stats <- c(extended = attr(sightings, "extended"), started = attr(sightings, "started"))
  attr(sightings, "extended") <- NULL; attr(sightings, "started") <- NULL
  checks <- dplyr::bind_rows(checks, run$checks)
  checks <- checks[order(checks$Date, checks$District), , drop = FALSE]
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
             sprintf("Postings: %d, %d with a per-posting key; stretches extended %d, started %d.", postings, keyed,
                     stats[["extended"]], stats[["started"]]))
  err <- day_checks[day_checks$Status == "error", , drop = FALSE]
  if (nrow(err)) lines <- c(lines, "", "Errors:", sprintf("- %s (%s): %s", err$District, err$Platform, err$Error))
  unkeyed <- day_checks[day_checks$Status == "ok" & day_checks$N_Keyed < day_checks$N_Postings, , drop = FALSE]
  if (nrow(unkeyed)) lines <- c(lines, "", "Postings without a per-posting link:",
                                sprintf("- %s (%s): %d of %d", unkeyed$District, unkeyed$Platform,
                                        unkeyed$N_Postings - unkeyed$N_Keyed, unkeyed$N_Postings))
  lines
}
