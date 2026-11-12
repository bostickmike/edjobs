# Posting spells: one row per posting, from a state's raw archives, with when it
# started, when (and how) it stopped being listed, and the flags that keep it
# out of listing-duration estimates. Moved from the MT and WY repos in phase 4
# (2026-11-11): MT's posting-level design (2026-11-04) and WY's audited version
# of it (spells trust audit 2026-11-05).
#
# The split between a state and this package:
# - The state builds a *panel*: one row per posting per run, with columns
#   Run_Date (Date), Posting_ID, Posting_Identity_Method ("Stable ..." for a
#   per-posting ID, anything else for a fallback), District, title,
#   date_posted, url, Posting_Source, position, and optionally Close_Date,
#   Is_Feed (row from a statewide feed), Posted_Approx (date_posted is a
#   relative date like "2 weeks ago"), Bridgeable (this row's ID may change to
#   another posting's across runs) and Role_Hint. Reading the archives and
#   assigning identity is the state's own code: archives and identity rules
#   differ by state.
# - This package bridges IDs across runs, decides endings and starts, flags
#   what can't be measured, and summarizes durations, under a `spell_rules()`
#   object a state sets once (its state configuration).
#
# Starts: the board's own posted date when plausible (on or before the first
# sighting, at most 365 days before it); otherwise the first sighting, which is
# left-censored when it was the source's first run (or, with
# `left_censor_after_gap`, the run it came back after a run with no rows).
# Ends: "no longer listed" only when the source was healthy the next run
# (coverage calendar; before it starts, "the district returned rows from that
# source"). SchoolSpring endings come from the posting's own record
# (filled / canceled / review), and a posting's own page, checked after it
# dropped off (posting_status.csv), confirms or overrules the ending. An
# ending's date is the stated closing date when the posting closed on it,
# otherwise the midpoint between the last sighting and the next run.
#
# Durations are Kaplan-Meier medians with delayed entry (open and censored
# postings count as censored); a median is reported only with at least
# `min_events` endings. Read them as "days a posting stays listed", not
# time-to-fill: only SchoolSpring says filled.

#' Endings that count as events in the duration estimates
#'
#' Every other `Ending` value ("still listed", "censored: ...") is censored.
#' @export
POSTING_SPELL_EVENTS <- c("no longer listed", "closed (confirmed)", "filled", "canceled", "review")

#' Title patterns for standing ads and pools
#'
#' `EVERGREEN_TITLE_PATTERN`: substitute lists, "always accepting" and other
#' standing ads. `POOL_TITLE_PATTERN`: applicant pools, adjunct and part-time
#' faculty listings. Both are excluded from durations (case-insensitive, perl).
#' @export
EVERGREEN_TITLE_PATTERN <- paste0(
  "\\bsub(stitute)?s?\\b|substitute|on[- ]call|always accepting|accepting applications|",
  "ongoing|continuous|general application|interest (form|list)|^copy of\\b")

#' @rdname EVERGREEN_TITLE_PATTERN
#' @export
POOL_TITLE_PATTERN <- "\\bpool\\b|\\badjunct\\b|part[- ]time faculty|applicant (pool|list)|\\bstanding\\b"

#' Frozen-board and partial-run thresholds
#'
#' A board whose whole posting set stayed identical for `FROZEN_MIN_RUNS`+ runs
#' with `FROZEN_MIN_POSTINGS`+ postings is frozen (a stale page or cache:
#' Fremont #38, WY, 2024-10 to 2025-11). A source that returned under half its
#' previous run's rows, with at least `PARTIAL_MIN_PREVIOUS` before, had a
#' partial run (University of Wyoming: 208 rows on 2026-07-31, 25 on 08-04).
#' @export
FROZEN_MIN_RUNS <- 12

#' @rdname FROZEN_MIN_RUNS
#' @export
FROZEN_MIN_POSTINGS <- 5

#' @rdname FROZEN_MIN_RUNS
#' @export
PARTIAL_MIN_PREVIOUS <- 10

#' The rules a state's posting spells are built under
#'
#' One object per segment (K-12, HE), set once in the state's configuration and
#' passed to `bridge_posting_ids()` and `build_posting_spells()`. The defaults
#' are the rules the WY spells trust audit (2026-11-05) settled on.
#'
#' @param feed_entity The coverage calendar's `Entity` for rows with
#'   `Is_Feed` (a statewide feed: "OPI statewide feed", "WSBA statewide feed").
#' @param feed_in_durations Whether feed postings count in durations. FALSE
#'   when a feed's titles aren't postings (MT's OPI rows are subject areas).
#' @param shared_titles Which same-title postings are "shared" (can't be told
#'   apart, so left out of durations): `"indistinguishable"`, fallback-identified
#'   rows with the same district, board, title and posted date in one run;
#'   `"all"`, any title held by two postings in a district in a run;
#'   `"fallback only"`, the same but only for fallback-identified postings.
#' @param partial_min_previous A source's run with under half its previous
#'   run's rows (and at least this many before) is partial: endings that run are
#'   censored. `Inf` turns it off.
#' @param moved_boards Censor an ending when the same district and title turn
#'   up on another board, under another ID, the next run.
#' @param left_censor_after_gap A first sighting in the run a source came back
#'   after a run with no rows is left-censored too.
#' @param evergreen_pattern,pool_pattern Title patterns (perl, any case) for
#'   standing ads and pools; `NA` matches nothing.
#' @return A list of class `spell_rules`.
#' @export
spell_rules <- function(feed_entity = NA_character_, feed_in_durations = TRUE,
                        shared_titles = c("indistinguishable", "all", "fallback only"),
                        partial_min_previous = PARTIAL_MIN_PREVIOUS, moved_boards = TRUE,
                        left_censor_after_gap = TRUE, evergreen_pattern = EVERGREEN_TITLE_PATTERN,
                        pool_pattern = POOL_TITLE_PATTERN) {
  structure(list(feed_entity = feed_entity, feed_in_durations = isTRUE(feed_in_durations),
                 shared_titles = match.arg(shared_titles), partial_min_previous = partial_min_previous,
                 moved_boards = isTRUE(moved_boards), left_censor_after_gap = isTRUE(left_censor_after_gap),
                 evergreen_pattern = evergreen_pattern, pool_pattern = pool_pattern),
            class = "spell_rules")
}

# A panel without Posted_Approx has only exact posted dates.
posted_approx <- function(panel) if (is.null(panel$Posted_Approx)) rep(FALSE, nrow(panel)) else panel$Posted_Approx %in% TRUE

match_pattern <- function(pattern, x) {
  if (is.null(pattern) || is.na(pattern) || !nzchar(pattern)) return(rep(FALSE, length(x)))
  grepl(pattern, x, ignore.case = TRUE, perl = TRUE)
}

#' Title text and keys for matching postings across runs
#'
#' `spell_text()` repairs encoding (`fix_title_encoding()`), drops zero-width
#' characters (Aaniiih Nakoda's "Facilities Manager\\u200b" would split one
#' posting in two) and squishes spaces. `spell_key()` is its lower case, with
#' `NA` as "".
#' @param x Character vector.
#' @export
spell_text <- function(x) {
  x <- fix_title_encoding(as.character(x))
  x <- gsub("[\u200b-\u200d\ufeff]", "", x, perl = TRUE)
  trimws(gsub("\\s+", " ", x))
}

#' @rdname spell_text
#' @export
spell_key <- function(x) { x <- tolower(spell_text(x)); x[is.na(x)] <- ""; x }

#' Relisted the next run
#'
#' A spell whose posting dropped off, and a posting with the same title (case,
#' spacing and punctuation aside) at the same district and source appears in
#' the source's very next run. Usually the district re-advertising, but not
#' always: of 23 WY SchoolSpring cases whose old record has a final status
#' (2026-11-09), 8 were "filled", a new vacancy with the same title. So it is
#' neither linked to the new posting nor counted as an ending: its outcome is
#' unknown, unless the platform itself says it was filled.
#' @param district,source,title,first_seen,next_run Spell columns.
#' @export
relisted_next_run <- function(district, source, title, first_seen, next_run) {
  key <- paste(district, source, trimws(gsub("[^[:alnum:]]+", " ", tolower(title))), sep = "\u001f")
  starts <- paste(key, format(first_seen), sep = "\u001f")
  !is.na(next_run) & paste(key, format(next_run), sep = "\u001f") %in% starts
}

#' Posted dates as job boards print them
#'
#' 2026-10-14, 10/14/2026, "Oct 14, 2026 9:41 AM (UTC)", "14-Oct-26", and on
#' college boards "3 days ago(10/27/2026 9:12 AM)" or a bare "Posted 2 weeks
#' ago" / "Today". A bare relative date is dated back from the run
#' (approximate: "2 weeks ago" is 14-20 days); "more than 30 days ago" gives no
#' date, and a date after the run is a parse error.
#' @param x Posted-date text.
#' @param run_date The run each value was scraped in.
#' @return A data frame: `date` (character, ISO) and `approx` (logical).
#' @export
parse_board_posted_date <- function(x, run_date) {
  x <- trimws(gsub("\\s+", " ", as.character(x)))
  run_date <- rep_len(as.Date(run_date), length(x))
  out <- rep(as.Date(NA), length(x))
  try_fmt <- function(s, fmt) suppressWarnings(as.Date(s, format = fmt))
  inner <- ifelse(grepl("\\((\\d{1,2}/\\d{1,2}/\\d{4})", x), sub(".*\\((\\d{1,2}/\\d{1,2}/\\d{4}).*", "\\1", x), x)
  ymd <- grepl("^\\d{4}-\\d{2}-\\d{2}", inner)
  out[ymd] <- try_fmt(substr(inner[ymd], 1, 10), "%Y-%m-%d")
  mdy <- is.na(out) & grepl("^\\d{1,2}/\\d{1,2}/\\d{4}", inner)
  out[mdy] <- try_fmt(sub("^(\\d{1,2}/\\d{1,2}/\\d{4}).*", "\\1", inner[mdy]), "%m/%d/%Y")
  mon <- is.na(out) & grepl("^[A-Z][a-z]{2} \\d{1,2}, \\d{4}", inner)
  out[mon] <- try_fmt(sub("^([A-Z][a-z]{2} \\d{1,2}, \\d{4}).*", "\\1", inner[mon]), "%b %d, %Y")
  dmy <- is.na(out) & grepl("^\\d{1,2}-[A-Z][a-z]{2}-\\d{2}$", inner)
  out[dmy] <- try_fmt(inner[dmy], "%d-%b-%y")
  approx <- rep(FALSE, length(x))
  rel <- is.na(out) & !is.na(x) & grepl("^(posted )?(today|\\d+ (hour|day|week|month)s? ago)$", tolower(x))
  if (any(rel)) {
    s <- sub("^posted ", "", tolower(x[rel]))
    n <- suppressWarnings(as.numeric(sub("^(\\d+).*", "\\1", s)))
    n[s == "today"] <- 0
    unit <- ifelse(grepl("week", s), 7, ifelse(grepl("month", s), 30, ifelse(grepl("day", s), 1, 0)))
    out[rel] <- run_date[rel] - n * unit
    approx[rel] <- TRUE
  }
  bad <- !is.na(out) & !is.na(run_date) & out > run_date
  out[bad] <- NA
  approx[is.na(out)] <- FALSE
  data.frame(date = ifelse(is.na(out), NA_character_, format(out)), approx = approx, stringsAsFactors = FALSE)
}

#' A stated closing date
#'
#' 2026-11-09, 11/9/2026, "Nov 9, 2026"; "open until filled" and the like give
#' none.
#' @param x Closing-date text.
#' @return ISO date strings, `NA` where none.
#' @export
parse_stated_close_date <- function(x) {
  x <- trimws(as.character(x))
  d <- suppressWarnings(as.Date(substr(x, 1, 10), format = "%Y-%m-%d"))
  d[is.na(d)] <- suppressWarnings(as.Date(sub("^(\\d{1,2}/\\d{1,2}/\\d{4}).*$", "\\1", x[is.na(d)]), format = "%m/%d/%Y"))
  d[is.na(d)] <- suppressWarnings(as.Date(x[is.na(d)], format = "%b %d, %Y"))
  ifelse(is.na(d), NA_character_, format(d))
}

#' A posting's start is the earliest plausible posted date it ever showed
#'
#' Exact dates win over relative ones (`Posted_Approx`). PeopleAdmin's feed
#' date moves forward when a posting is re-published (one Great Falls pool by
#' two years). Apply after bridging, per posting.
#' @param panel A spells panel.
#' @export
use_earliest_posted_date <- function(panel) {
  posted <- suppressWarnings(as.Date(panel$date_posted))
  ok <- !is.na(posted) & posted <= panel$Run_Date
  approx <- posted_approx(panel)
  pick <- function(sel) stats::ave(ifelse(sel, as.numeric(posted), NA_real_), panel$Posting_ID,
                                   FUN = function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE))
  exact <- pick(ok & !approx)
  rough <- pick(ok & approx)
  earliest <- ifelse(is.na(exact), rough, exact)
  panel$Posted_Approx <- is.na(exact) & !is.na(rough)
  panel$date_posted <- ifelse(is.na(earliest), NA_character_, format(as.Date(earliest, origin = "1970-01-01")))
  panel
}

#' Bridge a posting's IDs across runs
#'
#' `bridge_posting_ids()`: earlier fallback-identified rows of a posting that
#' later carries a stable ID (same source, district, title and exact posted
#' date, the key unique on both sides, the two never listed in the same run)
#' take the stable ID; the old ID goes in `Bridged_From`. Then
#' `bridge_adjacent_runs()`: an ID change between two consecutive runs that the
#' key misses (no posted date before, a parser fix, a board moving to
#' per-posting links): a posting last seen in one run and one first seen in the
#' next, same source, district and title, unique on both sides, the old posted
#' date blank or equal to the new one. A new exact posted date is a real repost
#' and stays separate (Billings "Custodian III", 08-04 then 08-18). Only rows
#' with `Bridgeable` (or, without that column, fallback-identified rows) may
#' change ID: a new requisition number is a new posting.
#' @param panel A spells panel.
#' @param rules `spell_rules()` (reserved: bridging has no state-specific rules yet).
#' @export
bridge_posting_ids <- function(panel, rules = spell_rules()) {
  posted <- ifelse(posted_approx(panel), NA_character_, panel$date_posted)
  key <- paste(panel$Posting_Source, panel$District, spell_key(panel$title), posted, sep = "\u001f")
  stable <- grepl("^Stable", panel$Posting_Identity_Method)
  fallback <- !stable & !is.na(posted)
  ids_by_key <- function(sel) {
    x <- unique(data.frame(key = key[sel], id = panel$Posting_ID[sel], stringsAsFactors = FALSE))
    x[!x$key %in% x$key[duplicated(x$key)], , drop = FALSE]   # unique key -> one ID
  }
  s <- ids_by_key(stable); f <- ids_by_key(fallback)
  m <- merge(f, s, by = "key", suffixes = c("_old", "_new"))
  panel$Bridged_From <- NA_character_
  if (nrow(m) > 0) {
    runs_of <- split(panel$Run_Date, panel$Posting_ID)
    ok <- vapply(seq_len(nrow(m)), function(i) {
      length(intersect(runs_of[[m$id_old[i]]], runs_of[[m$id_new[i]]])) == 0 &&
        max(runs_of[[m$id_old[i]]]) < min(runs_of[[m$id_new[i]]])
    }, logical(1))
    m <- m[ok, , drop = FALSE]
    hit <- match(panel$Posting_ID, m$id_old)
    panel$Bridged_From[!is.na(hit)] <- panel$Posting_ID[!is.na(hit)]
    panel$Posting_ID[!is.na(hit)] <- m$id_new[hit[!is.na(hit)]]
  }
  bridge_adjacent_runs(panel, rules)
}

#' @rdname bridge_posting_ids
#' @export
bridge_adjacent_runs <- function(panel, rules = spell_rules()) {
  runs <- sort(unique(panel$Run_Date))
  if (is.null(panel$Bridged_From)) panel$Bridged_From <- NA_character_
  movable <- if (!is.null(panel$Bridgeable)) panel$Bridgeable %in% TRUE else !grepl("^Stable", panel$Posting_Identity_Method)
  movable_ids <- unique(panel$Posting_ID[movable])
  exact_posted <- ifelse(posted_approx(panel), NA_character_, panel$date_posted)
  span <- data.frame(Posting_ID = panel$Posting_ID, Run_Date = panel$Run_Date, Posting_Source = panel$Posting_Source,
                     District = panel$District, t = spell_key(panel$title), posted = exact_posted,
                     stringsAsFactors = FALSE)
  for (k in seq_len(length(runs) - 1)) {
    s <- span %>%
      dplyr::group_by(Posting_ID) %>%
      dplyr::summarize(first = min(Run_Date), last = max(Run_Date), Posting_Source = dplyr::last(Posting_Source),
                       District = dplyr::last(District), t = dplyr::last(t),
                       posted = dplyr::last(posted), .groups = "drop")
    old <- s[s$last == runs[k] & s$Posting_ID %in% movable_ids, ]
    new <- s[s$first == runs[k + 1], ]
    keyf <- function(x) paste(x$Posting_Source, x$District, x$t, sep = "\u001f")
    old <- old[!keyf(old) %in% keyf(old)[duplicated(keyf(old))], ]
    new <- new[!keyf(new) %in% keyf(new)[duplicated(keyf(new))], ]
    if (nrow(old) == 0 || nrow(new) == 0) next
    m <- merge(transform(old, key = keyf(old)), transform(new, key = keyf(new)), by = "key", suffixes = c("_old", "_new"))
    same_date <- !is.na(m$posted_old) & !is.na(m$posted_new) & m$posted_old == m$posted_new
    m <- m[is.na(m$posted_old) | !nzchar(m$posted_old) | is.na(m$posted_new) |
             same_date, , drop = FALSE]
    if (nrow(m) == 0) next
    hit <- match(panel$Posting_ID, m$Posting_ID_old)
    panel$Bridged_From[!is.na(hit)] <- panel$Posting_ID[!is.na(hit)]
    panel$Posting_ID[!is.na(hit)] <- m$Posting_ID_new[hit[!is.na(hit)]]
    span$Posting_ID <- panel$Posting_ID
    movable_ids <- unique(c(movable_ids, m$Posting_ID_new[m$Posting_ID_old %in% movable_ids]))
  }
  panel
}

#' Frozen boards
#'
#' A source whose whole posting set (titles, any case) was identical run after
#' run. Real boards change; a set unchanged for `min_runs`+ runs with
#' `min_postings`+ postings is a stale page or cache, and every posting on it
#' looks listed for as long as the freeze. A run with no rows from the board
#' breaks a stretch.
#' @param rows Any frame with Run_Date, District, Posting_Source (or none: one
#'   board per district) and title.
#' @param min_runs,min_postings Thresholds.
#' @return One row per frozen stretch: District, Posting_Source, From, To,
#'   Runs, Postings, Ongoing (still frozen in the latest run).
#' @export
frozen_board_runs <- function(rows, min_runs = FROZEN_MIN_RUNS, min_postings = FROZEN_MIN_POSTINGS) {
  empty <- data.frame(District = character(0), Posting_Source = character(0), From = character(0), To = character(0),
                      Runs = integer(0), Postings = integer(0), Ongoing = logical(0), stringsAsFactors = FALSE)
  if (is.null(rows) || nrow(rows) == 0) return(empty)
  if (is.null(rows$Posting_Source)) rows$Posting_Source <- ""
  all_runs <- sort(unique(as.Date(rows$Run_Date)))
  sets <- rows %>%
    dplyr::mutate(Run_Date = as.Date(Run_Date), t = spell_key(title)) %>%
    dplyr::group_by(District, Posting_Source, Run_Date) %>%
    dplyr::summarize(key = paste(sort(unique(t)), collapse = "\u001f"), n = dplyr::n_distinct(t), .groups = "drop") %>%
    dplyr::arrange(District, Posting_Source, Run_Date)
  sets$run_idx <- match(sets$Run_Date, all_runs)
  out <- sets %>%
    dplyr::group_by(District, Posting_Source) %>%
    dplyr::mutate(chg = c(TRUE, key[-1] != key[-dplyr::n()] | diff(run_idx) != 1), stretch = cumsum(chg)) %>%
    dplyr::group_by(District, Posting_Source, stretch) %>%
    dplyr::summarize(From = format(min(Run_Date)), To = format(max(Run_Date)), Runs = dplyr::n(),
                     Postings = dplyr::first(n), .groups = "drop") %>%
    dplyr::filter(Runs >= min_runs, Postings >= min_postings) %>%
    dplyr::mutate(Ongoing = To == format(max(all_runs))) %>%
    dplyr::select(District, Posting_Source, From, To, Runs, Postings, Ongoing) %>%
    dplyr::arrange(dplyr::desc(Runs))
  as.data.frame(out)
}

#' Drift-report section for boards still frozen this run
#'
#' @param frozen `frozen_board_runs()` rows with a `Segment` column.
#' @param date Report date.
#' @return Markdown lines; none when no board is still frozen.
#' @export
build_frozen_board_report_markdown <- function(frozen, date = Sys.Date()) {
  if (is.null(frozen) || nrow(frozen) == 0) return(character(0))
  frozen <- frozen[frozen$Ongoing, , drop = FALSE]
  if (nrow(frozen) == 0) return(character(0))
  board <- ifelse(is.na(frozen$Posting_Source) | !nzchar(frozen$Posting_Source) | frozen$Posting_Source == frozen$District,
                  "", paste0(" (", frozen$Posting_Source, ")"))
  c(
    paste0("### Boards that haven't changed in weeks (", date, ")"),
    "",
    paste("Each board below has listed exactly the same postings every week for the number of weeks shown.",
          "Real boards change; this is usually a stale page, a cache, or a scraper reading an old copy, and",
          "its postings keep counting as live in the weekly totals. Open the board and compare."),
    "",
    sprintf("- **%s**%s (%s): the same %d posting(s) for %d weeks, since %s",
            frozen$District, board, frozen$Segment, frozen$Postings, frozen$Runs, frozen$From),
    ""
  )
}

#' Health of a posting's source in a run
#'
#' The coverage calendar where it covers the run (`Entity` = the district, or
#' `feed_entity` for feed rows); before the calendar starts, whether the
#' district returned any rows from that source ("ok_proxy" / "no_rows").
#' @param panel A spells panel.
#' @param calendar coverage_calendar.csv rows, or NULL.
#' @param district,source,is_feed,run One spell's district, source, feed flag
#'   and next run.
#' @param feed_entity The calendar's entity for feed rows.
#' @export
source_health <- function(panel, calendar, district, source, is_feed, run, feed_entity = NA_character_) {
  if (is.na(run)) return(NA_character_)
  run_chr <- format(run)
  if (!is.null(calendar) && nrow(calendar) > 0 && run_chr >= min(calendar$Run_Date)) {
    entity <- if (isTRUE(is_feed)) feed_entity else district
    st <- calendar$Status[calendar$Run_Date == run_chr & calendar$Entity == entity]
    if (length(st) == 0) return(if (any(calendar$Run_Date == run_chr)) "not_logged" else "no_calendar")
    return(st[1])
  }
  if (any(panel$Run_Date == run & panel$District == district & panel$Posting_Source == source)) "ok_proxy" else "no_rows"
}

#' Columns of a posting-spells table, in order
#' @export
POSTING_SPELL_COLUMNS <- c(
  "Posting_ID", "District", "Title", "Position", "Role_Hint", "Posting_Source", "Identity", "Is_Feed", "URL",
  "Posted_Date", "First_Seen", "Last_Seen", "Runs_Seen", "Runs_Spanned", "Next_Run", "Next_Run_Source_Status",
  "Moved_Boards", "Page_Status", "Ending", "Event", "Start_Basis", "Start_Date", "School_Year", "Left_Censored",
  "Days_Before_First_Seen", "Stated_Close_Date", "End_Basis", "End_Date", "Days_Listed", "Advertised_Days",
  "Evergreen", "Pool", "Frozen_Runs", "Frozen_Board", "Shared_Title", "In_Durations")

#' Build posting spells from a panel
#'
#' One row per posting (`POSTING_SPELL_COLUMNS`): its start and how it was
#' dated, its end and how it ended, and the flags that keep it out of
#' durations (`In_Durations` is FALSE for standing ads, pools, frozen boards,
#' shared titles, left-censored starts, and feed rows when the rules say so).
#' @param panel A spells panel (see the file header), after
#'   `bridge_posting_ids()`.
#' @param calendar coverage_calendar.csv rows for this segment, or NULL.
#' @param endings schoolspring_endings.csv (job_id, Job_Status), or NULL.
#' @param frozen `frozen_board_runs()` of the same panel, or NULL.
#' @param statuses posting_status.csv: what each posting's own page said after
#'   it dropped off its board, or NULL.
#' @param rules `spell_rules()`.
#' @export
build_posting_spells <- function(panel, calendar = NULL, endings = NULL, frozen = NULL, statuses = NULL,
                                 rules = spell_rules()) {
  if (is.null(panel$Close_Date)) panel$Close_Date <- NA_character_
  if (is.null(panel$Is_Feed)) panel$Is_Feed <- FALSE
  if (is.null(panel$Bridged_From)) panel$Bridged_From <- NA_character_
  if (is.null(panel$Posted_Approx)) panel$Posted_Approx <- FALSE
  if (is.null(panel$Role_Hint)) panel$Role_Hint <- NA_character_
  runs <- sort(unique(panel$Run_Date))
  latest <- max(runs)
  stable_row <- grepl("^Stable", panel$Posting_Identity_Method)
  dup_ids <- if (rules$shared_titles == "indistinguishable") {
    # Title-identified rows with the same district, board, title and posted
    # date in one run (the fallback ID's occurrence suffix #2, #3, ...).
    fb_base <- ifelse(stable_row, NA_character_, sub("#\\d+$", "", dplyr::coalesce(panel$Bridged_From, panel$Posting_ID)))
    dup_base <- unique(fb_base[!is.na(fb_base) & duplicated(paste(fb_base, panel$Run_Date))])
    unique(panel$Posting_ID[!is.na(fb_base) & fb_base %in% dup_base])
  } else {
    # Titles held by more than one posting in a district in any run.
    tl <- tolower(panel$title)
    n_ids <- stats::ave(panel$Posting_ID, panel$Run_Date, panel$District, tl, FUN = function(x) length(unique(x)))
    unique(panel$Posting_ID[as.integer(n_ids) > 1])
  }
  source_runs <- panel %>% dplyr::count(District, Posting_Source, Run_Date, name = "Rows")
  run_idx <- match(source_runs$Run_Date, runs)
  prev_rows <- source_runs$Rows[match(paste(source_runs$District, source_runs$Posting_Source, run_idx - 1L),
                                      paste(source_runs$District, source_runs$Posting_Source, run_idx))]
  partial <- source_runs[!is.na(prev_rows) & prev_rows >= rules$partial_min_previous & source_runs$Rows < 0.5 * prev_rows, ]
  first_source_run <- source_runs %>%
    dplyr::group_by(District, Posting_Source) %>%
    dplyr::summarize(Source_First_Run = min(Run_Date), .groups = "drop")

  sp <- panel %>%
    dplyr::arrange(Run_Date) %>%
    dplyr::group_by(Posting_ID) %>%
    dplyr::summarize(District = dplyr::last(District), Title = dplyr::last(title),
                     Position = dplyr::last(position), Role_Hint = dplyr::last(Role_Hint),
                     Posting_Source = dplyr::last(Posting_Source),
                     Identity = dplyr::last(Posting_Identity_Method), Is_Feed = any(Is_Feed %in% TRUE),
                     Posted_Date = dplyr::first(date_posted[!is.na(date_posted) & nzchar(date_posted)], default = NA_character_),
                     Posted_Approx = dplyr::first((Posted_Approx %in% TRUE)[!is.na(date_posted) & nzchar(date_posted)], default = FALSE),
                     URL = dplyr::last(url), First_Seen = min(Run_Date), Last_Seen = max(Run_Date),
                     Stated_Close_Date = dplyr::last(Close_Date[!is.na(Close_Date)], default = NA_character_),
                     Runs_Seen = dplyr::n_distinct(Run_Date), .groups = "drop") %>%
    dplyr::left_join(first_source_run, by = c("District", "Posting_Source"))
  sp$Shared_Title <- sp$Posting_ID %in% dup_ids
  if (rules$shared_titles == "fallback only") sp$Shared_Title <- sp$Shared_Title & !grepl("^Stable", sp$Identity)
  sp$Runs_Spanned <- match(sp$Last_Seen, runs) - match(sp$First_Seen, runs) + 1L
  sp$Next_Run <- runs[match(sp$Last_Seen, runs) + 1L]
  sp$Next_Run_Source_Status <- vapply(seq_len(nrow(sp)), function(i)
    source_health(panel, calendar, sp$District[i], sp$Posting_Source[i], sp$Is_Feed[i], sp$Next_Run[i],
                  rules$feed_entity), character(1))
  is_partial <- paste(sp$District, sp$Posting_Source, sp$Next_Run) %in%
    paste(partial$District, partial$Posting_Source, partial$Run_Date)
  sp$Next_Run_Source_Status[is_partial & sp$Next_Run_Source_Status %in% c("ok", "ok_proxy")] <- "partial"

  # Moved boards: the same district + title listed from another board the next
  # run, under another ID (a feed posting the district's own scrape picks up, a
  # district changing platforms).
  sp$Moved_Boards <- FALSE
  if (rules$moved_boards) {
    nxt <- panel %>% dplyr::transmute(Run_Date, District, t = spell_key(title), Posting_Source_Next = Posting_Source,
                                      Posting_ID_Next = Posting_ID)
    sp_key <- data.frame(i = seq_len(nrow(sp)), Run_Date = sp$Next_Run, District = sp$District, t = spell_key(sp$Title),
                         Posting_Source = sp$Posting_Source, Posting_ID = sp$Posting_ID, stringsAsFactors = FALSE)
    mv <- merge(sp_key, nxt, by = c("Run_Date", "District", "t"))
    mv <- mv[mv$Posting_Source_Next != mv$Posting_Source & mv$Posting_ID_Next != mv$Posting_ID, , drop = FALSE]
    sp$Moved_Boards <- seq_len(nrow(sp)) %in% mv$i
  }

  ss_status <- NA_character_
  if (!is.null(endings) && nrow(endings) > 0) {
    id <- dplyr::case_when(grepl("^schoolspring:\\d+$", sp$Posting_ID) ~ sub("^schoolspring:", "", sp$Posting_ID),
                           grepl("schoolspring\\.com/jobs/\\d+", sp$URL) ~ sub("^.*/jobs/(\\d+).*$", "\\1", sp$URL),
                           TRUE ~ NA_character_)
    ss_status <- endings$Job_Status[match(id, endings$job_id)]
  }
  # The posting's own page, checked after it dropped off: closed is a
  # confirmed ending wherever the board's absence alone wasn't; still open
  # means the scrape missed it, so the end is censored. Only a check made after
  # the posting's last sighting speaks to this spell.
  page <- data.frame(Status = rep(NA_character_, nrow(sp)), Close = NA_character_, Checked = NA_character_,
                     stringsAsFactors = FALSE)
  if (!is.null(statuses) && nrow(statuses) > 0) {
    hit <- match(posting_status_check_url(sp$URL), statuses$Check_URL)
    page <- data.frame(Status = statuses$Status[hit], Close = statuses$Stated_Close_Date[hit],
                       Checked = statuses$Last_Checked[hit], stringsAsFactors = FALSE)
    page$Status[is.na(page$Checked) | page$Checked <= format(sp$Last_Seen)] <- NA_character_
  }
  sp$Page_Status <- page$Status
  sp$Stated_Close_Date <- dplyr::coalesce(sp$Stated_Close_Date, page$Close)
  sp$Ending <- dplyr::case_when(
    sp$Last_Seen == latest ~ "still listed",
    !is.na(ss_status) ~ ss_status,
    sp$Page_Status %in% "closed" ~ "closed (confirmed)",
    sp$Page_Status %in% "open" ~ "censored: still open on its page",
    sp$Moved_Boards ~ "censored: moved boards",
    sp$Next_Run_Source_Status %in% c("ok", "ok_proxy") ~ "no longer listed",
    TRUE ~ paste0("censored: source ", sp$Next_Run_Source_Status))
  relisted <- relisted_next_run(sp$District, sp$Posting_Source, sp$Title, sp$First_Seen, sp$Next_Run)
  sp$Ending[relisted & sp$Ending %in% setdiff(POSTING_SPELL_EVENTS, "filled")] <- "censored: relisted next run"
  sp$Event <- sp$Ending %in% POSTING_SPELL_EVENTS

  posted <- suppressWarnings(as.Date(substr(sp$Posted_Date, 1, 10)))
  lag <- as.numeric(sp$First_Seen - posted)
  sp$Start_Basis <- ifelse(!is.na(lag) & lag >= 0 & lag <= 365,
                           ifelse(sp$Posted_Approx, "board posted date (approximate)", "board posted date"),
                           "first sighting")
  sp$Start_Date <- as.Date(ifelse(grepl("^board", sp$Start_Basis), posted, sp$First_Seen), origin = "1970-01-01")
  # Left-censored: the first sighting is the start, and it was the source's
  # first run (or the run it came back after a run with no rows).
  sp$Left_Censored <- sp$Start_Basis == "first sighting" & sp$First_Seen == sp$Source_First_Run
  if (rules$left_censor_after_gap) {
    prev_idx <- match(sp$First_Seen, runs) - 1L
    prev_run <- runs[ifelse(prev_idx >= 1L, prev_idx, NA_integer_)]
    had_rows_prev <- paste(sp$District, sp$Posting_Source, prev_run) %in%
      paste(source_runs$District, source_runs$Posting_Source, source_runs$Run_Date)
    sp$Left_Censored <- sp$Left_Censored | (sp$Start_Basis == "first sighting" & !is.na(prev_run) & !had_rows_prev)
  }
  sp$Evergreen <- match_pattern(rules$evergreen_pattern, sp$Title) | (!is.na(lag) & lag > 365)
  sp$Pool <- match_pattern(rules$pool_pattern, sp$Title)

  # Frozen board: the posting spent FROZEN_MIN_RUNS+ runs inside one of its
  # board's frozen stretches.
  sp$Frozen_Runs <- 0L
  if (!is.null(frozen) && nrow(frozen) > 0) {
    for (j in seq_len(nrow(frozen))) {
      f <- frozen[j, ]
      hit <- which(sp$District == f$District & sp$Posting_Source == f$Posting_Source)
      if (length(hit) == 0) next
      a <- pmax(match(sp$First_Seen[hit], runs), match(as.Date(f$From), runs))
      b <- pmin(match(sp$Last_Seen[hit], runs), match(as.Date(f$To), runs))
      sp$Frozen_Runs[hit] <- pmax(sp$Frozen_Runs[hit], pmax(0L, b - a + 1L))
    }
  }
  sp$Frozen_Board <- sp$Frozen_Runs >= FROZEN_MIN_RUNS

  close <- suppressWarnings(as.Date(sp$Stated_Close_Date))
  on_close <- sp$Event & !is.na(close) & !is.na(sp$Next_Run) & close >= sp$Last_Seen & close <= sp$Next_Run
  sp$End_Basis <- ifelse(on_close, "stated closing date", ifelse(sp$Event, "midpoint to next run", "censored"))
  sp$End_Date <- as.Date(ifelse(on_close, close,
                                ifelse(sp$Event, sp$Last_Seen + as.numeric(sp$Next_Run - sp$Last_Seen) / 2, sp$Last_Seen)),
                         origin = "1970-01-01")
  sp$Days_Listed <- as.numeric(sp$End_Date - sp$Start_Date)
  # Advertised application window: start to the stated closing date, from the
  # posting itself; needs no ending. A date a year or more out is a board
  # default, and a date the posting outlived by more than a week was not its
  # deadline (Sheridan's "Initial Review Date", a stale 2023 field).
  sp$Advertised_Days <- as.numeric(close - sp$Start_Date)
  sp$Advertised_Days[is.na(sp$Advertised_Days) | sp$Advertised_Days < 0 | sp$Advertised_Days > 365] <- NA
  sp$Advertised_Days[!is.na(close) & sp$Last_Seen > close + 7] <- NA
  # Age when first seen (delayed entry): a posting up since April that we first
  # saw in August is only at risk of "ending under our eyes" from then on.
  sp$Days_Before_First_Seen <- as.numeric(sp$First_Seen - sp$Start_Date)
  sp$School_Year <- ifelse(as.integer(format(sp$Start_Date, "%m")) >= 7,
                           paste0(format(sp$Start_Date, "%Y"), "-", as.integer(format(sp$Start_Date, "%y")) + 1L),
                           paste0(as.integer(format(sp$Start_Date, "%Y")) - 1L, "-", format(sp$Start_Date, "%y")))
  sp$In_Durations <- !sp$Evergreen & !sp$Pool & !sp$Frozen_Board & !sp$Shared_Title & !sp$Left_Censored &
    (rules$feed_in_durations | !sp$Is_Feed)
  sp <- sp %>%
    dplyr::mutate(dplyr::across(c(First_Seen, Last_Seen, Next_Run, Source_First_Run, Start_Date, End_Date), format)) %>%
    dplyr::arrange(District, First_Seen, Title)
  as.data.frame(sp[, POSTING_SPELL_COLUMNS])
}

#' Kaplan-Meier median days listed per group
#'
#' Over `In_Durations` spells, with delayed entry (each posting enters at its
#' age when first seen). A posting seen once and still listed adds half a day
#' of exposure so it stays valid.
#' @param spells `build_posting_spells()` output.
#' @param group A column to group by, or NULL for all.
#' @param min_events Fewest endings for a median.
#' @export
summarize_posting_durations <- function(spells, group = NULL, min_events = 20) {
  s <- spells[spells$In_Durations & !is.na(spells$Days_Listed) & spells$Days_Listed >= 0, , drop = FALSE]
  groups <- if (is.null(group)) list(All = s) else split(s, s[[group]])
  dplyr::bind_rows(lapply(names(groups), function(g) {
    x <- groups[[g]]
    if (nrow(x) == 0) return(NULL)
    entry <- pmax(0, x$Days_Before_First_Seen)
    exit <- pmax(x$Days_Listed, entry + 0.5)
    fit <- survival::survfit(survival::Surv(entry, exit, x$Event) ~ 1)
    tab <- summary(fit)$table
    enough <- sum(x$Event) >= min_events
    data.frame(Group = g, Postings = nrow(x), Endings = sum(x$Event),
               Median_Days = if (enough) unname(tab["median"]) else NA_real_,
               Lower_95 = if (enough) unname(tab["0.95LCL"]) else NA_real_,
               Upper_95 = if (enough) unname(tab["0.95UCL"]) else NA_real_,
               Note = if (enough) "" else paste("fewer than", min_events, "endings"), stringsAsFactors = FALSE)
  }))
}

#' First seen, for the dashboard's "Posted / listed" column
#'
#' Many boards give no posted date (MT: 546 of 1,380 K-12 postings on
#' 2026-11-03, mostly Apptegy district sites). For those the app shows the
#' first weekly run that found the posting. The app reads only its own folder
#' and the posting IDs differ between files, so this writes a small lookup
#' keyed on what the app shows: district/institution, title, link.
#' `Left_Censored` = the posting was already up the first time we scraped that
#' board ("On the board since at least"). The app has its own copy of
#' `first_seen_key()`; a state test checks the two agree.
#' @param district,title,url Columns the app shows.
#' @param panel,spells A segment's panel and its spells.
#' @param segment "K-12" or "HE".
#' @export
first_seen_key <- function(district, title, url) {
  sq <- function(x) tolower(gsub("\\s+", " ", trimws(as.character(x))))
  paste(sq(district), sq(title), trimws(as.character(url)), sep = " | ")
}

#' @rdname first_seen_key
#' @export
first_seen_lookup <- function(panel, spells, segment) {
  latest <- max(panel$Run_Date)
  cur <- panel[panel$Run_Date == latest, c("Posting_ID", "District", "title", "url"), drop = FALSE]
  cur <- merge(cur, spells[, c("Posting_ID", "First_Seen", "Left_Censored")], by = "Posting_ID")
  cur$Key <- first_seen_key(cur$District, cur$title, cur$url)
  cur <- cur[order(cur$Key, as.character(cur$First_Seen)), , drop = FALSE]
  cur <- cur[!duplicated(cur$Key), , drop = FALSE]
  data.frame(Segment = rep(segment, nrow(cur)), Key = cur$Key, First_Seen = as.character(cur$First_Seen),
             Left_Censored = as.logical(cur$Left_Censored), stringsAsFactors = FALSE)
}
