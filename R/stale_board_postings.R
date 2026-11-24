# Stale board ads (2026-11-24). A board's platform keeps an ad listed, and takes applications for it, until the
# district deactivates it, whatever its dates: AppliTrack's "Closing Date" is free text the district types and is not
# enforced (a Poplar ad "closing 06/30/2024" still listed with an Apply button in November 2026). Some districts leave
# whole boards of old ads up and post their current openings elsewhere (the statewide feed). The measurement, on
# every archive since 2026-08 in two states and 15 posting pages read live, is in the workspace's
# claude-notes/2026-11-24_stale-ats-ads/README.md; the rule is its option C ("V1"), chosen 2026-11-24.
#
# The rule: a dated board posting whose posted date, or a stated closing date that parses as a date, is before the
# start of the previous school year (the July 1 that began it, with the defaults) is stale and not counted. Kept,
# whatever their dates: standing pools (STANDING_POOL_PATTERN) and ongoing roles a district keeps advertising
# (bus drivers and aides, paraprofessionals, custodians and maintenance, food service, athletic coaches). Rows are
# flagged in `Stale_Reason`, never deleted: the raw archive keeps them, and the counted files leave them out.

#' Ongoing roles: titles a stale-ad rule keeps whatever their dates
#'
#' Districts keep these ads up all year and fill them again and again, so an old posted date says little about
#' whether the job is open: bus drivers, bus aides and monitors, crossing guards, paraprofessionals and aides,
#' custodians, maintenance and cleaning staff, and food service (cooks, kitchen, nutrition, lunch, cafeteria).
#' Athletic coaches are `STALE_ATHLETIC_COACH_PATTERN`. Driver education is not a driver job.
#' @export
STALE_ONGOING_ROLE_PATTERN <- paste0(
  "(?i)\\bbus(es)?\\b|\\bdrivers?\\b(?!\\s*(ed\\b|education|training))|crossing guard|\\bmonitors?\\b|",
  "\\bpara(professional|educator|pro)?s?\\b|\\baides?\\b|",
  "custodi|janitor|maintenance|\\bcleaning\\b|\\blaborers?\\b|groundskeep|",
  "\\bcooks?\\b|food service|kitchen|nutrition|\\blunch|cafeteria")

#' Athletic coaches: titles a stale-ad rule keeps whatever their dates
#'
#' A coach title with a sport or athletics word, or a head or assistant coach, or a "coach or sponsor" ad. An
#' instructional, academic, literacy, math, reading, curriculum, technology, success or career coach is a staff
#' role, not a season's coaching job, and is not matched (a 2024 "Instructional Coach/Testing/Curriculum
#' Coordinator" ad is judged by its dates).
#' @export
STALE_ATHLETIC_COACH_PATTERN <- paste0(
  "(?i)^(?!.*\\b(instructional|academic|literacy|reading|math(ematics)?|curriculum|technology|tech|success|career|",
  "learning|data|graduation|life|behavior|teacher|testing)\\s+coach)",
  "(?=.*\\bcoach)",
  "(?=.*(\\b(head|assistant|asst|jv|junior varsity|varsity|c[- ]squad|athletic|athletics|sports?|football|",
  "basketball|volleyball|wrestling|soccer|swim(ming)?|golf|track|cross[- ]country|nordic|ski(ing)?|baseball|",
  "softball|tennis|cheer(leading)?|dance|hockey|rodeo|weightlifting|strength)\\b|coach(es)?\\s*(/|or)\\s*sponsor))")

#' The titles a stale-ad rule keeps, whatever their dates
#'
#' Named patterns (perl, any case): `pool` (`STANDING_POOL_PATTERN`), `ongoing` (`STALE_ONGOING_ROLE_PATTERN`) and
#' `coach` (`STALE_ATHLETIC_COACH_PATTERN`). A title matching any one is exempt.
#' @export
STALE_EXEMPT_PATTERNS <- c(pool = STANDING_POOL_PATTERN, ongoing = STALE_ONGOING_ROLE_PATTERN,
                           coach = STALE_ATHLETIC_COACH_PATTERN)

#' Why a board row is flagged stale (`Stale_Reason`)
#'
#' `STALE_POSTED_REASON`: its posted date is before the cutoff. `STALE_CLOSED_REASON`: its posted date isn't (or it
#' has none) but its stated closing date is. `STALE_FEED_LINK_REASON`: a statewide-feed row that is the same ad as a
#' flagged board row (it links that posting, or has its district, title and posted date), set by the state's own
#' feed matcher with [mark_stale_feed_rows()].
#' @export
STALE_POSTED_REASON <- "posted before the previous school year"

#' @rdname STALE_POSTED_REASON
#' @export
STALE_CLOSED_REASON <- "closing date before the previous school year"

#' @rdname STALE_POSTED_REASON
#' @export
STALE_FEED_LINK_REASON <- "same ad as a board posting from before the previous school year"

#' A stale-ad rule's settings
#'
#' The rule [flag_stale_board_postings()] applies, as data: a state passes one to [state_config()]
#' (`stale_board_rule`), or the package default is used.
#' @param school_year_start The day a school year starts, "MM-DD" ("07-01").
#' @param years_back How many school years before the run's own the cutoff is: 1 (the default) is the start of the
#'   previous school year, so on 2026-11-24 the cutoff is 2025-07-01; 0 would be the start of the current one.
#' @param exempt_patterns Named title patterns (perl) whose rows are never stale; `STALE_EXEMPT_PATTERNS`.
#' @return A list of class `edjobs_stale_rule`.
#' @export
stale_board_rule <- function(school_year_start = "07-01", years_back = 1L, exempt_patterns = STALE_EXEMPT_PATTERNS) {
  if (!(is.character(school_year_start) && length(school_year_start) == 1 && grepl("^\\d{2}-\\d{2}$", school_year_start) &&
        !is.na(as.Date(paste0("2001-", school_year_start), optional = TRUE)))) {
    stop("school_year_start must be one \"MM-DD\" day")
  }
  if (!(is.numeric(years_back) && length(years_back) == 1 && !is.na(years_back) && years_back >= 0 &&
        years_back == round(years_back))) {
    stop("years_back must be one whole number >= 0")
  }
  if (!is.character(exempt_patterns) || anyNA(exempt_patterns)) stop("exempt_patterns must be a character vector")
  for (p in exempt_patterns) {   # an invalid pattern fails here, not mid-run
    ok <- tryCatch(suppressWarnings({ grepl(p, "", perl = TRUE); TRUE }), error = function(e) FALSE)
    if (!ok) stop("exempt_patterns: not a valid perl pattern: ", p)
  }
  structure(list(school_year_start = school_year_start, years_back = as.integer(years_back),
                 exempt_patterns = exempt_patterns), class = "edjobs_stale_rule")
}

#' The cutoff a stale-ad rule uses on a run date
#'
#' The start of the school year `years_back` years before the one `run_date` falls in. With the defaults: on
#' 2026-11-24 (school year 2026-27) the cutoff is 2025-07-01, and on 2026-06-30 it is 2024-07-01.
#' @param run_date The run's date(s).
#' @param school_year_start,years_back As in [stale_board_rule()].
#' @return Dates, one per `run_date`.
#' @export
stale_cutoff_date <- function(run_date, school_year_start = "07-01", years_back = 1L) {
  run_date <- as.Date(run_date)
  y <- as.integer(format(run_date, "%Y"))
  this_start <- as.Date(ifelse(is.na(y), NA_character_, paste0(y, "-", school_year_start)))
  y <- ifelse(run_date < this_start, y - 1L, y)
  as.Date(ifelse(is.na(y), NA_character_, paste0(y - years_back, "-", school_year_start)))
}

#' Why a board posting is stale, or NA
#'
#' The rule (see [flag_stale_board_postings()]) for given titles and dates.
#' @param title Posting titles.
#' @param date_posted Posted dates (ISO, m/d/Y or "Nov 9, 2026"; anything else counts as none).
#' @param close_date Stated closing dates as the board writes them ("06/30/2024", "Until Filled"); text that isn't a
#'   date ([parse_stated_close_date()]) counts as none.
#' @param run_date The run's date (one, or one per row).
#' @param rule A [stale_board_rule()].
#' @return Character: `STALE_POSTED_REASON`, `STALE_CLOSED_REASON` or `NA`.
#' @export
stale_board_reason <- function(title, date_posted, close_date, run_date, rule = stale_board_rule()) {
  if (!inherits(rule, "edjobs_stale_rule")) stop("rule must come from stale_board_rule()")
  n <- length(title)
  if (n == 0) return(character(0))
  title <- as.character(title)
  cutoff <- rep_len(stale_cutoff_date(run_date, rule$school_year_start, rule$years_back), n)
  posted <- as.Date(rep_len(parse_stated_close_date(date_posted), n))
  close <- as.Date(rep_len(parse_stated_close_date(close_date), n))
  exempt <- rep(FALSE, n)
  for (p in rule$exempt_patterns) exempt <- exempt | grepl(p, title, perl = TRUE)
  old_post <- !is.na(posted) & !is.na(cutoff) & posted < cutoff
  old_close <- !is.na(close) & !is.na(cutoff) & close < cutoff
  out <- ifelse(old_post, STALE_POSTED_REASON, ifelse(old_close, STALE_CLOSED_REASON, NA_character_))
  out[exempt] <- NA_character_
  out
}

#' Flag stale board ads
#'
#' Sets `Stale_Reason` on each row of a run's board postings: [stale_board_reason()] under the state's rule
#' (`state_config(stale_board_rule = )`, else [stale_board_rule()]'s defaults), for every row `board` marks;
#' `NA` elsewhere. A reason the row already carries is kept. Works for any source that gives a posted or closing
#' date (AppliTrack, SchoolSpring, Tyler Portico, Red Rover, TedK12, dated district pages); rows with neither are
#' never stale. Statewide-feed rows are not board rows: pass `board = FALSE` for them (a feed row that is the same
#' ad as a flagged board row is marked by [mark_stale_feed_rows()]).
#'
#' Flag, don't delete: the raw archive keeps flagged rows, so the decision can be read and undone, and
#' [drop_stale_rows()] leaves them out of everything counted. Run it before any step that matches feed postings to
#' board postings, so a stale ad can't stand in for a current feed posting of the job.
#' @param df A run's rows.
#' @param run_date The run's date.
#' @param rule A [stale_board_rule()], or NULL for the state's (or the package default).
#' @param board Logical, one per row (or one): which rows are board postings.
#' @param title_col,posted_col,close_col Column names; a missing date column counts as no date.
#' @return `df` with `Stale_Reason`.
#' @export
flag_stale_board_postings <- function(df, run_date, rule = NULL, board = TRUE, title_col = "title",
                                      posted_col = "date_posted", close_col = "Close_Date") {
  if (is.null(rule)) rule <- current_state_config()$stale_board_rule
  if (is.null(rule)) rule <- stale_board_rule()
  n <- nrow(df)
  board <- rep_len(as.logical(board), n)
  col <- function(name) if (name %in% names(df)) df[[name]] else rep(NA_character_, n)
  reason <- if (n == 0) character(0) else stale_board_reason(col(title_col), col(posted_col), col(close_col), run_date, rule)
  reason[!(board %in% TRUE)] <- NA_character_
  prior <- if ("Stale_Reason" %in% names(df)) as.character(df$Stale_Reason) else rep(NA_character_, n)
  df$Stale_Reason <- as.character(ifelse(is_stale_flagged(prior), prior, reason))
  df
}

#' Mark feed rows that are the same ad as a stale board posting
#'
#' A statewide feed often relists a district's own ad, linking its posting. When that board posting is flagged
#' stale, the feed's copy is the same stale ad and must not come back through the feed. Which feed rows are the same
#' ad is the state's own matching (its feed's link and key rules); this sets their reason.
#' @param feed_df Feed rows.
#' @param same_as_stale Logical, one per feed row: TRUE where the row is the same ad as a flagged board posting.
#' @return `feed_df` with `Stale_Reason` (`STALE_FEED_LINK_REASON` where `same_as_stale`, else kept or `NA`).
#' @export
mark_stale_feed_rows <- function(feed_df, same_as_stale) {
  n <- nrow(feed_df)
  same_as_stale <- rep_len(as.logical(same_as_stale), n) %in% TRUE
  prior <- if ("Stale_Reason" %in% names(feed_df)) as.character(feed_df$Stale_Reason) else rep(NA_character_, n)
  feed_df$Stale_Reason <- as.character(ifelse(is_stale_flagged(prior), prior,
                                              ifelse(same_as_stale, STALE_FEED_LINK_REASON, NA_character_)))
  feed_df
}

#' Which rows are flagged stale
#'
#' @param x `Stale_Reason` values as written to, or read back from, an archive (`NA` and "" are not flagged).
#' @return Logical.
#' @export
is_stale_flagged <- function(x) {
  x <- as.character(x)
  !is.na(x) & nzchar(trimws(x)) & x != "NA"
}

#' Leave flagged stale rows out
#'
#' The rows everything counted is built from. A data frame without the column (an archive written before the rule)
#' comes back unchanged: the rule applies from the first run that wrote it on.
#' @param df Rows, with or without `Stale_Reason`.
#' @param col The reason column.
#' @return `df` without its flagged rows.
#' @export
drop_stale_rows <- function(df, col = "Stale_Reason") {
  if (!col %in% names(df)) return(df)
  df[!is_stale_flagged(df[[col]]), , drop = FALSE]
}
