# Dashboard app helpers (package phase 6, slice 1, 2026-11-16): plain functions both states' app.R defined
# identically (deparsed; measured in Jobs_Dash_repos claude-notes/2026-11-16_phase6/). They read no app globals and
# build no shiny/bslib UI; their code is as it was in app.R. One change: fill_first_seen() takes its lookup table as
# an argument (app.R's default was its own `first_seen_table` global). first_seen_key() was already here
# (posting_spells.R); app.R's copy is retired with this slice.

#' Classify a college posting's appointment type
#'
#' "Adjunct/part-time faculty", "Faculty/instructor (non-adjunct)" or "Other / not faculty", from the title alone.
#' The pipeline's own classifier ([classify_he_job_type()]) uses the same title patterns; this one keeps the
#' dashboard from inferring FTE where a source does not state it.
#' @param titles Posting titles.
#' @export
classify_he_appointment <- function(titles) {
  titles <- as.character(titles)
  dplyr::case_when(
    grepl("Adjunct|Part[- ]?Time", titles, ignore.case = TRUE) ~ "Adjunct/part-time faculty",
    grepl("Instructor|Instructional|Teacher|Faculty|Professor|Lecturer|Post Doc|Subject Matter Expert|Librarian|Educator",
          titles, ignore.case = TRUE) ~ "Faculty/instructor (non-adjunct)",
    TRUE ~ "Other / not faculty"
  )
}

#' Count faculty and adjunct college postings
#'
#' @param titles Posting titles.
#' @return A list: `faculty_instructor` and `adjunct_part_time` counts ([classify_he_appointment()]).
#' @export
summarize_he_faculty_postings <- function(titles) {
  appointment <- classify_he_appointment(titles)
  list(
    faculty_instructor = sum(appointment == "Faculty/instructor (non-adjunct)", na.rm = TRUE),
    adjunct_part_time = sum(appointment == "Adjunct/part-time faculty", na.rm = TRUE)
  )
}

#' Posted date, or "Listed since" for an old one
#'
#' Sources that keep evergreen pools active may retain an original posting date for years. Makes that distinction
#' visible without discarding a still-open opportunity or inventing a newer date the source does not provide.
#' @param dates Posted dates as the source gives them.
#' @param stale_after_days A date older than this many days reads "Listed since <date>".
#' @export
format_posted_or_listed_date <- function(dates, stale_after_days = 180) {
  values <- as.character(dates)
  parsed <- as.Date(lubridate::parse_date_time(
    values,
    orders = c("ymd", "mdy", "dmy", "b d Y h:M p", "B d Y h:M p", "Y-m-d H:M:S"),
    tz = "UTC",
    quiet = TRUE
  ))
  stale <- !is.na(parsed) & parsed < Sys.Date() - stale_after_days
  values[stale] <- paste("Listed since", values[stale])
  values
}

#' Fill a blank posted date from the first run that found the posting
#'
#' "Posted / listed" for a board that gives no posted date: the first weekly run that found the posting
#' (posting_first_seen.csv, from [first_seen_lookup()], keyed by [first_seen_key()]). "On the board since at least"
#' when the posting was already up the first time that board was scraped. A posted date is kept as it is.
#' @param posted Posted dates (blank or NA where the board gives none).
#' @param district,title,url The posting's district (or college), title and URL.
#' @param segment "K-12" or "HE" (the lookup's `Segment`).
#' @param lookup The posting_first_seen.csv table (`Segment`, `Key`, `First_Seen`, `Left_Censored`); NULL (no file
#'   yet) or a table without those columns returns `posted` unchanged.
#' @export
fill_first_seen <- function(posted, district, title, url, segment, lookup) {
  posted <- as.character(posted)
  if (is.null(lookup) || !all(c("Segment", "Key", "First_Seen", "Left_Censored") %in% names(lookup))) return(posted)
  lookup <- lookup[lookup$Segment == segment, , drop = FALSE]
  k <- match(first_seen_key(district, title, url), lookup$Key)
  blank <- is.na(posted) | !nzchar(trimws(posted))
  seen <- lookup$First_Seen[k]
  label <- ifelse(lookup$Left_Censored[k] %in% "TRUE", paste("On the board since at least", seen), paste("First seen", seen))
  ifelse(blank & !is.na(k) & !is.na(seen) & nzchar(seen), label, posted)
}

#' SAIPE child poverty range as text
#'
#' Census's approximate 90% range for a SAIPE child poverty estimate (Child_Poverty_Low / _High,
#' [add_saipe_range()]): "about 0%-32%" (an en dash). NA where either end is missing.
#' @param low,high Range ends as shares (0-1).
#' @export
format_poverty_range <- function(low, high) {
  ifelse(is.na(low) | is.na(high), NA_character_,
         paste0("about ", scales::percent(low, accuracy = 1), "\u2013", scales::percent(high, accuracy = 1)))
}

# ACS county figures' 90% margins of error (Mike's decision 2026-11-16, option B): "+/-$2,190" / "+/-0.4 pts";
# a share whose MOE is at least the share itself is not distinguishable from zero (Census's CV > 0.61 line).

#' ACS margin of error in dollars
#'
#' " +/-$2,190"; "" where the MOE is missing.
#' @param moe Margins of error in dollars.
#' @export
format_moe_dollars <- function(moe) ifelse(is.na(moe), "", paste0(" \u00b1", scales::dollar(moe)))

#' ACS margin of error for a share, in percentage points
#'
#' " +/-0.4 pts"; "" where the MOE is missing.
#' @param moe Margins of error as shares (0-1).
#' @export
format_moe_points <- function(moe) ifelse(is.na(moe), "", paste0(" \u00b1", formatC(100 * moe, format = "f", digits = 1), " pts"))

#' Estimate not distinguishable from zero
#'
#' TRUE where the margin of error is at least the estimate (Census's CV > 0.61 line).
#' @param est Estimates.
#' @param moe Their margins of error.
#' @export
indistinct_from_zero <- function(est, moe) !is.na(est) & !is.na(moe) & moe >= est

#' One line of a county's ACS margins of error
#'
#' "income +/-$2,190; rent +/-$85; mining +/-0.4 pts", leaving out missing parts; NA when all are missing.
#' @param income_moe,rent_moe Margins of error for median household income and median gross rent (dollars).
#' @param share,share_moe The mining employment share and its margin of error.
#' @export
county_moe_summary <- function(income_moe, rent_moe, share, share_moe) {
  parts <- cbind(ifelse(is.na(income_moe), NA, paste0("income", format_moe_dollars(income_moe))),
                 ifelse(is.na(rent_moe), NA, paste0("rent", format_moe_dollars(rent_moe))),
                 ifelse(is.na(share_moe), NA, paste0("mining", format_moe_points(share_moe),
                                                     ifelse(indistinct_from_zero(share, share_moe), " (not distinguishable from 0)", ""))))
  out <- apply(parts, 1, function(x) paste(x[!is.na(x)], collapse = "; "))
  ifelse(nzchar(out), out, NA_character_)
}

#' Posted pay as text
#'
#' Pay as the posting itself states it (Pay_* from [extract_posted_pay()]): "$21.46-$22.81/hr",
#' "$46,500-$71,803/yr", "$3,691 stipend" (en dashes). Units are never converted: an hourly wage isn't turned into
#' an annual figure, since hours per year aren't known. An em dash where there is no pay.
#' @param pay_min,pay_max Range ends (equal or NA max for a single figure).
#' @param pay_unit "hour", "day", "month", "year", "credit hour" or "stipend".
#' @export
format_posted_pay <- function(pay_min, pay_max, pay_unit) {
  fmt <- function(x) ifelse(x >= 1000, scales::dollar(x, accuracy = 1), scales::dollar(x, accuracy = 0.01))
  amount <- ifelse(is.na(pay_min), NA_character_,
                   ifelse(is.na(pay_max) | pay_min == pay_max, fmt(pay_min), paste0(fmt(pay_min), "\u2013", fmt(pay_max))))
  suffix <- unname(c(hour = "/hr", day = "/day", month = "/mo", year = "/yr",
                     "credit hour" = "/credit hr", stipend = " stipend")[pay_unit])
  ifelse(is.na(amount), "\u2014", paste0(amount, ifelse(is.na(suffix), "", suffix)))
}

#' BLS pay estimate as text
#'
#' BLS OEWS regional median for the posting's role (Est_* from [attach_oews_estimates()]), shown only where the
#' posting states no pay: "$34,750/yr", with the role and area in a hover tooltip ("BLS median for teaching
#' assistants, West Montana nonmetro").
#' @param est_pay,est_unit The estimate and its unit.
#' @param occupation,area The OEWS occupation and area names.
#' @param has_posted_pay TRUE where the posting states its own pay (the estimate is then not shown).
#' @export
format_bls_estimate <- function(est_pay, est_unit, occupation, area, has_posted_pay) {
  format_pay_estimate(est_pay, est_unit, paste0("BLS median for ", tolower(occupation), ", ", area), has_posted_pay)
}

#' A pay estimate as text with a hover note
#'
#' An HTML span with `tooltip` as its title, or an em dash where the posting has its own pay or there is no estimate.
#' @param est_pay,est_unit The estimate and its unit.
#' @param tooltip The hover text (escaped here).
#' @param has_posted_pay TRUE where the posting states its own pay.
#' @export
format_pay_estimate <- function(est_pay, est_unit, tooltip, has_posted_pay) {
  show <- !has_posted_pay & !is.na(est_pay)
  tip <- htmltools::htmlEscape(tooltip, attribute = TRUE)
  ifelse(show,
         paste0("<span title=\"", tip, "\" style=\"border-bottom: 1px dotted #888; cursor: help;\">",
                format_posted_pay(est_pay, est_pay, est_unit), "</span>"),
         "\u2014")
}

#' The pay figure a posting is filtered on
#'
#' Its posted pay (midpoint of a range) when stated by the hour or year; nothing when stated in another unit (a day
#' rate or stipend is the posting's own figure, so no estimate replaces it); otherwise the fallback the caller
#' supplies (district role figure, then the BLS estimate).
#' @param pay_min,pay_max,pay_unit The posting's own pay.
#' @param fallback_value,fallback_unit The figure to use where the posting states none.
#' @return A data frame with `value` and `unit`.
#' @export
posting_pay_value <- function(pay_min, pay_max, pay_unit, fallback_value, fallback_unit) {
  posted <- !is.na(pay_min)
  filterable <- posted & pay_unit %in% c("hour", "year")
  data.frame(
    value = ifelse(filterable, (pay_min + coalesce(pay_max, pay_min)) / 2, ifelse(posted, NA_real_, fallback_value)),
    unit = ifelse(filterable, pay_unit, ifelse(posted, NA_character_, fallback_unit)),
    stringsAsFactors = FALSE
  )
}

# Lets "31 openings" read differently for a district climbing steadily and one that just posted a batch. Plain SVG
# templating rather than a ggplot/plotly render, so it is cheap enough to make per table row.
#' Sparkline SVG
#'
#' Tiny inline trend chart (no axes or legend, just the shape of a trend) for the Top Hiring tables and KPI tiles.
#' The end dot is green when the series ends above where it started, red below, grey level. "" for fewer than two
#' non-missing values.
#' @param series Numeric series, oldest first.
#' @param accent Line colour (hex).
#' @export
make_sparkline_svg <- function(series, accent = "#2a78d6") {
  series <- series[!is.na(series)]
  if (length(series) < 2) return("")

  w <- 96; h <- 28; pad <- 3
  rng <- range(series)
  span <- diff(rng)
  if (span == 0) span <- 1

  n <- length(series)
  step_x <- (w - pad * 2) / (n - 1)
  xs <- pad + (seq_len(n) - 1) * step_x
  ys <- pad + (1 - (series - rng[1]) / span) * (h - pad * 2)

  line_path <- paste0(ifelse(seq_along(xs) == 1, "M", "L"), round(xs, 1), ",", round(ys, 1), collapse = " ")
  area_path <- paste0(line_path, " L", round(xs[n], 1), ",", h - pad, " L", round(xs[1], 1), ",", h - pad, " Z")

  dot_color <- if (series[n] > series[1]) "#1baf7a" else if (series[n] < series[1]) "#e34948" else "#999999"

  sprintf(
    '<svg width="%d" height="%d" viewBox="0 0 %d %d" style="vertical-align:middle;"><path d="%s" fill="%s22" stroke="none"></path><path d="%s" fill="none" stroke="%s" stroke-width="1.75" stroke-linejoin="round" stroke-linecap="round"></path><circle cx="%s" cy="%s" r="2.5" fill="%s"></circle></svg>',
    w, h, w, h, area_path, accent, line_path, accent, round(xs[n], 1), round(ys[n], 1), dot_color
  )
}

#' Sparkline SVG for a KPI tile
#'
#' Larger, monochrome-white variant of [make_sparkline_svg()] for the KPI tiles' icon slot.
#' @param series Numeric series, oldest first.
#' @export
make_sparkline_svg_light <- function(series) {
  series <- series[!is.na(series)]
  if (length(series) < 2) return("")

  w <- 160; h <- 80; pad <- 6
  rng <- range(series)
  span <- diff(rng)
  if (span == 0) span <- 1

  n <- length(series)
  step_x <- (w - pad * 2) / (n - 1)
  xs <- pad + (seq_len(n) - 1) * step_x
  ys <- pad + (1 - (series - rng[1]) / span) * (h - pad * 2)

  line_path <- paste0(ifelse(seq_along(xs) == 1, "M", "L"), round(xs, 1), ",", round(ys, 1), collapse = " ")
  area_path <- paste0(line_path, " L", round(xs[n], 1), ",", h - pad, " L", round(xs[1], 1), ",", h - pad, " Z")

  sprintf(
    '<svg width="%d" height="%d" viewBox="0 0 %d %d"><path d="%s" fill="rgba(255,255,255,0.18)" stroke="none"></path><path d="%s" fill="none" stroke="rgba(255,255,255,0.85)" stroke-width="2.5" stroke-linejoin="round" stroke-linecap="round"></path><circle cx="%s" cy="%s" r="3.5" fill="#ffffff"></circle></svg>',
    w, h, w, h, area_path, line_path, round(xs[n], 1), round(ys[n], 1)
  )
}

# Why month, quarter and year, not week (WY app.R, measured 2026-08-05 on WY's data): at the category level
# week-over-week is noise (median absolute change 0); month is weak but real (median 1); quarter is strong (median 4)
# but mixes in the seasonal hiring cycle (Elementary peaks in spring, troughs in late summer); year compares two points
# at the same place in that cycle (Elementary was -31 quarter-over-quarter but -17 year-over-year for the same "now").
# `history` is already filtered to one district or college and summed over other dimensions (e.g. HE's Job_Type).
#' Current Trends table
#'
#' One row per category: current count, a 12-week sparkline, and the change against a month, a quarter and a year
#' ago (the nearest archive on or before each). Works off whatever history it is given: flat or blank changes
#' until enough weekly history has built up.
#' @param history Weekly counts with `Archive_Date` (Date), `Category` and `n`.
#' @param accent Sparkline colour (hex).
#' @return A data frame, or NULL with no history.
#' @export
build_current_trends_table <- function(history, accent) {
  dates <- sort(unique(history$Archive_Date))
  if (length(dates) == 0) return(NULL)
  latest <- max(dates)

  nearest_on_or_before <- function(target) {
    candidates <- dates[dates <= target]
    if (length(candidates) == 0) return(NA)
    max(candidates)
  }
  month_ago <- nearest_on_or_before(latest - 28)
  quarter_ago <- nearest_on_or_before(latest - 90)
  year_ago <- nearest_on_or_before(latest - 365)

  current <- history %>%
    filter(Archive_Date == latest) %>%
    group_by(Category) %>%
    summarize(`Current Postings` = sum(n), .groups = "drop") %>%
    arrange(desc(`Current Postings`))
  if (nrow(current) == 0) return(NULL)

  value_at <- function(cat, date) {
    if (is.na(date)) return(NA_integer_)
    v <- history$n[history$Category == cat & history$Archive_Date == date]
    if (length(v) == 0) 0L else sum(v)
  }
  fmt_delta <- function(now, then) {
    if (is.na(then)) return("<span style='color:#999;'>\u2014</span>")
    d <- now - then
    color <- if (d > 0) "#1baf7a" else if (d < 0) "#e34948" else "#999999"
    arrow <- if (d >= 0) "\u25b2" else "\u25bc"
    sprintf("<span style='color:%s;'>%s %d</span>", color, arrow, abs(d))
  }

  current$`Last 12 Weeks` <- vapply(current$Category, function(cat) {
    series <- history %>%
      filter(Category == cat) %>%
      group_by(Archive_Date) %>%
      summarize(n = sum(n), .groups = "drop") %>%
      arrange(Archive_Date) %>%
      utils::tail(12) %>%
      pull(n)
    make_sparkline_svg(series, accent = accent)
  }, character(1))

  current$`vs Month Ago` <- mapply(function(cat, now) fmt_delta(now, value_at(cat, month_ago)),
                                    current$Category, current$`Current Postings`)
  current$`vs Quarter Ago` <- mapply(function(cat, now) fmt_delta(now, value_at(cat, quarter_ago)),
                                      current$Category, current$`Current Postings`)
  current$`vs Year Ago` <- mapply(function(cat, now) fmt_delta(now, value_at(cat, year_ago)),
                                   current$Category, current$`Current Postings`)

  current
}

#' Latest snapshot in each week
#'
#' The weekly-totals downloads keep one snapshot per week (decided 2026-11-02): a week with an extra run (the
#' August catch-up runs) would otherwise list every district twice. The latest snapshot in each ISO week wins.
#' @param weekly_totals Rows with `Archive_Date`.
#' @export
latest_snapshot_per_week <- function(weekly_totals) {
  d <- as.Date(weekly_totals$Archive_Date)
  wk <- format(d, "%G-W%V")
  latest <- tapply(d, wk, max)
  weekly_totals[!is.na(d) & d == as.Date(latest[wk], origin = "1970-01-01"), , drop = FALSE]
}

# Statewide totals smooth per-district noise into a real signal (WY 2026-08-05: K-12 totals move ~20-50 a week,
# per-district week-over-week changes have a median absolute value of 0). Archive cadence isn't regular (several
# same-week CI runs, e.g. WY's 2026-08-03 archives), so without min_days_back an extra same-week run would make
# "vs last week" compare against a snapshot from hours earlier and understate or invent the KPI tile's change.
#' Statewide week-over-week change
#'
#' The latest snapshot's total minus the latest one at least `min_days_back` days older (so an extra same-week run
#' is skipped). NA with no snapshot that old.
#' @param weekly_totals Rows with `Archive_Date` (Date) and `n`.
#' @param min_days_back Minimum age in days of the comparison snapshot.
#' @export
compute_wow_delta <- function(weekly_totals, min_days_back = 5) {
  totals <- weekly_totals %>% group_by(Archive_Date) %>% summarize(n = sum(n), .groups = "drop") %>% arrange(Archive_Date)
  if (nrow(totals) < 2) return(NA_integer_)
  latest <- max(totals$Archive_Date)
  candidates <- totals$Archive_Date[totals$Archive_Date <= latest - min_days_back]
  if (length(candidates) == 0) return(NA_integer_)
  prior <- max(candidates)
  totals$n[totals$Archive_Date == latest] - totals$n[totals$Archive_Date == prior]
}

# Districts and colleges are mostly flat week to week (median absolute change 0), but real movement shows over 3+
# weeks (WY: Natrona SD1 +22, University of Wyoming +52, each over 4 weeks). The comparison is the nearest snapshot
# at least min_days_back old rather than a fixed index, since archive cadence isn't regular.
#' Biggest mover
#'
#' The district or college whose count changed most (absolute) between the latest snapshot and the latest one at
#' least `min_days_back` days older. Needs a longer window than a week to mean anything.
#' @param weekly_totals Rows with `Archive_Date` (Date), `n` and the `name_col` column.
#' @param name_col The name column ("District" or "Institution").
#' @param min_days_back Minimum age in days of the comparison snapshot.
#' @return A list (`name`, `prior`, `latest`, `delta`, `prior_date`, `latest_date`), or NULL.
#' @export
find_biggest_mover <- function(weekly_totals, name_col, min_days_back = 21) {
  dates <- sort(unique(weekly_totals$Archive_Date))
  if (length(dates) < 2) return(NULL)
  latest <- max(dates)
  candidates <- dates[dates <= latest - min_days_back]
  if (length(candidates) == 0) return(NULL)
  prior <- max(candidates)

  wide <- weekly_totals %>%
    filter(Archive_Date %in% c(latest, prior)) %>%
    tidyr::pivot_wider(names_from = Archive_Date, values_from = n, values_fill = 0)
  names(wide)[names(wide) == as.character(prior)] <- "Prior"
  names(wide)[names(wide) == as.character(latest)] <- "Latest"
  wide$Delta <- wide$Latest - wide$Prior

  top <- wide %>% arrange(desc(abs(Delta))) %>% slice(1)
  list(name = top[[name_col]], prior = top$Prior, latest = top$Latest, delta = top$Delta,
       prior_date = prior, latest_date = latest)
}

#' A college posting's map dot name
#'
#' Branch campuses, outreach centers and field offices ([place_he_postings()], 2026-11-16): a posting whose
#' Location names another town in the state has Campus_Town set by the pipeline and gets its own map dot, keyed
#' "<Institution> (<Town>)"; every other posting is on the institution's main dot.
#' @param institution Institution names.
#' @param town Campus_Town (NA or "" for the main campus).
#' @export
he_map_name <- function(institution, town) ifelse(is.na(town) | !nzchar(town), institution, paste0(institution, " (", town, ")"))

#' Match job titles to a search
#'
#' Flexible query matching for map and job searches: keywords, abbreviations ("special ed", "sped", "math", "pe",
#' "cte", "driver") and multi-word queries (every word must appear, in any order). An empty query matches all.
#' @param title_vec Titles (or title plus position text).
#' @param query The search text.
#' @export
match_job_query <- function(title_vec, query) {
  if (is.null(query) || !nzchar(trimws(query))) return(rep(TRUE, length(title_vec)))
  q <- trimws(query)
  q_lower <- tolower(q)
  pattern <- if (q_lower %in% c("special ed", "sped", "special education")) {
    "special[[:space:]]*ed|sped|special[[:space:]]*educ"
  } else if (q_lower %in% c("math", "mathematics")) {
    "math"
  } else if (q_lower %in% c("pe", "phys ed", "physical education")) {
    "physical[[:space:]]*ed|phys[[:space:]]*ed|\\bpe\\b"
  } else if (q_lower %in% c("cte", "career and technical", "vocational")) {
    "cte|career[[:space:]]*and[[:space:]]*tech|vocational|industrial[[:space:]]*arts"
  } else if (q_lower %in% c("bus driver", "driver", "transportation")) {
    "driver|transportation"
  } else {
    words <- strsplit(q, "[[:space:]]+")[[1]]
    words <- words[nzchar(words)]
    # What a visitor types is text, not a pattern: "(" or "c++" must match literally, never error (review 11-17).
    words <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", words)
    if (length(words) > 1) {
      paste0("(?=.*", words, ")", collapse = "")
    } else {
      words
    }
  }
  !is.na(title_vec) & grepl(pattern, title_vec, ignore.case = TRUE, perl = TRUE)
}

# If every Vacancy_Rate is NA (the CCD and IPEDS staffing fetches both failing, or a fresh checkout), range() returns
# c(Inf, -Inf); colorNumeric() then gets an invalid domain and the map legend breaks for every user. The 0-1
# placeholder is never used to colour a marker (every rate is NA, so every marker gets na.color).
#' Colour scale range for vacancy rates
#'
#' The range of the rates, or c(0, 1) when every rate is missing (instead of Inf/-Inf).
#' @param vacancy_rate Vacancy rates.
#' @export
compute_vacancy_rate_domain <- function(vacancy_rate) {
  if (all(is.na(vacancy_rate))) c(0, 1) else range(vacancy_rate, na.rm = TRUE)
}

# Phase 6, slice 2 (2026-11-16): helpers both states' app.R defined identically but that read an app global. The
# global becomes an argument; where both states set it to the same value, that value is the default. Bodies are as
# they were in app.R otherwise.

#' Pad a loaded data file's missing columns
#'
#' Adds every column of `required_cols` that `df` lacks, filled with NA, so a pipeline output that lost a column
#' shows blank values instead of crashing the dashboard. Each file with missing columns is reported once through
#' `on_missing`; the apps collect those messages for a Home-tab banner.
#' @param df A loaded data frame.
#' @param required_cols Columns the app reads.
#' @param source_name File name for the message.
#' @param on_missing Function given the message when columns are missing. Defaults to a warning.
#' @return `df`, with any missing columns added as NA.
#' @export
validate_and_pad_schema <- function(df, required_cols, source_name,
                                    on_missing = function(msg) warning(msg, call. = FALSE)) {
  missing <- setdiff(required_cols, names(df))
  if (length(missing) > 0) {
    on_missing(sprintf(
      "%s is missing expected column(s): %s. That data will show blank until the pipeline is fixed.",
      source_name, paste(missing, collapse = ", ")
    ))
    # rep(): a header-only file has zero rows, where `df[[col]] <- NA` fails (review 11-17).
    for (col in missing) df[[col]] <- rep(NA, nrow(df))
  }
  df
}

#' Which postings fall in a pay band
#'
#' @param value,unit Posted pay value and its unit (from [posting_pay_value()]).
#' @param band Name of a band in `bands`; NULL or an unknown name keeps every posting.
#' @param bands Named list of bands, each a list with `unit`, `lo` and `hi` (the state's `PAY_BANDS`; `lo` is
#'   inclusive, `hi` exclusive).
#' @return A logical vector.
#' @export
in_pay_band <- function(value, unit, band, bands) {
  if (is.null(band) || !band %in% names(bands)) return(rep(TRUE, length(value)))
  b <- bands[[band]]
  !is.na(value) & !is.na(unit) & unit == b$unit & value >= b$lo & value < b$hi
}

#' Relabel posting-spell statuses for display
#'
#' The posting-spell files say "Closed / Filled" for a spell whose posting stopped appearing. The dashboard shows
#' "No longer listed": a listing ends when it stops appearing, whether it was filled, cancelled, moved to another
#' board or missed by a scrape (the apps' spells footnote).
#' @param df A posting-spells data frame with a `Status` column.
#' @param labels Named character vector: names are file statuses, values the labels shown.
#' @export
relabel_spell_status <- function(df, labels = c("Closed / Filled" = "No longer listed")) {
  if (nrow(df) > 0 && "Status" %in% names(df)) {
    hit <- df$Status %in% names(labels)
    df$Status[hit] <- labels[df$Status[hit]]
  }
  df
}

#' Map marker radius from a posting count
#'
#' @param current_count Current postings at the marker.
#' @param min_radius,max_radius Radius in pixels for zero postings, and the cap.
#' @export
map_marker_radius <- function(current_count, min_radius = 6, max_radius = 24) {
  pmin(max_radius, min_radius + 1.2 * sqrt(current_count))
}

#' A download file's column definitions, for its help popover
#'
#' @param file_name The file, as in the data dictionary's `File_Name`.
#' @param dictionary The app's data dictionary (`File_Name`, `Column_Name`, `Definition`).
#' @return An htmltools tag list, or NULL when the file has no rows in the dictionary.
#' @export
dictionary_columns_help <- function(file_name, dictionary) {
  d <- dictionary[dictionary$File_Name == file_name, , drop = FALSE]
  if (nrow(d) == 0) return(NULL)
  htmltools::tagList(
    htmltools::p(class = "mb-1", htmltools::tags$strong(nrow(d), " columns:")),
    htmltools::tags$dl(class = "help-columns mb-0",
                       lapply(seq_len(nrow(d)), function(i) htmltools::tagList(htmltools::tags$dt(htmltools::tags$code(d$Column_Name[i])), htmltools::tags$dd(d$Definition[i]))))
  )
}

#' "(retrieved ...)" text for the data-sources footer
#'
#' @param sources Source names, as in `retrieved$Source`.
#' @param retrieved Data frame with `Source` and `Retrieved` (the app's `source_retrieved_dates.csv`).
#' @return " (retrieved D)", " (retrieved D1 to D2)" for several dates, or "" when none is known.
#' @export
footer_retrieved <- function(sources, retrieved) {
  x <- sort(unique(retrieved$Retrieved[retrieved$Source %in% sources & nzchar(retrieved$Retrieved)]))
  if (length(x) == 0) "" else if (length(x) == 1) paste0(" (retrieved ", x, ")") else paste0(" (retrieved ", x[1], " to ", x[length(x)], ")")
}
