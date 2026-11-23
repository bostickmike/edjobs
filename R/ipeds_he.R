# ---------------------------------------------------------------------------
# IPEDS faculty salaries, fall enrollment (FTE) and FSA Pell share
# ---------------------------------------------------------------------------
#
# Institution-level context for the higher-ed side of each dashboard, from
# the Urban Institute's Education Data Portal
# (https://educationdata.urban.org/documentation/colleges.html). Moved from
# the states' ipeds_salary_scraper.R, ipeds_enrollment_scraper.R and
# fsa_pell_scraper.R (package phase 5), which differed only in the state FIPS
# code and the unitid map.
#
# The unitid map is state data: a data frame with `unitid` (integer) and
# `Name` (the dashboard's institution name). Two names may share a unitid
# (Wyoming's Sheridan and Gillette colleges report together as Northern
# Wyoming Community College District); each gets the shared figures. An
# optional `Salary_Note` column is carried into parse_ipeds_he_salaries()'s
# output, so a state can explain such a case on the dashboard.
#
# Endpoints and quirks:
# - salaries-instructional-staff: one row per unitid x academic_rank x
#   contract_length x sex; 99 = all. IPEDS collects salaries for full-time
#   instructional staff only. The salary figures are the average over staff on
#   9-, 10-, 11- and 12-month contracts (contract_length 9-12: outlays / count;
#   Mike 2026-11-16), not the contract_length 99 "Total" row: that row counts
#   staff on contracts under 9 months (code 8), whose salaries IPEDS doesn't
#   collect, so it divides the others' pay by too many people (Blackfeet 2024:
#   $1,357,915 for 25 people on 10-month contracts over 53 heads = $25,621 vs
#   $54,317). Faculty_Count stays the Total row's count: all full-time
#   instructional staff. academic_rank 1 is Professor.
#   claude-notes/2026-11-16_ipeds-faculty-salary.md.
# - fall-enrollment: despite its name, this is IPEDS's FTE enrollment survey
#   (one row per unitid x level_of_study with est_fte/rep_fte; Urban's own
#   docs call it enrollment-full-time-equivalent). est_fte (computed from
#   credit hours) and rep_fte (reported by the institution) are two methods for
#   the same quantity and which one is filled varies (University of Wyoming's
#   first-professional row: est_fte -1, rep_fte 417), so each row takes
#   whichever is real, then levels are summed per institution.
# - fsa/grants: grant_type 4 is Pell. grant_recipients_unitid is the
#   unduplicated recipient count at that unitid (grant_recipients_opeid can
#   double-count across unitids sharing an OPEID). FSA lags IPEDS by a couple of
#   years (2021 vs 2023 on 2026-08-06), so the Pell share divides by FTE
#   enrollment from the same year as the grants data, never the latest
#   enrollment. A recipients-over-FTE ratio, not two comparable headcounts.
# - Negative values are IPEDS/FSA sentinel codes (-1 missing, -2 not
#   applicable, -3 suppressed) and become NA.
# - Each survey is published with a lag; the find_latest_*() functions walk
#   back from this calendar year to the first year with rows for the state.
# - Records mix JSON nulls with values, so pages are read with
#   fetch_json_paginated() (simplifyVector = TRUE turns nulls into NA).

#' `IPEDS_SALARY_ENDPOINT`
#' @keywords internal
IPEDS_SALARY_ENDPOINT <- "https://educationdata.urban.org/api/v1/college-university/ipeds/salaries-instructional-staff"

#' `IPEDS_ENROLLMENT_ENDPOINT`
#' @keywords internal
IPEDS_ENROLLMENT_ENDPOINT <- "https://educationdata.urban.org/api/v1/college-university/ipeds/fall-enrollment"

#' `IPEDS_COLLEGE_DIRECTORY_ENDPOINT`
#' @keywords internal
IPEDS_COLLEGE_DIRECTORY_ENDPOINT <- "https://educationdata.urban.org/api/v1/college-university/ipeds/directory"

#' `FSA_GRANTS_ENDPOINT`
#' @keywords internal
FSA_GRANTS_ENDPOINT <- "https://educationdata.urban.org/api/v1/college-university/fsa/grants"

#' `FSA_PELL_GRANT_TYPE`
#'
#' FSA's grant_type code for Pell Grants.
#' @keywords internal
FSA_PELL_GRANT_TYPE <- 4L

#' Clean an IPEDS or FSA value
#'
#' Negative sentinel codes and NA become NA; everything else is kept.
#' @param x A numeric vector.
#' @keywords internal
clean_ipeds_value <- function(x) ifelse(is.na(x) | x < 0, NA_real_, x)

#' @rdname clean_ipeds_value
clean_ipeds_enrollment_value <- clean_ipeds_value

#' @rdname clean_ipeds_value
clean_fsa_value <- clean_ipeds_value

#' Fetch one Urban Institute IPEDS/FSA endpoint for one year and state
#'
#' @param endpoint One of the `*_ENDPOINT` constants.
#' @param year Survey year.
#' @param fips Two-digit state FIPS code; defaults to the current state config's.
#' @keywords internal
fetch_urban_college_year <- function(endpoint, year, fips = state_fips()) {
  fetch_json_paginated(paste0(endpoint, "/", year, "/?fips=", fips))
}

#' Find the latest year an Urban Institute endpoint has rows for a state
#'
#' Walks back from `start_year` and returns `list(year, data)` for the first
#' year with rows, or `list(year = NA, data = data.frame())` when none has.
#' With `quiet_errors = TRUE` a 404 counts as no rows ([try_federal_year()]);
#' any other failed request propagates either way (until 2026-11-17 every
#' error counted as no rows, so a 5xx on the newest year shipped an older one).
#' @param endpoint One of the `*_ENDPOINT` constants.
#' @param fips See [fetch_urban_college_year()].
#' @param start_year,years_back Years searched: `start_year` down to `start_year - years_back`.
#' @param quiet_errors TRUE to treat a failed request (a 404 for a year not yet published) as no rows and keep searching; FALSE to stop on it.
#' @keywords internal
find_latest_urban_college_year <- function(endpoint, fips = state_fips(),
                                           start_year = as.integer(format(Sys.Date(), "%Y")),
                                           years_back = 5, quiet_errors = FALSE) {
  for (year in seq(start_year, start_year - years_back)) {
    df <- if (quiet_errors) {
      try_federal_year(fetch_urban_college_year(endpoint, year, fips))
    } else {
      fetch_urban_college_year(endpoint, year, fips)
    }
    if (nrow(df) > 0) return(list(year = year, data = df))
  }
  list(year = NA_integer_, data = data.frame())
}

check_unitid_map <- function(unitid_map) {
  if (!is.data.frame(unitid_map) || !all(c("unitid", "Name") %in% names(unitid_map))) {
    stop("unitid_map must be a data frame with unitid and Name columns")
  }
  unitid_map
}

# ---- Instructional staff salaries ------------------------------------------

#' Find the latest IPEDS instructional staff salary year
#' @inheritParams find_latest_urban_college_year
#' @keywords internal
find_latest_ipeds_salary_year <- function(fips = state_fips(), start_year = as.integer(format(Sys.Date(), "%Y")),
                                          years_back = 5) {
  find_latest_urban_college_year(IPEDS_SALARY_ENDPOINT, fips, start_year, years_back, quiet_errors = TRUE)
}

#' Average salary over 9- to 12-month contracts
#'
#' Per unitid: salary outlays / staff over contract_length 9-12 (both sexes, the given rank), using only
#' contract groups that report both a positive count and positive outlays. NA when none do.
#' @param df Rows from the salaries-instructional-staff endpoint.
#' @param rank academic_rank (99 = all ranks, 1 = Professor).
#' @keywords internal
ipeds_avg_salary_9to12 <- function(df, rank = 99) {
  x <- df[which(df$sex == 99 & df$academic_rank == rank & df$contract_length %in% 9:12 &
                  df$instruc_staff_count > 0 & df$salary_outlays > 0), , drop = FALSE]
  if (nrow(x) == 0) return(data.frame(unitid = df$unitid[0], avg = numeric(0)))
  a <- stats::aggregate(cbind(outlays = salary_outlays, n = instruc_staff_count) ~ unitid, data = x, FUN = sum)
  data.frame(unitid = a$unitid, avg = a$outlays / a$n)
}

#' Parse IPEDS instructional staff salaries
#'
#' One row per `unitid_map` row: Name, Faculty_Avg_Salary (all ranks, full-time staff on 9-12-month contracts),
#' Faculty_Avg_Salary_Professor, Faculty_Count, Salary_Year, and Salary_Note
#' when the map has that column. An institution missing from `df` keeps its
#' row with NA figures.
#' @param df Rows from the salaries-instructional-staff endpoint.
#' @param year Survey year.
#' @param unitid_map See the file header.
#' @return Data frame with one row per `unitid_map` row: `Name`, `Faculty_Avg_Salary`, `Faculty_Avg_Salary_Professor`, `Faculty_Count`, `Salary_Year` (and `Salary_Note` when the map has it).
#' @export
parse_ipeds_he_salaries <- function(df, year, unitid_map) {
  check_unitid_map(unitid_map)
  has_note <- "Salary_Note" %in% names(unitid_map)
  cols <- c("Name", "Faculty_Avg_Salary", "Faculty_Avg_Salary_Professor", "Faculty_Count", "Salary_Year",
            if (has_note) "Salary_Note")
  if (nrow(df) == 0) {
    empty <- data.frame(Name = character(0), Faculty_Avg_Salary = numeric(0),
                        Faculty_Avg_Salary_Professor = numeric(0), Faculty_Count = numeric(0),
                        Salary_Year = character(0), Salary_Note = character(0), stringsAsFactors = FALSE)
    return(empty[, cols, drop = FALSE])
  }
  total <- df[which(df$contract_length == 99 & df$sex == 99 & df$academic_rank == 99), , drop = FALSE]
  total <- data.frame(unitid = total$unitid, Faculty_Count = clean_ipeds_value(total$instruc_staff_count))
  overall <- ipeds_avg_salary_9to12(df, 99)
  names(overall)[2] <- "Faculty_Avg_Salary"
  professor <- ipeds_avg_salary_9to12(df, 1)
  names(professor)[2] <- "Faculty_Avg_Salary_Professor"
  out <- unitid_map %>%
    dplyr::left_join(overall, by = "unitid") %>%
    dplyr::left_join(total, by = "unitid") %>%
    dplyr::left_join(professor, by = "unitid")
  out$Salary_Year <- as.character(year)
  as.data.frame(out)[, cols, drop = FALSE]
}

#' Fetch IPEDS instructional staff salaries for the latest year
#' @inheritParams parse_ipeds_he_salaries
#' @inheritParams fetch_urban_college_year
#' @return As `parse_ipeds_he_salaries()`, for the latest IPEDS year with data.
#' @export
fetch_ipeds_he_salaries <- function(unitid_map, fips = state_fips()) {
  latest <- find_latest_ipeds_salary_year(fips)
  parse_ipeds_he_salaries(latest$data, latest$year, unitid_map)
}

#' Parse one year of the IPEDS salary trend
#'
#' Name, Year, Faculty_Avg_Salary (the all-ranks, 9-12-month-contract figure) per map row.
#' @inheritParams parse_ipeds_he_salaries
#' @keywords internal
parse_ipeds_salary_trend_year <- function(df, year, unitid_map) {
  check_unitid_map(unitid_map)
  if (nrow(df) == 0) {
    return(data.frame(Name = character(0), Year = integer(0), Faculty_Avg_Salary = numeric(0)))
  }
  overall <- ipeds_avg_salary_9to12(df, 99)
  names(overall)[2] <- "Faculty_Avg_Salary"
  out <- dplyr::left_join(unitid_map, overall, by = "unitid")
  data.frame(Name = out$Name, Year = rep(year, nrow(out)), Faculty_Avg_Salary = out$Faculty_Avg_Salary)
}

#' Fetch the IPEDS salary trend
#'
#' The latest year and the `n_years - 1` before it, stacked; a failed prior
#' year contributes no rows.
#' @inheritParams fetch_ipeds_he_salaries
#' @param n_years Number of years, counting the latest.
#' @return Data frame with one row per institution and year: `Name`, `Year`, `Faculty_Avg_Salary`.
#' @export
fetch_ipeds_he_salary_trend <- function(unitid_map, fips = state_fips(), n_years = 3) {
  latest <- find_latest_ipeds_salary_year(fips)
  if (is.na(latest$year)) {
    return(data.frame(Name = character(0), Year = integer(0), Faculty_Avg_Salary = numeric(0)))
  }
  rows <- lapply(seq(latest$year, latest$year - n_years + 1), function(y) {
    df <- if (y == latest$year) latest$data else
      tryCatch(fetch_urban_college_year(IPEDS_SALARY_ENDPOINT, y, fips), error = function(e) data.frame())
    parse_ipeds_salary_trend_year(df, y, unitid_map)
  })
  dplyr::bind_rows(rows)
}

# ---- Fall enrollment (FTE) --------------------------------------------------

#' Find the latest IPEDS fall enrollment year
#' @inheritParams find_latest_urban_college_year
#' @keywords internal
find_latest_ipeds_enrollment_year <- function(fips = state_fips(), start_year = as.integer(format(Sys.Date(), "%Y")),
                                              years_back = 5) {
  find_latest_urban_college_year(IPEDS_ENROLLMENT_ENDPOINT, fips, start_year, years_back)
}

#' Parse IPEDS fall enrollment
#'
#' Name, Enrollment (FTE summed over levels of study; NA when no level has a
#' real figure), Enrollment_Year, per map row.
#' @inheritParams parse_ipeds_he_salaries
#' @keywords internal
parse_ipeds_he_enrollment <- function(df, year, unitid_map) {
  check_unitid_map(unitid_map)
  if (nrow(df) == 0) {
    return(data.frame(Name = character(0), Enrollment = numeric(0), Enrollment_Year = character(0),
                      stringsAsFactors = FALSE))
  }
  fte <- dplyr::coalesce(clean_ipeds_value(df$est_fte), clean_ipeds_value(df$rep_fte))
  totals <- vapply(split(fte, df$unitid), function(v) if (all(is.na(v))) NA_real_ else sum(v, na.rm = TRUE), numeric(1))
  totals <- data.frame(unitid = as.numeric(names(totals)), Enrollment = unname(totals))
  out <- dplyr::left_join(unitid_map, totals, by = "unitid")
  data.frame(Name = out$Name, Enrollment = out$Enrollment, Enrollment_Year = rep(as.character(year), nrow(out)),
             stringsAsFactors = FALSE)
}

#' Fetch IPEDS fall enrollment for the latest year
#' @inheritParams fetch_ipeds_he_salaries
#' @return Data frame with one row per `unitid_map` row: `Name`, `Enrollment`, `Enrollment_Year`, for the latest IPEDS year with data.
#' @export
fetch_ipeds_he_enrollment <- function(unitid_map, fips = state_fips()) {
  latest <- find_latest_ipeds_enrollment_year(fips)
  parse_ipeds_he_enrollment(latest$data, latest$year, unitid_map)
}

#' Compute the change in enrollment between two years
#'
#' Name, Enrollment_Change_Pct ((current - prior) / prior; NA when either is
#' missing or the prior is 0), for names in both frames.
#' @param current,prior Outputs of [parse_ipeds_he_enrollment()].
#' @keywords internal
compute_enrollment_change <- function(current, prior) {
  if (nrow(current) == 0 || nrow(prior) == 0) {
    return(data.frame(Name = character(0), Enrollment_Change_Pct = numeric(0), stringsAsFactors = FALSE))
  }
  both <- dplyr::inner_join(as.data.frame(current)[, c("Name", "Enrollment")],
                            stats::setNames(as.data.frame(prior)[, c("Name", "Enrollment")], c("Name", "Enrollment_Prior")),
                            by = "Name")
  ok <- !is.na(both$Enrollment) & !is.na(both$Enrollment_Prior) & both$Enrollment_Prior > 0
  data.frame(Name = both$Name,
             Enrollment_Change_Pct = ifelse(ok, (both$Enrollment - both$Enrollment_Prior) / both$Enrollment_Prior, NA_real_),
             stringsAsFactors = FALSE, row.names = NULL)
}

#' Fetch the IPEDS enrollment trend
#'
#' Change from `trend_years_back` years before the latest year (5, the same
#' window as the K-12 side's Population_Change_Pct).
#' @inheritParams fetch_ipeds_he_salaries
#' @param trend_years_back How many years before the latest the change is measured from.
#' @return Data frame: `Name`, `Enrollment_Change_Pct` (a proportion; NA without both years).
#' @export
fetch_ipeds_he_enrollment_trend <- function(unitid_map, fips = state_fips(), trend_years_back = 5) {
  latest <- find_latest_ipeds_enrollment_year(fips)
  if (is.na(latest$year)) {
    return(data.frame(Name = character(0), Enrollment_Change_Pct = numeric(0), stringsAsFactors = FALSE))
  }
  current <- parse_ipeds_he_enrollment(latest$data, latest$year, unitid_map)
  prior_year <- latest$year - trend_years_back
  prior <- parse_ipeds_he_enrollment(fetch_urban_college_year(IPEDS_ENROLLMENT_ENDPOINT, prior_year, fips),
                                     prior_year, unitid_map)
  compute_enrollment_change(current, prior)
}

# ---- FSA Pell recipient share ----------------------------------------------

#' Find the latest FSA grants year
#' @inheritParams find_latest_urban_college_year
#' @keywords internal
find_latest_fsa_grants_year <- function(fips = state_fips(), start_year = as.integer(format(Sys.Date(), "%Y")),
                                        years_back = 6) {
  find_latest_urban_college_year(FSA_GRANTS_ENDPOINT, fips, start_year, years_back)
}

#' Parse the Pell recipient share
#'
#' Name, Pell_Recipient_Share (Pell recipients / FTE enrollment), Pell_Year,
#' per map row. `enrollment_df` must be [parse_ipeds_he_enrollment()] for the
#' same year as `grants_df`.
#' @param grants_df Rows from the fsa/grants endpoint.
#' @param enrollment_df [parse_ipeds_he_enrollment()] output for the same year as `grants_df`.
#' @inheritParams parse_ipeds_he_salaries
#' @keywords internal
parse_ipeds_he_pell_share <- function(grants_df, enrollment_df, year, unitid_map) {
  check_unitid_map(unitid_map)
  if (nrow(grants_df) == 0 || nrow(enrollment_df) == 0) {
    return(data.frame(Name = character(0), Pell_Recipient_Share = numeric(0), Pell_Year = character(0),
                      stringsAsFactors = FALSE))
  }
  pell <- grants_df[which(grants_df$grant_type == FSA_PELL_GRANT_TYPE), , drop = FALSE]
  pell <- data.frame(unitid = pell$unitid, Pell_Recipients = clean_fsa_value(pell$grant_recipients_unitid))
  out <- unitid_map %>%
    dplyr::left_join(pell, by = "unitid") %>%
    dplyr::left_join(enrollment_df[, c("Name", "Enrollment")], by = "Name")
  ok <- !is.na(out$Pell_Recipients) & !is.na(out$Enrollment) & out$Enrollment > 0
  data.frame(Name = out$Name, Pell_Recipient_Share = ifelse(ok, out$Pell_Recipients / out$Enrollment, NA_real_),
             Pell_Year = rep(as.character(year), nrow(out)), stringsAsFactors = FALSE)
}

#' Fetch the Pell recipient share for the latest FSA year
#'
#' Pairs the latest FSA grants year with IPEDS enrollment for that same year.
#' @inheritParams fetch_ipeds_he_salaries
#' @return Data frame with one row per `unitid_map` row: `Name`, `Pell_Recipient_Share`, `Pell_Year`.
#' @export
fetch_ipeds_he_pell_share <- function(unitid_map, fips = state_fips()) {
  latest <- find_latest_fsa_grants_year(fips)
  if (is.na(latest$year)) {
    return(data.frame(Name = character(0), Pell_Recipient_Share = numeric(0), Pell_Year = character(0),
                      stringsAsFactors = FALSE))
  }
  enrollment_df <- parse_ipeds_he_enrollment(
    fetch_urban_college_year(IPEDS_ENROLLMENT_ENDPOINT, latest$year, fips), latest$year, unitid_map)
  parse_ipeds_he_pell_share(latest$data, enrollment_df, latest$year, unitid_map)
}

# ---- College directory -------------------------------------------------------

#' Find the latest IPEDS college directory year
#'
#' For checks on the unitid map itself, such as whether two institutions that
#' share a unitid have started reporting separately.
#' @inheritParams find_latest_urban_college_year
#' @return `list(year, data)` for the latest directory year with rows, or `list(year = NA, data = data.frame())`.
#' @export
find_latest_ipeds_directory_year <- function(fips = state_fips(), start_year = as.integer(format(Sys.Date(), "%Y")),
                                             years_back = 5) {
  find_latest_urban_college_year(IPEDS_COLLEGE_DIRECTORY_ENDPOINT, fips, start_year, years_back, quiet_errors = TRUE)
}
