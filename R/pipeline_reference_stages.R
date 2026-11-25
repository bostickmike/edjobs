# Pipeline stages, slice E (2026-11-25; package review, step 5): the reference refreshes and salary, staffing and
# coverage columns both states' Rmds wrote out the same way. State sources (Montana's DLI report, Wyoming's WSBA
# document and UW salary book) stay in the state's Rmd. Each stage writes what the old blocks wrote
# (test-pipeline-reference-stages.R keeps those blocks beside the stages).

# "MT" for Montana: the postal abbreviation the scrape log's names end with and BLS's area rows are keyed by. A state
# base R doesn't list (a toy state, DC) uses its name in log entries and its FIPS code for OEWS.
config_abbr <- function(config) {
  i <- match(config$state, datasets::state.name)
  if (is.na(i)) NA_character_ else datasets::state.abb[i]
}
config_label <- function(config) {
  abbr <- config_abbr(config)
  if (is.na(abbr)) config$state else abbr
}

# Runs `refresh`; an error is logged against the reference table at `path` and the committed file stays.
refresh_or_keep <- function(path, refresh) {
  tryCatch(refresh(path), error = function(e) log_reference_result(path, "error", conditionMessage(e)))
}

#' Pipeline stage: refresh the pay benchmarks
#'
#' The labeled fallbacks for postings that state no pay: BLS OEWS regional pay by role (`oews_role_wages.csv`, fetched
#' only when BLS publishes a new year) and BLS QCEW average pay of public school employees by county
#' (`qcew_county_school_pay.csv`). A failed, partial or implausible refresh keeps the committed file
#' ([write_reference_file()]) and is logged.
#' @param config The state's config; the session's by default (its `app_dir`, `state` and `fips`).
#' @param refresh_oews,fetch_qcew The refresh and the fetch, for tests.
#' @return The OEWS wage table on file afterwards, its codes and year read as text.
#' @export
refresh_pay_benchmarks <- function(config = current_state_config(), refresh_oews = refresh_oews_wages,
                                   fetch_qcew = fetch_qcew_school_pay) {
  if (is.null(config)) stop("refresh_pay_benchmarks(): no state config (set_state_config())")
  oews_path <- file.path(config$app_dir, "oews_role_wages.csv")
  abbr <- config_abbr(config)
  refresh_or_keep(oews_path, function(p) refresh_oews(if (is.na(abbr)) config$fips else abbr, p, retrieved_source = "BLS OEWS"))
  refresh_or_keep(file.path(config$app_dir, "qcew_county_school_pay.csv"),
                  function(p) write_reference_file(fetch_qcew(config$fips), p, retrieved_source = "BLS QCEW"))
  utils::read.csv(oews_path, stringsAsFactors = FALSE,
                  colClasses = c(Area_Code = "character", SOC = "character", Year = "character"))
}

#' Pipeline stage: refresh IPEDS staff pay by category
#'
#' `ipeds_staff_salaries.csv`, the labeled fallback for college staff postings that state no pay
#' ([fetch_ipeds_staff_salaries()], for the colleges in the profile file). A failed fetch keeps the committed file.
#' @param file The colleges' profile CSV inside the app folder; its `IPEDS_UNITID` column names them.
#' @param config The state's config; the session's by default.
#' @param fetch The fetch, for tests.
#' @return NULL, invisibly.
#' @export
refresh_ipeds_staff_pay <- function(file = "salarymap.csv", config = current_state_config(),
                                    fetch = fetch_ipeds_staff_salaries) {
  if (is.null(config)) stop("refresh_ipeds_staff_pay(): no state config (set_state_config())")
  unitids <- stats::na.omit(utils::read.csv(file.path(config$app_dir, file))$IPEDS_UNITID)
  refresh_or_keep(file.path(config$app_dir, "ipeds_staff_salaries.csv"),
                  function(p) write_reference_file(fetch(as.integer(config$fips), unitids), p, retrieved_source = "Urban IPEDS"))
  invisible(NULL)
}

#' The IPEDS salary trend as one row per college
#'
#' The years before the latest, as `Faculty_Avg_Salary_Y1Ago`, `Faculty_Avg_Salary_Y2Ago`: years-ago names, not
#' literal years, so the columns stay the same when the calendar rolls over.
#' @param trend [fetch_ipeds_he_salary_trend()]'s table.
#' @return A data frame with `Name` and one column per prior year.
#' @export
widen_salary_trend <- function(trend) {
  trend$YearsAgo <- max(trend$Year) - trend$Year
  trend <- trend[which(trend$YearsAgo > 0), c("Name", "YearsAgo", "Faculty_Avg_Salary"), drop = FALSE]
  as.data.frame(tidyr::pivot_wider(trend, names_from = "YearsAgo", values_from = "Faculty_Avg_Salary",
                                   names_glue = "Faculty_Avg_Salary_Y{YearsAgo}Ago"))
}

#' Pipeline stage: refresh the colleges' IPEDS faculty salaries
#'
#' Rewrites the profile file's salary columns from IPEDS ([refresh_file_columns()] by `Name`), with `Salary_Source`
#' only where IPEDS gave a figure, then the two prior years' averages ([widen_salary_trend()]). The trend is fetched
#' only when the current salaries came back: its columns are dated from `Salary_Year`. A failed fetch leaves its columns as
#' they were. The new columns go last in the file.
#' @param unitid_map The colleges ([fetch_ipeds_he_salaries()]); a `Salary_Note` column in it is kept.
#' @param file The profile CSV inside the app folder.
#' @param config The state's config; the session's by default.
#' @param fetch,fetch_trend The fetches, for tests: functions of `unitid_map`.
#' @return The salary table (zero rows when the fetch failed), invisibly.
#' @export
refresh_ipeds_he_salaries <- function(unitid_map, file = "salarymap.csv", config = current_state_config(),
                                      fetch = function(m) fetch_ipeds_he_salaries(m, fips = config$fips),
                                      fetch_trend = function(m) fetch_ipeds_he_salary_trend(m, fips = config$fips, n_years = 3)) {
  if (is.null(config)) stop("refresh_ipeds_he_salaries(): no state config (set_state_config())")
  label <- config_label(config)
  salary_cols <- c("Faculty_Avg_Salary", "Faculty_Avg_Salary_Professor", "Faculty_Count", "Salary_Year",
                   if ("Salary_Note" %in% names(unitid_map)) "Salary_Note")
  salary <- refresh_file_columns(
    file, paste0("IPEDS instructional staff salaries (", label, ")"), fetch = function() fetch(unitid_map),
    expected_cols = c("Name", salary_cols), by = "Name", retrieved_source = "Urban IPEDS",
    replace = c(salary_cols, "Salary_Source"), config = config,
    then = function(d) {
      d$Salary_Source <- ifelse(is.na(d$Faculty_Avg_Salary), NA_character_,
                                "IPEDS via Urban Institute Education Data Portal (educationdata.urban.org)")
      d
    })
  if (nrow(salary) > 0) {
    trend <- safe_scrape(paste0("IPEDS instructional staff salary 3-year trend (", label, ")"),
                         retrieved_source = "Urban IPEDS", app_dir = config$app_dir,
                         scrape_fn = function() fetch_trend(unitid_map),
                         expected_cols = c("Name", "Year", "Faculty_Avg_Salary"))
    if (nrow(trend) > 0) {
      path <- file.path(config$app_dir, file)
      old <- utils::read.csv(path, fileEncoding = "UTF-8")
      old <- old[, !grepl("^Faculty_Avg_Salary_Y\\d+Ago$", names(old)), drop = FALSE]
      utils::write.csv(dplyr::left_join(old, widen_salary_trend(trend), by = "Name"), path, row.names = FALSE)
    }
  }
  invisible(salary)
}

#' Pipeline stage: refresh the districts' teacher FTE and enrollment
#'
#' Rewrites the profile file's CCD staffing columns ([fetch_ccd_teacher_fte()], by NCES id) and `CCD_Source`. A
#' failed fetch leaves them as they were.
#' @param district_ids The districts' NCES ids ([fetch_ccd_teacher_fte()]).
#' @param file The profile CSV inside the app folder.
#' @param config The state's config; the session's by default.
#' @param fetch The fetch, for tests: a function of `district_ids`.
#' @return The fetched table (zero rows when the fetch failed), invisibly.
#' @export
refresh_ccd_teacher_fte <- function(district_ids, file = "salarymap2.csv", config = current_state_config(),
                                    fetch = function(ids) fetch_ccd_teacher_fte(ids, fips = config$fips)) {
  if (is.null(config)) stop("refresh_ccd_teacher_fte(): no state config (set_state_config())")
  refresh_file_columns(
    file, paste0("CCD district teacher FTE (", config_label(config), ")"), fetch = function() fetch(district_ids),
    expected_cols = c("District", "Teachers_Total_FTE", "Enrollment", "CCD_Year"), by = "District",
    retrieved_source = "Urban CCD", replace = c("Teachers_Total_FTE", "Enrollment", "CCD_Year", "CCD_Source"),
    config = config,
    then = function(d) {
      d$CCD_Source <- "NCES Common Core of Data via Urban Institute Education Data Portal (educationdata.urban.org)"
      d
    })
}

#' Pipeline stage: refresh the districts' data coverage tier
#'
#' Sets the profile file's `Data_Coverage` from `tiers`; a district not in it is "Full". Runs every week: the tier is
#' a fact about the platform a district is read from, not something fetched.
#' @param tiers A data frame with `District` and `Data_Coverage`, one row per district that isn't "Full".
#' @param file The profile CSV inside the app folder.
#' @param config The state's config; the session's by default.
#' @param before A function of the file's frame applied first (Wyoming's fixed columns from its registry), or NULL.
#' @return The rewritten frame, invisibly.
#' @export
refresh_data_coverage <- function(tiers, file = "salarymap2.csv", config = current_state_config(), before = NULL) {
  if (is.null(config)) stop("refresh_data_coverage(): no state config (set_state_config())")
  stopifnot(c("District", "Data_Coverage") %in% names(tiers), is.null(before) || is.function(before))
  path <- file.path(config$app_dir, file)
  d <- utils::read.csv(path, fileEncoding = "UTF-8")
  if (!is.null(before)) d <- before(d)
  d <- dplyr::left_join(d[, setdiff(names(d), "Data_Coverage"), drop = FALSE], tiers, by = "District")
  d$Data_Coverage <- dplyr::coalesce(d$Data_Coverage, "Full")
  utils::write.csv(d, path, row.names = FALSE)
  invisible(d)
}
