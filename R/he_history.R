# The higher-ed stage after the scrapes (review step 5, slice D, 0.97.0): frames to one table, the rows the dashboard
# shows, and the accumulated history files, built one way by each state's weekly run and its from-archive rebuild.
# What differs by state stays with the caller: placing postings on the map (place_he_postings()'s tables), a college's
# own salary book, the institution spellings (canonicalize_he_institution()).

#' The columns of `facultydata.csv`
#'
#' The accumulated faculty history predates posted pay, pay estimates, closing dates and map places, so those columns
#' stay out of it (`append_weekly_rows()` refuses a changed column set).
#' @export
HE_FACULTY_COLUMNS <- c("Title", "Location", "Posted_Date", "Institution", "Link", "Archive_Date", "Job_Type", "Category")

#' One run's scraped college frames as one table
#'
#' Dates become character per frame (`bind_rows()` won't mix a Date with a character one), `Close_Date` and the posted-pay columns
#' are added where a scraper has none, and `Archive_Date` is the run date.
#' @param frames A list of scraped frames with `Institution` ([scrape_registry_platforms()]`(shape = "raw")`), or one data frame.
#' @param run_date The run date.
#' @return A data frame: `Title`, `Location`, `Posted_Date`, `Institution`, `Link`, `Close_Date`, the posted-pay columns,
#'   `Page_Places` when a scraper reads it, `Archive_Date`.
#' @export
combine_he_frames <- function(frames, run_date = Sys.Date()) {
  if (is.data.frame(frames)) frames <- list(frames)
  out <- lapply(unname(frames), function(df) {
    df$Posted_Date <- as.character(df$Posted_Date)
    df$Close_Date <- if ("Close_Date" %in% names(df)) as.character(df$Close_Date) else rep(NA_character_, nrow(df))
    ensure_posted_pay_columns(df)
  })
  out <- dplyr::bind_rows(out) %>%
    dplyr::select(dplyr::all_of(c("Title", "Location", "Posted_Date", "Institution", "Link", "Close_Date", POSTED_PAY_COLUMNS)),
                  dplyr::any_of("Page_Places"))
  out$Archive_Date <- rep(run_date, nrow(out))
  out
}

#' The higher-ed rows the dashboard shows, from one run's archived table
#'
#' Non-postings out (`is_placeholder_posting()`), implausible posted pay blanked (`drop_implausible_pay()`), and, for
#' postings that state no pay, the institution's IPEDS average for the posting's staff category
#' (`attach_ipeds_staff_estimates()`, from the previous run's `ipeds_staff_salaries.csv`; skipped when there is none).
#' Call after the raw archive is written, so the archive stays as scraped.
#' @param hedata The archived table.
#' @param app_dir The dashboard folder (`ipeds_staff_salaries.csv`, and `salarymap.csv`'s `Name` and `IPEDS_UNITID`).
#' @param canonicalize The state's institution-name function, for the `salarymap.csv` match only.
#' @return A data frame.
#' @export
finish_he_rows <- function(hedata, app_dir = current_state_config()$app_dir, canonicalize = identity) {
  stopifnot(is.character(app_dir), length(app_dir) == 1, is.function(canonicalize))
  out <- hedata %>% dplyr::filter(!is_placeholder_posting(Title)) %>% drop_implausible_pay()
  staff_path <- file.path(app_dir, "ipeds_staff_salaries.csv")
  if (!file.exists(staff_path)) return(out)
  ids <- utils::read.csv(file.path(app_dir, "salarymap.csv"))[, c("Name", "IPEDS_UNITID")]
  attach_ipeds_staff_estimates(out, utils::read.csv(staff_path, stringsAsFactors = FALSE, colClasses = c(Year = "character")),
                               ids$IPEDS_UNITID[match(canonicalize(out$Institution), ids$Name)])
}

#' The higher-ed history tables from counted postings
#'
#' As [build_k12_history()]: one run's rows give that run's, every run's give the full tables. Institution names must
#' already be canonical. Every faculty posting (full-time and adjunct) is in `facultydata`; `Job_Type` tells them apart.
#' @param hedata One or more runs' postings, with `Archive_Date`.
#' @return A list: `facultydata` ([HE_FACULTY_COLUMNS]), `he_institution_weekly_totals`, `he_jobtype_weekly_totals`
#'   (every posting), `allsum_he` (faculty by category, run, institution and job type, with a `Total` that is the sum of
#'   the institutions) and `allnow_he` (the latest run's, with `Sum`).
#' @export
build_he_history <- function(hedata) {
  cat_rows <- hedata %>% dplyr::mutate(Job_Type = classify_he_job_type(Title))
  faculty <- cat_rows %>%
    dplyr::filter(Job_Type %in% c("Instructor/Teacher/Faculty", "Adjunct/Part-Time Faculty")) %>%
    dplyr::mutate(Category = classify_he_faculty_category(Title)) %>%
    dplyr::select(dplyr::all_of(HE_FACULTY_COLUMNS)) %>%
    as.data.frame()
  by_inst <- faculty %>%
    dplyr::group_by(Category, Archive_Date, Institution, Job_Type) %>%
    dplyr::summarize(sum = dplyr::n(), .groups = "drop")
  total <- faculty %>%
    dplyr::group_by(Category, Archive_Date, Job_Type) %>%
    dplyr::summarize(sum = dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(Institution = "Total") %>%
    dplyr::select(Category, Archive_Date, Institution, Job_Type, sum)
  check_total_matches_parts(total, by_inst, group_cols = c("Category", "Archive_Date", "Job_Type"))
  latest <- function(df) if (nrow(df) == 0) df else df[df$Archive_Date == max(faculty$Archive_Date), , drop = FALSE]
  now <- function(df) latest(df) %>% dplyr::transmute(Category, Job_Type, Sum = sum, Institution)
  list(
    facultydata = faculty,
    he_institution_weekly_totals = posting_run_totals(cat_rows, "Institution", "Job_Type", "Title", "college"),
    he_jobtype_weekly_totals = jobtype_run_totals(cat_rows, "Job_Type", "Title", "college"),
    allsum_he = dplyr::bind_rows(total, by_inst),
    allnow_he = dplyr::bind_rows(now(total), now(by_inst))
  )
}

#' Append one run's higher-ed history to the dashboard's files
#'
#' [build_he_history()] on one run, appended ([append_weekly_rows()]) to `facultydata.csv`,
#' `he_institution_weekly_totals.csv`, `he_jobtype_weekly_totals.csv` and `allsum_he.csv`; `allnow_he.csv` is this run's
#' alone. Every file is checked before any is written.
#' @param hedata One run's postings, institution names canonical.
#' @param app_dir The dashboard folder; defaults to `state_config(app_dir = )`.
#' @return [build_he_history()]'s list for this run, invisibly.
#' @export
write_he_history <- function(hedata, app_dir = current_state_config()$app_dir) {
  stopifnot(is.character(app_dir), length(app_dir) == 1)
  run <- build_he_history(hedata)
  files <- c(facultydata = "facultydata.csv", he_institution_weekly_totals = "he_institution_weekly_totals.csv",
             he_jobtype_weekly_totals = "he_jobtype_weekly_totals.csv", allsum_he = "allsum_he.csv")
  out <- lapply(names(files), function(n) append_weekly_rows(file.path(app_dir, files[[n]]), run[[n]]))
  for (i in seq_along(files)) utils::write.csv(out[[i]], file.path(app_dir, files[[i]]), row.names = FALSE)
  utils::write.csv(run$allnow_he, file.path(app_dir, "allnow_he.csv"), row.names = FALSE)
  invisible(run)
}
