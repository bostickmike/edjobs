# Pipeline stages (2026-11-17 package review, step 5): a chunk both states' weekly Rmd wrote out the same way, as one
# function taking the state's config. A state's Rmd calls the stage; what differs by state comes from
# state_config(). The first stages are the ones that already used only package code: the federal county context and
# the source record links.

#' The county-context columns a refresh replaces
#'
#' What [fetch_census_county_context()] adds to a profile file. A refresh drops these first, so a re-run doesn't
#' leave `.x`/`.y` copies, and an older file's MOE columns don't linger beside a new estimate.
#' @format A character vector.
#' @export
COUNTY_CONTEXT_COLUMNS <- c("Median_Household_Income", "Median_Gross_Rent", "Population", "ACS_Year",
                            "Mining_Employment_Share", "Population_Change_Pct",
                            "Median_Household_Income_MOE", "Median_Gross_Rent_MOE", "Mining_Employment_Share_MOE")

#' Join county context into a profile file
#'
#' Drops [COUNTY_CONTEXT_COLUMNS] from `profile` and joins `context` by `County`: one county's figures go to every
#' row in it (sibling districts share their county's).
#' @param profile A data frame with a `County` column (salarymap2.csv's districts, salarymap.csv's colleges).
#' @param context [fetch_census_county_context()]'s table.
#' @return `profile` with the new context columns.
#' @export
join_county_context <- function(profile, context) {
  profile <- profile[, setdiff(names(profile), COUNTY_CONTEXT_COLUMNS), drop = FALSE]
  dplyr::left_join(profile, context, by = "County")
}

#' Pipeline stage: refresh a profile file's county context
#'
#' Fetches the Census ACS county context through [safe_scrape()] (logged as `source_name`, retrieval recorded as
#' "Census ACS") and rewrites `file` with it ([join_county_context()]). A failed fetch leaves the file as it was, so
#' last week's good values are never blanked.
#' @param file The profile CSV inside the app folder ("salarymap2.csv", "salarymap.csv").
#' @param source_name The scrape log's name for this fetch ("Census ACS county context (WY)").
#' @param config The state's config; the session's by default. Its `app_dir` holds `file`; its `county_suffix`
#'   is passed to [fetch_census_county_context()].
#' @param fetch The fetch, for tests.
#' @return The context table (zero rows when the fetch failed), invisibly, for later stages that join it again.
#' @export
refresh_county_context <- function(file, source_name, config = current_state_config(),
                                   fetch = function() fetch_census_county_context(county_suffix = config$county_suffix)) {
  if (is.null(config)) stop("refresh_county_context(): no state config (set_state_config())")
  path <- file.path(config$app_dir, file)
  context <- safe_scrape(source_name, retrieved_source = "Census ACS", app_dir = config$app_dir, scrape_fn = fetch,
                         expected_cols = c("County", "Median_Household_Income", "Median_Gross_Rent", "Population",
                                           "ACS_Year", "Mining_Employment_Share", "Population_Change_Pct"))
  if (nrow(context) > 0) {
    profile <- utils::read.csv(path, fileEncoding = "UTF-8")
    utils::write.csv(join_county_context(profile, context), path, row.names = FALSE)
  } else {
    message("Census ACS county context fetch failed -- leaving ", file, " untouched this run.")
  }
  invisible(context)
}

#' Pipeline stage: add source record links to the profile files
#'
#' Rewrites each of `files` (inside the app folder) with [add_source_record_links()]; a file that doesn't exist is
#' skipped. Run last, after every stage that rewrites these files: the links are rebuilt from the ids each time.
#' @param files The profile CSVs.
#' @param config The state's config; the session's by default (its `app_dir`).
#' @return The paths rewritten, invisibly.
#' @export
refresh_source_record_links <- function(files = c("salarymap2.csv", "salarymap.csv"), config = current_state_config()) {
  if (is.null(config)) stop("refresh_source_record_links(): no state config (set_state_config())")
  paths <- file.path(config$app_dir, files)
  paths <- paths[file.exists(paths)]
  for (p in paths) utils::write.csv(add_source_record_links(utils::read.csv(p, fileEncoding = "UTF-8")), p, row.names = FALSE)
  invisible(paths)
}
