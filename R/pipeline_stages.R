# Pipeline stages (2026-11-17 package review, step 5): a chunk both states' weekly Rmd wrote out the same way, as one
# function taking the state's config. A state's Rmd calls the stage; what differs by state comes from
# state_config(). Slice 1 (0.47.0): the federal county context and the source record links. Slice 2 (0.48.0):
# refresh_file_columns(), the fetch-then-swap-columns step the context chunks repeat (IPEDS enrollment and its
# trend, FSA Pell share; the county context is now one of its uses).

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

#' Pipeline stage: refresh some of a file's columns from one fetch
#'
#' The step most of the weekly pipeline's context chunks repeat: run `fetch` through [safe_scrape()] (logged as
#' `source_name`; its retrieval recorded under `retrieved_source`), and if it returned rows, drop `replace` from
#' `file` and join the fetch's columns by `by`. A failed or empty fetch leaves the file as it was, so last week's good
#' values are never blanked.
#' @param file The CSV inside the app folder ("salarymap.csv").
#' @param source_name The scrape log's name for this fetch ("IPEDS fall enrollment FTE (WY)").
#' @param fetch A function of no arguments returning the new columns plus `by`.
#' @param expected_cols The columns `fetch` must return ([safe_scrape()]).
#' @param by The join key ("Name", "District", "County").
#' @param retrieved_source The source-retrieved name ("Urban IPEDS"), or NULL.
#' @param replace The file's columns this fetch replaces: `expected_cols` without `by` unless given.
#' @param fail_message The message on a failed fetch; a default naming the file and the columns.
#' @param config The state's config; the session's by default (its `app_dir`).
#' @return The fetched table (zero rows when the fetch failed), invisibly.
#' @export
refresh_file_columns <- function(file, source_name, fetch, expected_cols, by, retrieved_source = NULL,
                                 replace = setdiff(expected_cols, by), fail_message = NULL,
                                 config = current_state_config()) {
  if (is.null(config)) stop("refresh_file_columns(): no state config (set_state_config())")
  stopifnot(is.character(by), length(by) >= 1, by %in% expected_cols)
  path <- file.path(config$app_dir, file)
  new <- safe_scrape(source_name, retrieved_source = retrieved_source, app_dir = config$app_dir, scrape_fn = fetch,
                     expected_cols = expected_cols)
  if (nrow(new) > 0) {
    old <- utils::read.csv(path, fileEncoding = "UTF-8")
    old <- old[, setdiff(names(old), replace), drop = FALSE]
    utils::write.csv(dplyr::left_join(old, new, by = by), path, row.names = FALSE)
  } else {
    message(if (is.null(fail_message)) {
      paste0(source_name, " fetch failed -- leaving ", file, "'s ", paste(replace, collapse = ", "), " untouched this run.")
    } else fail_message)
  }
  invisible(new)
}

#' Pipeline stage: refresh a profile file's county context
#'
#' Fetches the Census ACS county context and rewrites `file` with it: [refresh_file_columns()] by `County`, replacing
#' [COUNTY_CONTEXT_COLUMNS] (the same result as [join_county_context()]). A failed fetch leaves the file as it was.
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
  refresh_file_columns(file, source_name, fetch = fetch, by = "County", retrieved_source = "Census ACS",
                       expected_cols = c("County", "Median_Household_Income", "Median_Gross_Rent", "Population",
                                         "ACS_Year", "Mining_Employment_Share", "Population_Change_Pct"),
                       replace = COUNTY_CONTEXT_COLUMNS,
                       fail_message = paste0("Census ACS county context fetch failed -- leaving ", file, " untouched this run."),
                       config = config)
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
