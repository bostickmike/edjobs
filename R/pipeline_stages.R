# Pipeline stages (2026-11-17 package review, step 5): a chunk both states' weekly Rmd wrote out the same way, as one
# function taking the state's config. A state's Rmd calls the stage; what differs by state comes from
# state_config(). Slice 1 (0.47.0): the federal county context and the source record links. Slice 2 (0.48.0):
# refresh_file_columns(), the fetch-then-swap-columns step the context chunks repeat (IPEDS enrollment and its
# trend, FSA Pell share; the county context is now one of its uses). 0.49.0: `then` (a step after the join, such as
# add_saipe_range() or a source label) and `relationship`, for the K-12 SAIPE, finance and lunch blocks.

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
#' @param then A function of the joined frame, applied before the write (a derived column such as
#'   [add_saipe_range()], or a source label), or NULL.
#' @param relationship Passed to [dplyr::left_join()] ("many-to-one" where several keys share one row), or NULL.
#' @param config The state's config; the session's by default (its `app_dir`).
#' @return The fetched table (zero rows when the fetch failed), invisibly.
#' @export
refresh_file_columns <- function(file, source_name, fetch, expected_cols, by, retrieved_source = NULL,
                                 replace = setdiff(expected_cols, by), fail_message = NULL, then = NULL,
                                 relationship = NULL, config = current_state_config()) {
  if (is.null(config)) stop("refresh_file_columns(): no state config (set_state_config())")
  stopifnot(is.character(by), length(by) >= 1, by %in% expected_cols, is.null(then) || is.function(then))
  path <- file.path(config$app_dir, file)
  new <- safe_scrape(source_name, retrieved_source = retrieved_source, app_dir = config$app_dir, scrape_fn = fetch,
                     expected_cols = expected_cols)
  if (nrow(new) > 0) {
    old <- utils::read.csv(path, fileEncoding = "UTF-8")
    old <- old[, setdiff(names(old), replace), drop = FALSE]
    joined <- dplyr::left_join(old, new, by = by, relationship = relationship)
    if (!is.null(then)) joined <- then(joined)
    utils::write.csv(joined, path, row.names = FALSE)
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

#' A registry row's posting-source label
#'
#' The default `label` for [scrape_registry_platforms()]: the row's `Posting_Source` column when it has one, else
#' "<District> site (direct board)". A label is what the spells and the weekly checks call the board, so a district
#' keeps its label when its scraper changes (a one-off moved into a family reader).
#' @param row One registry row.
#' @return One string.
#' @export
registry_posting_source <- function(row) {
  v <- registry_text("Posting_Source")(row)
  if (is.na(v)) paste0(row$District, " site (direct board)") else v
}

#' Scrape the registry's direct boards, platform by platform
#'
#' The K-12 "direct board" stage (2026-11-17 package review, step 5, slice 4): every registry row on the given
#' platforms is scraped through its `scraper_spec()` ([scraper_registry()]'s specs or fallback, the same call the
#' live check and the repro script make), under [safe_scrape()] with the district as the source name, and its rows
#' put in the K-12 columns. A state lists its platforms; a district on one of them is a registry row, with no code.
#'
#' Each group's dates go through [standardize_date()] together, as the per-platform blocks this replaces did: the
#' parser picks its formats from the whole vector, so splitting or merging groups can change how an ambiguous date
#' reads.
#' @param registry The registry (a data frame with `District` and the platform column).
#' @param platforms The groups, in order: a list (or character vector) whose elements are platform names; an element
#'   with several names is one group (a family read by two platform modes). Names, when given, name the output.
#' @param source The [scraper_registry()] holding the specs, by default the session's first registry source.
#' @param label Function of a registry row giving the row's `Posting_Source`; [registry_posting_source()].
#' @param expected_cols What [safe_scrape()] returns on a failure.
#' @param session_factory Called (no arguments) for a chromote session, for specs with `session = TRUE`.
#' @param envir Where the state's scraper functions are, and the `safe_scrape()` used (the package's unless `envir`
#'   has its own, such as a test's stub).
#' @param log_path The scrape log, as [safe_scrape()].
#' @return A list of data frames, one per group, each with `title`, `date_posted` (a Date), `position` (NA),
#'   `location`, `url`, `District`, `Posting_Source`, and the four posted-pay columns when the scraper returned any.
#' @export
scrape_registry_platforms <- function(registry, platforms, source = NULL, label = registry_posting_source,
                                      expected_cols = c("Title", "Location", "Posted_Date", "Link"),
                                      session_factory = NULL, envir = parent.frame(), log_path = "scrape_log.csv") {
  if (is.null(source)) {
    regs <- Filter(function(s) identical(s$kind, "registry"), state_scraper_sources())
    if (length(regs) == 0) stop("scrape_registry_platforms(): no scraper_registry() in the state config; pass `source`")
    source <- regs[[1]]
  }
  stopifnot(is.data.frame(registry), inherits(source, "edjobs_scraper_source"), is.function(label))
  platforms <- as.list(platforms)
  pcol <- source$platform_col
  # safe_scrape() as the caller's environment sees it, as the per-platform blocks called it: a test that stubs
  # safe_scrape there (the state's pipeline dry run) reaches the stage too, instead of the live sites.
  run_safely <- get0("safe_scrape", envir = envir, mode = "function", ifnotfound = safe_scrape)
  out <- lapply(platforms, function(group) {
    rows <- registry[registry[[pcol]] %in% group, , drop = FALSE]
    parts <- lapply(seq_len(nrow(rows)), function(i) {
      row <- rows[i, , drop = FALSE]
      call <- scraper_call_for_row(source, row)
      if (is.null(call)) stop("scrape_registry_platforms(): no scraper spec for platform '", row[[pcol]], "' (",
                              row$District, ")")
      df <- run_safely(row$District,
                        scrape_fn = function() run_scraper_call(call, session_factory = session_factory, envir = envir),
                        expected_cols = expected_cols, log_path = log_path)
      if (nrow(df) > 0) {
        df$District <- row$District
        df$Posting_Source <- label(row)
      }
      df
    })
    registry_rows_to_k12(dplyr::bind_rows(parts))
  })
  if (!is.null(names(platforms))) names(out) <- names(platforms)
  out
}

# A group's scraped rows in the K-12 columns (the transmute every per-platform block wrote), dates standardized.
registry_rows_to_k12 <- function(df) {
  pay <- c("Pay_Min", "Pay_Max", "Pay_Unit", "Pay_Text")
  if (nrow(df) == 0) {
    out <- data.frame(title = character(0), date_posted = character(0), position = character(0),
                      location = character(0), url = character(0), District = character(0),
                      Posting_Source = character(0), stringsAsFactors = FALSE)
  } else {
    out <- data.frame(title = df$Title, date_posted = df$Posted_Date, position = NA_character_,
                      location = df$Location, url = df$Link, District = df$District,
                      Posting_Source = df$Posting_Source, stringsAsFactors = FALSE)
    if (any(pay %in% names(df))) {
      df <- ensure_posted_pay_columns(df)
      out[pay] <- df[pay]
    }
  }
  out$date_posted <- standardize_date(out$date_posted)
  out
}
