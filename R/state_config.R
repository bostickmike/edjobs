# A state's configuration: what package code needs to know about the state it
# is running for, as data. Package functions hold no state names, FIPS codes,
# file names or URLs (vignette("edjobs")); this is where those come in.
#
# Phase 4 (2026-11-11) starts it with the identity fields and the state's own
# reference tables, then (0.8.0) the statewide feed. Later fields (spell rules, feed adapter,
# registry paths, schemas) join as the forks move.

.edjobs_state <- new.env(parent = emptyenv())

#' A state's configuration
#'
#' Each state repository builds one in its `state_config.R` and sets it with
#' `set_state_config()` before running pipeline code.
#' @param state The state's name, as the dashboards show it ("Montana").
#' @param fips Two-digit state FIPS code, as a string ("30").
#' @param app_dir The folder holding the app and its data files.
#' @param reference_tables The state's own reference tables, in the shape of
#'   `REFERENCE_TABLE_RULES` (one entry per file name without `.csv`). A name
#'   already in `REFERENCE_TABLE_RULES` is an error: shared tables have one set
#'   of rules.
#' @param feed The state's statewide job feed, or NULL: a list with
#'   `short_name` ("OPI"), `label` (its `Posting_Source` value), `url` and
#'   `discrepancy_file` (the report of feed postings missing from direct
#'   scrapes). The drift report reads it.
#' @param drift_baseline_from The first run date whose archive counts toward
#'   the drift check's per-source baselines (`build_historical_counts()`):
#'   earlier archives predate fixes that changed the counts.
#' @param scraper_sources Where the state's sources are listed and how each
#'   is scraped: a list of `scraper_registry()` and `scraper_calls()`, in
#'   lookup order (`resolve_scraper_call()`, `build_source_url_lookup()`).
#' @param autofix Settings for the auto-fix issue: `scraper_files` (the
#'   state's scraper files, read as text), `fixtures_by` (`"name"` or
#'   `"tests"`, see `gather_autofix_evidence()`) and `fixture_example` (a path
#'   pattern for a new fixture, with `{platform}` and `{date}`).
#' @return A list of class `edjobs_state_config`.
#' @export
state_config <- function(state, fips, app_dir, reference_tables = list(), feed = NULL, drift_baseline_from = NULL,
                         scraper_sources = list(), autofix = list()) {
  stopifnot(is.character(state), length(state) == 1, nzchar(state),
            is.character(fips), grepl("^\\d{2}$", fips), is.character(app_dir), length(app_dir) == 1,
            is.list(reference_tables))
  clash <- intersect(names(reference_tables), names(REFERENCE_TABLE_RULES))
  if (length(clash) > 0) stop("Reference table(s) already shared in REFERENCE_TABLE_RULES: ", paste(clash, collapse = ", "))
  bad <- names(reference_tables)[!vapply(reference_tables, function(r)
    all(c("name", "required", "bounds", "year_col", "max_age") %in% names(r)), logical(1))]
  if (length(bad) > 0) stop("Reference table rules need name, required, bounds, year_col, max_age: ", paste(bad, collapse = ", "))
  if (!is.null(feed)) {
    miss <- setdiff(c("short_name", "label", "url", "discrepancy_file"), names(feed))
    if (length(miss) > 0) stop("feed needs ", paste(miss, collapse = ", "))
  }
  if (!is.list(scraper_sources) || !all(vapply(scraper_sources, inherits, logical(1), "edjobs_scraper_source"))) {
    stop("scraper_sources must be a list of scraper_registry() / scraper_calls()")
  }
  extra <- setdiff(names(autofix), c("scraper_files", "fixtures_by", "fixture_example"))
  if (!is.list(autofix) || length(extra) > 0) stop("autofix takes scraper_files, fixtures_by, fixture_example")
  if (!is.null(autofix$fixtures_by) && !autofix$fixtures_by %in% c("name", "tests")) stop("autofix$fixtures_by must be \"name\" or \"tests\"")
  structure(list(state = state, fips = fips, app_dir = app_dir, reference_tables = reference_tables, feed = feed,
                 scraper_sources = scraper_sources, autofix = autofix,
                 drift_baseline_from = if (is.null(drift_baseline_from)) NULL else as.Date(drift_baseline_from)),
            class = "edjobs_state_config")
}

#' Set and get the state configuration for this R session
#'
#' `set_state_config()` makes `config` the session's state; package code that
#' needs state data (`reference_table_rules()`) reads it with
#' `current_state_config()`, which is NULL until one is set.
#' @param config A `state_config()`, or NULL to clear it.
#' @return `set_state_config()`: the config, invisibly.
#' @export
set_state_config <- function(config) {
  if (!is.null(config) && !inherits(config, "edjobs_state_config")) stop("config must come from state_config()")
  assign("config", config, envir = .edjobs_state)
  invisible(config)
}

#' @rdname set_state_config
#' @export
current_state_config <- function() get0("config", envir = .edjobs_state, inherits = FALSE, ifnotfound = NULL)

#' @describeIn REFERENCE_TABLE_RULES Every reference table's rules for a
#'   state: the shared `REFERENCE_TABLE_RULES` plus the state's own
#'   (`state_config(reference_tables = )`).
#' @param config A `state_config()`; defaults to the session's.
#' @export
reference_table_rules <- function(config = current_state_config()) {
  c(REFERENCE_TABLE_RULES, if (!is.null(config)) config$reference_tables)
}
