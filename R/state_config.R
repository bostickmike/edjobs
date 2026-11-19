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
#'   scrapes). The drift report reads it. Optional: `signature` (words the feed's page must show for the pre-flight
#'   canary, any one; default the short name and the state's name) and `cainfo` (a CA bundle file for a feed server
#'   whose certificate chain the runner's store lacks; [run_canaries()]).
#' @param drift_baseline_from The first run date whose archive counts toward
#'   the drift check's per-source baselines (`build_historical_counts()`):
#'   earlier archives predate fixes that changed the counts.
#' @param scraper_sources Where the state's sources are listed and how each
#'   is scraped: a list of `scraper_registry()` and `scraper_calls()`, in
#'   lookup order (`resolve_scraper_call()`, `build_source_url_lookup()`).
#' @param timezone The state's time zone (an `OlsonNames()` entry, "America/Denver"): the day a check counts for
#'   and a JSON-LD date-time's local day are in the boards' own time, not the runner's UTC (`state_timezone()`).
#'   NULL is allowed for code that never needs it.
#' @param oews_areas The state's own BLS OEWS area rows, when the package's
#'   `inst/extdata/oews_area_definitions.csv` doesn't have them: a data frame
#'   or CSV path with that file's columns (`oews_area_definitions()`). NULL
#'   for a state the package already knows.
#' @param county_suffix What the Census county names end with that the state's files leave off (", Montana":
#'   the files say "Gallatin County"), passed to [fetch_census_county_context()]; NULL keeps Census's name
#'   ("Albany County, Wyoming").
#' @param k12_source_names How the weekly checks name the source of each K-12 row (drift, pay coverage, title
#'   quality): a function of an archive or `combinedclean.csv` data frame returning one name per row, or NULL to
#'   leave that archive out (one written before the columns the naming needs). NULL uses `District` as is.
#' @param drift_min_mean The drift check's floor: sources averaging fewer postings a week aren't judged on a quiet
#'   week (`flag_drift(min_mean_count = )`).
#' @param direct_registry The registry the drift report's statewide-feed section counts as "registered
#'   districts": a function of no arguments returning it, or NULL to read `k12_district_registry.csv`.
#' @param saipe_gaps Districts in `salarymap2.csv` with no Census SAIPE child poverty rate, by design (a BOCES, a
#'   state institution's school): the Census check ([check_census_step()]) expects one for every other district.
#' @param autofix Settings for the auto-fix issue: `scraper_files` (the
#'   state's scraper files, read as text), `fixtures_by` (`"name"` or
#'   `"tests"`, see `gather_autofix_evidence()`) and `fixture_example` (a path
#'   pattern for a new fixture, with `{platform}` and `{date}`).
#' @return A list of class `edjobs_state_config`.
#' @export
state_config <- function(state, fips, app_dir, reference_tables = list(), feed = NULL, drift_baseline_from = NULL,
                         scraper_sources = list(), autofix = list(), timezone = NULL, oews_areas = NULL,
                         county_suffix = NULL, k12_source_names = NULL, drift_min_mean = 3, direct_registry = NULL,
                         saipe_gaps = character(0)) {
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
  if (!is.null(timezone) && !(is.character(timezone) && length(timezone) == 1 && timezone %in% OlsonNames())) {
    stop("timezone must be one OlsonNames() entry (an Area/City name)")
  }
  if (!is.null(oews_areas) && !is.data.frame(oews_areas) && !(is.character(oews_areas) && length(oews_areas) == 1)) {
    stop("oews_areas must be a data frame or a CSV path")
  }
  if (!is.null(county_suffix) && !(is.character(county_suffix) && length(county_suffix) == 1)) {
    stop("county_suffix must be one string or NULL")
  }
  if (!is.null(k12_source_names) && !is.function(k12_source_names)) stop("k12_source_names must be a function or NULL")
  if (!is.null(direct_registry) && !is.function(direct_registry)) stop("direct_registry must be a function or NULL")
  if (!(is.numeric(drift_min_mean) && length(drift_min_mean) == 1 && drift_min_mean >= 0)) stop("drift_min_mean must be one number >= 0")
  if (!is.character(saipe_gaps)) stop("saipe_gaps must be a character vector of district names")
  structure(list(state = state, fips = fips, timezone = timezone, oews_areas = oews_areas, county_suffix = county_suffix,
                 app_dir = app_dir, reference_tables = reference_tables, feed = feed,
                 scraper_sources = scraper_sources, autofix = autofix,
                 k12_source_names = k12_source_names, drift_min_mean = drift_min_mean, direct_registry = direct_registry,
                 saipe_gaps = saipe_gaps,
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

#' The current state's time zone
#'
#' `current_state_config()$timezone`, with a clear error when no state config (or no time zone) is set. The default
#' `tz` of `presence_today()` and of the JSON-LD date reader.
#' @export
state_timezone <- function() {
  tz <- current_state_config()$timezone
  if (is.null(tz)) stop("No state time zone set: give state_config(timezone = ) in the state's state_config.R, or pass tz")
  tz
}

#' The current state's FIPS code
#'
#' `current_state_config()$fips`, with a clear error when no state config is
#' set. The default `fips` for the federal-data fetchers.
#' @keywords internal
state_fips <- function() {
  config <- current_state_config()
  if (is.null(config)) stop("No state config set: source the state's state_config.R (set_state_config()) or pass fips")
  config$fips
}
