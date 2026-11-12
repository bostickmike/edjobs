# A state's configuration: what package code needs to know about the state it
# is running for, as data. Package functions hold no state names, FIPS codes,
# file names or URLs (vignette("edjobs")); this is where those come in.
#
# Phase 4 (2026-11-11) starts it with the identity fields and the state's own
# reference tables. Later fields (spell rules, statewide-feed adapter,
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
#' @return A list of class `edjobs_state_config`.
#' @export
state_config <- function(state, fips, app_dir, reference_tables = list()) {
  stopifnot(is.character(state), length(state) == 1, nzchar(state),
            is.character(fips), grepl("^\\d{2}$", fips), is.character(app_dir), length(app_dir) == 1,
            is.list(reference_tables))
  clash <- intersect(names(reference_tables), names(REFERENCE_TABLE_RULES))
  if (length(clash) > 0) stop("Reference table(s) already shared in REFERENCE_TABLE_RULES: ", paste(clash, collapse = ", "))
  bad <- names(reference_tables)[!vapply(reference_tables, function(r)
    all(c("name", "required", "bounds", "year_col", "max_age") %in% names(r)), logical(1))]
  if (length(bad) > 0) stop("Reference table rules need name, required, bounds, year_col, max_age: ", paste(bad, collapse = ", "))
  structure(list(state = state, fips = fips, app_dir = app_dir, reference_tables = reference_tables),
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
