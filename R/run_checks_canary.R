# The weekly run's pre-flight canary and the rebuild-parity report as package functions (2026-11-17 package review,
# step 6, slice 3). The states' canary_api_check.R differed only in the FIPS code, the statewide feed (URL, name, the
# words its page must show, Montana's certificate bundle) and the run's own name for them, all state_config() data.

#' Pre-flight canary checks
#'
#' Tests the core external APIs for reachability and basic payload shape in a few seconds before the full scrape, so
#' an upstream outage, a retired endpoint or a restructured payload doesn't waste a 20-minute run.
#' `check_endpoint()` fetches one URL and runs a validator; the `validate_*()` functions are the validators (each
#' takes an httr2 response and returns `list(ok = TRUE)` or `list(ok = FALSE, error = )`). `run_canaries()` runs the
#' IPEDS, CCD, statewide feed and (with `CENSUS_API_KEY`) Census checks; `canary_step()` prints them as the weekly run
#' does and stops only in strict mode.
#' @param name,url,validator,timeout_secs The check's label, URL, validator and timeout.
#' @param cainfo A CA bundle file for a server whose chain the runner's store lacks, or NULL.
#' @return `check_endpoint()`: a list with `ok`, `msg`, `elapsed`, `name`.
#' @export
check_endpoint <- function(name, url, validator, timeout_secs = 10, cainfo = NULL) {
  t0 <- proc.time()
  res <- tryCatch({
    req <- httr2::req_timeout(httr2::request(url), timeout_secs)
    if (!is.null(cainfo) && file.exists(cainfo)) req <- httr2::req_options(req, cainfo = normalizePath(cainfo))
    resp <- httr2::req_perform(req)
    status <- httr2::resp_status(resp)
    if (status != 200) {
      list(ok = FALSE, msg = sprintf("HTTP %d", status), elapsed = (proc.time() - t0)[["elapsed"]])
    } else {
      val_res <- validator(resp)
      if (isTRUE(val_res$ok)) {
        list(ok = TRUE, msg = "OK", elapsed = (proc.time() - t0)[["elapsed"]])
      } else {
        list(ok = FALSE, msg = val_res$error, elapsed = (proc.time() - t0)[["elapsed"]])
      }
    }
  }, error = function(e) {
    list(ok = FALSE, msg = conditionMessage(e), elapsed = (proc.time() - t0)[["elapsed"]])
  })
  res$name <- name
  res
}

#' @rdname check_endpoint
#' @param resp An httr2 response.
#' @return `list(ok = TRUE)`, or `list(ok = FALSE, error = <why>)`.
#' @export
validate_urban_ipeds <- function(resp) {
  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  if (!"results" %in% names(body) || length(body$results) == 0) {
    return(list(ok = FALSE, error = "missing 'results' in payload"))
  }
  df <- as.data.frame(body$results)
  if (!all(c("unitid", "year") %in% names(df))) {
    return(list(ok = FALSE, error = "expected keys 'unitid' and 'year' missing"))
  }
  list(ok = TRUE)
}

#' @rdname check_endpoint
#' @return `list(ok = TRUE)`, or `list(ok = FALSE, error = <why>)`.
#' @export
validate_urban_ccd <- function(resp) {
  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  if (!"results" %in% names(body) || length(body$results) == 0) {
    return(list(ok = FALSE, error = "missing 'results' in payload"))
  }
  df <- as.data.frame(body$results)
  if (!all(c("leaid", "year") %in% names(df))) {
    return(list(ok = FALSE, error = "expected keys 'leaid' and 'year' missing"))
  }
  list(ok = TRUE)
}

#' @rdname check_endpoint
#' @return `list(ok = TRUE)`, or `list(ok = FALSE, error = <why>)`.
#' @export
validate_census <- function(resp) {
  json <- httr2::resp_body_json(resp)
  if (!is.list(json) || length(json) < 2) {
    return(list(ok = FALSE, error = "Census response not tabular list matrix"))
  }
  list(ok = TRUE)
}

#' @rdname check_endpoint
#' @param signature Words the feed's page must show (any one, case ignored).
#' @param short_name The feed's short name, for the error message.
#' @return `list(ok = TRUE)`, or `list(ok = FALSE, error = <why>)`.
#' @export
validate_feed_page <- function(resp, signature, short_name) {
  html <- httr2::resp_body_string(resp)
  if (!any(vapply(signature, function(w) grepl(w, html, ignore.case = TRUE), logical(1)))) {
    return(list(ok = FALSE, error = sprintf("response did not match the %s page's signature (%s)", short_name,
                                            paste(signature, collapse = " / "))))
  }
  list(ok = TRUE)
}

#' @rdname check_endpoint
#' @param config A `state_config()`; defaults to the session's. Its `fips` and `feed` (`url`, `short_name`, and
#'   optionally `signature`, default the short name and the state's name, and `cainfo`) say what to check.
#' @return `run_canaries()`: the list of `check_endpoint()` results.
#' @export
run_canaries <- function(config = current_state_config()) {
  if (is.null(config)) stop("No state config set: source the state's state_config.R (set_state_config())", call. = FALSE)
  fips <- config$fips
  checks <- list()
  ipeds_url <- sprintf("https://educationdata.urban.org/api/v1/college-university/ipeds/salaries-instructional-staff/2024/?fips=%s&limit=1", fips)
  checks[[1]] <- check_endpoint("Urban Institute IPEDS Salary", ipeds_url, validate_urban_ipeds)
  ccd_url <- sprintf("https://educationdata.urban.org/api/v1/school-districts/ccd/directory/2023/?fips=%s&limit=1", fips)
  checks[[2]] <- check_endpoint("Urban Institute CCD Directory", ccd_url, validate_urban_ccd)
  feed <- config$feed
  if (!is.null(feed)) {
    signature <- if (is.null(feed$signature)) c(feed$short_name, config$state) else feed$signature
    checks[[length(checks) + 1]] <- check_endpoint(
      paste(feed$short_name, "Statewide Feed"), feed$url,
      function(resp) validate_feed_page(resp, signature, feed$short_name), cainfo = feed$cainfo)
  }
  census_key <- Sys.getenv("CENSUS_API_KEY")
  if (nzchar(census_key)) {
    census_url <- sprintf("https://api.census.gov/data/2022/acs/acs5?get=NAME,B19013_001E&for=county:001&in=state:%s&key=%s", fips, census_key)
    checks[[length(checks) + 1]] <- check_endpoint("Census Bureau ACS", census_url, validate_census)
  }
  checks
}

#' @rdname check_endpoint
#' @param strict Stop (non-zero exit) when a check fails; otherwise print a warning line and carry on.
#' @return `canary_step()`: the results, invisibly.
#' @export
canary_step <- function(strict = "--strict" %in% commandArgs(trailingOnly = TRUE), config = current_state_config()) {
  cat("Running Pre-Flight API Canary Checks...\n")
  results <- run_canaries(config)
  all_ok <- TRUE
  for (r in results) {
    status_tag <- if (r$ok) "[OK]  " else "[FAIL]"
    cat(sprintf("  %s %-32s (%.2fs): %s\n", status_tag, r$name, r$elapsed, r$msg))
    if (!r$ok) all_ok <- FALSE
  }
  if (!all_ok && strict) {
    stop("API Canary checks failed in strict mode.", call. = FALSE)
  } else if (!all_ok) {
    cat("Warning: One or more canary checks failed (non-strict mode; continuing).\n")
  } else {
    cat("All pre-flight API canary checks passed.\n")
  }
  invisible(results)
}

#' Report whether a rebuild from the archives reproduces the committed files
#'
#' The second half of the weekly rebuild-parity check (classification safeguards item 12): the state rebuilds its
#' K-12 and HE datasets from the raw archives in memory (its `scripts/rebuild_*_history_from_archive.R`), and this
#' compares each with the committed file, ignoring row order ([rebuild_parity_row()]). Non-blocking: differences go
#' to the drift report and `out_csv`.
#' @param k12,he The rebuilt K-12 and HE results (`rebuild_k12_history_from_archive()`,
#'   `rebuild_he_history_from_archive()`).
#' @param app_dir The app folder; defaults to the session's `state_config()`.
#' @param out_csv,report_path Where the table and the drift-report section go.
#' @return The parity table, invisibly.
#' @export
report_rebuild_parity <- function(k12, he, app_dir = NULL, out_csv = "/tmp/rebuild_parity.csv",
                                  report_path = "/tmp/drift_report.md") {
  app_dir <- run_app_dir(app_dir)
  pairs <- list("combinedclean.csv" = k12$combinedclean, "k12jobanalysis.csv" = k12$k12jobs,
                "allsum.csv" = k12$allsum, "allnow.csv" = k12$allnow,
                "k12_district_weekly_totals.csv" = k12$k12_district_weekly_totals,
                "facultydata.csv" = he$facultydata, "allsum_he.csv" = he$allsum_he, "allnow_he.csv" = he$allnow_he,
                "he_institution_weekly_totals.csv" = he$he_institution_weekly_totals,
                "k12_jobtype_weekly_totals.csv" = k12$k12_jobtype_weekly_totals,
                "he_jobtype_weekly_totals.csv" = he$he_jobtype_weekly_totals)
  parity <- dplyr::bind_rows(lapply(names(pairs), function(f) {
    rebuild_parity_row(f, utils::read.csv(file.path(app_dir, f), colClasses = "character", check.names = FALSE), pairs[[f]])
  }))
  utils::write.csv(parity, out_csv, row.names = FALSE)
  bad <- parity[nzchar(parity$Missing_Cols) | nzchar(parity$Extra_Cols) | parity$Only_Committed > 0 | parity$Only_Rebuilt > 0, ]
  print(parity[, c("File", "Committed_Rows", "Rebuilt_Rows", "Only_Committed", "Only_Rebuilt")], row.names = FALSE)
  if (nrow(bad) == 0) {
    cat("Rebuild parity: all", nrow(parity), "files reproduced from the archives.\n")
  } else {
    cat("Rebuild parity: ", nrow(bad), " file(s) differ.\n", sep = "")
    lines <- c("### Rebuild from the archives no longer reproduces the committed data", "",
               "`scripts/rebuild_*_history_from_archive.R` and the weekly pipeline have drifted apart. Don't run a rebuild or `scripts/reclassify_committed_data.R` into the repo until this is understood (details in this run's log).", "",
               "| File | Committed rows | Rebuilt rows | Rows only committed | Rows only rebuilt | Columns missing / extra |",
               "|---|---|---|---|---|---|",
               sprintf("| `%s` | %d | %d | %d | %d | %s |", bad$File, bad$Committed_Rows, bad$Rebuilt_Rows, bad$Only_Committed,
                       bad$Only_Rebuilt, gsub("\\|", "/", paste(bad$Missing_Cols, "/", bad$Extra_Cols))))
    for (i in seq_len(nrow(bad))) if (nzchar(bad$Example[i])) cat(bad$File[i], "e.g.", substr(bad$Example[i], 1, 400), "\n")
    append_drift_report(lines, report_path)
  }
  invisible(parity)
}
