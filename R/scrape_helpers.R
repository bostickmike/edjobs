# Shared scraping-resilience helpers for both states' weekly pipelines (edjobs).
#
# Every per-institution/per-district scraper chunk previously called its
# scrape function bare: if a site's HTML changed, a page timed out, or a
# selector stopped matching, that uncaught error halted the ENTIRE weekly
# knit -- including the final munge chunks that write the CSVs the live app
# reads -- so a problem with one source silently prevented that week's data
# update for every other source too. There was also no persisted record of
# what each run actually scraped, so a source quietly returning 0 rows (a
# broken selector, not a genuine empty week) looked identical to a real
# empty week with nothing to distinguish them after the fact -- this is
# exactly how the Eastern Wyoming CC name typo went unnoticed for 17 months.
#
# safe_scrape() wraps a scrape function so a failure degrades to "this one
# source is missing from this week's data" instead of "nothing updated this
# week," and logs every run's outcome (ok/empty/error, row count, message)
# to scrape_log.csv so failures and selector drift are visible without
# having to notice a downstream count looks off.

#' Perform with retry
#'
#' Every fetch_*() function across this project's scraper files ends its
#' httr2 pipe with this instead of a bare req_perform() -- a single 429/503
#' against a shared multi-tenant host (Applitrack, SchoolSpring, ...) or a
#' transient blip against any other live source would otherwise drop that
#' whole source for the week with no recourse (confirmed real: Lincoln
#' County SD1 and Sublette County SD9 both logged a bare "HTTP 500 Internal
#' Server Error" on 2026-08-11 with no retry available). httr2's
#' req_retry() retries 429/503 and honors a Retry-After header by default; a
#' persistent failure (a genuinely broken/migrated tenant, a 404, a real
#' 500) still surfaces as an error after the retries, exactly as before --
#' safe_scrape() logs it the same as always. Takes and returns the same
#' shapes as req_perform() (path included, for fetch_google_drive_pdf()'s
#' streamed-to-disk download in salary_scrapers.R), so it drops into any
#' existing `request(...) \%>\% ... \%>\% req_perform()` pipe as a straight
#' substitution for the last step.
#' 
#' is_transient overrides which responses count as retryable (NULL keeps
#' httr2's 429/503 default) -- see is_transient_gateway_error() below.
#' @param req See the description.
#' @param max_tries See the description.
#' @param path See the description.
#' @param is_transient See the description.
#' @export
perform_with_retry <- function(req, max_tries = 3, path = NULL, is_transient = NULL) {
  req <- httr2::req_retry(req, max_tries = max_tries, backoff = function(i) 2^i,
                          is_transient = is_transient)
  httr2::req_perform(req, path = path)
}

#' Is transient gateway error
#'
#' httr2's default retries only 429/503, so a 502/504 or a Cloudflare
#' 520-524 (origin unreachable/timed out) fails on the very first try. Ported
#' from Montana, where Roberts Public School lost 2026-09-22 and 09-29 to a
#' single 520/502 while its page was up. No Wyoming scraper uses it yet --
#' pass it for a source whose scrape_log errors are gateway 5xx.
#' @param resp See the description.
#' @export
is_transient_gateway_error <- function(resp) {
  httr2::resp_status(resp) %in% c(429, 502, 503, 504, 520:524)
}

#' Fetch json paginated
#'
#' Follows a JSON API's pagination link (defaulting to `resp[["next"]]`) until
#' exhausted, collecting records into a single consolidated data frame. Uses
#' httr2::resp_body_json(simplifyVector = TRUE) so that JSON null values across
#' mixed or optional fields (e.g. Urban Institute IPEDS/CCD directory/salary
#' endpoints) are converted into NA consistently without failing per-record
#' as.data.frame() calls.
#' @param url See the description.
#' @param results_field See the description.
#' @param next_field See the description.
#' @param max_pages See the description.
#' @param req_modifier See the description.
#' @export
fetch_json_paginated <- function(url,
                                 results_field = "results",
                                 next_field = "next",
                                 max_pages = NULL,
                                 req_modifier = identity) {
  pages <- list()
  page_count <- 0L

  while (!is.null(url) && nzchar(url)) {
    if (!is.null(max_pages) && page_count >= max_pages) break

    req <- req_modifier(httr2::request(url))
    resp <- req |> perform_with_retry() |> httr2::resp_body_json(simplifyVector = TRUE)
    res <- resp[[results_field]]

    if (is.data.frame(res) && nrow(res) > 0) {
      pages[[length(pages) + 1]] <- res
    } else if (is.list(res) && length(res) > 0) {
      pages[[length(pages) + 1]] <- as.data.frame(res)
    }

    page_count <- page_count + 1L
    url <- resp[[next_field]]
  }

  if (length(pages) == 0) return(data.frame())
  dplyr::bind_rows(pages)
}

#' Empty df
#'
#' Build a zero-row data frame with the given column names, so a failed or
#' empty scrape can still be bind_rows()'d/rbind()'d with everything else
#' without special-casing a NULL result at every call site.
#' @param cols See the description.
#' @export
empty_df <- function(cols) {
  as.data.frame(
    stats::setNames(rep(list(character(0)), length(cols)), cols),
    stringsAsFactors = FALSE
  )
}

#' Log scrape result
#'
#' Append one row to the scrape run log, creating it with a header if it
#' doesn't exist yet.
#' @param source_name See the description.
#' @param status See the description.
#' @param n_rows See the description.
#' @param error_message See the description.
#' @param log_path See the description.
#' @export
log_scrape_result <- function(source_name, status, n_rows,
                               error_message = NA_character_,
                               log_path = "scrape_log.csv") {
  entry <- data.frame(
    timestamp = as.character(Sys.time()),
    source = source_name,
    status = status,
    n_rows = n_rows,
    error_message = error_message,
    stringsAsFactors = FALSE
  )
  utils::write.table(
    entry,
    file = log_path,
    sep = ",",
    row.names = FALSE,
    col.names = !file.exists(log_path),
    append = file.exists(log_path),
    # Quotes inside an error message are doubled (CSV standard). write.table's
    # default backslash-escapes them, which read.csv can't parse: every row
    # after such a message was lost (MT 2026-11-03: all HE sources).
    qmethod = "double"
  )
  invisible(entry)
}

#' Safe scrape
#'
#' Run scrape_fn() under tryCatch, log the outcome, and always return a data
#' frame with `expected_cols` -- the real scraped result on success, or an
#' empty frame with the same shape on failure/empty result, so a caller can
#' unconditionally rbind()/bind_rows() every source's result without checking
#' for NULL or a missing/misnamed column first.
#' @param source_name See the description.
#' @param scrape_fn See the description.
#' @param expected_cols See the description.
#' @param log_path See the description.
#' @export
safe_scrape <- function(source_name, scrape_fn, expected_cols, log_path = "scrape_log.csv") {
  outcome <- tryCatch(
    list(df = scrape_fn(), error = NULL),
    error = function(e) list(df = NULL, error = conditionMessage(e))
  )

  if (!is.null(outcome$error)) {
    log_scrape_result(source_name, status = "error", n_rows = 0L,
                       error_message = outcome$error, log_path = log_path)
    message("safe_scrape: ", source_name, " failed: ", outcome$error)
    return(empty_df(expected_cols))
  }

  df <- outcome$df
  if (is.null(df) || nrow(df) == 0) {
    log_scrape_result(source_name, status = "empty", n_rows = 0L, log_path = log_path)
    return(empty_df(expected_cols))
  }

  log_scrape_result(source_name, status = "ok", n_rows = nrow(df), log_path = log_path)
  if ("Posted_Date" %in% names(df)) df$Posted_Date <- as.character(df$Posted_Date)
  if ("date_posted" %in% names(df)) df$date_posted <- as.character(df$date_posted)
  df
}

# ---------------------------------------------------------------------------
# Reference tables
# ---------------------------------------------------------------------------

#' `REFERENCE_TABLE_RULES`
#'
#' Files the pipeline re-downloads and keeps between runs. A failed or
#' partial download must never replace a good file, so each refresh goes
#' through write_reference_file(). `required`: columns at most 25\% blank.
#' `bounds`: the range every non-blank value must fall in -- generous, to
#' catch a units error or a misparsed column, not real change. `max_age`:
#' years the newest figure may lag the calendar year before
#' check_salary_drift.R calls the file stale.
#'
#' These are the tables every state refreshes (BLS OEWS and QCEW, IPEDS staff
#' pay). A state's own tables (MT's district crosswalk and OPI district
#' counties, WY's UW salary book) come from its `state_config()`
#' (`reference_tables`); `reference_table_rules()` gives both.
#' @export
REFERENCE_TABLE_RULES <- list(
  oews_role_wages = list(name = "BLS OEWS role wages", required = "Annual_Median",
                         bounds = list(Annual_Median = c(15000, 250000), Hourly_Median = c(7, 150)),
                         year_col = "Year", max_age = 2),
  qcew_county_school_pay = list(name = "BLS QCEW county school pay", required = "Avg_Annual_Pay",
                                bounds = list(Avg_Annual_Pay = c(15000, 150000)),
                                year_col = "Year", max_age = 2),
  ipeds_staff_salaries = list(name = "IPEDS staff salaries", required = "Avg_Salary",
                              bounds = list(Avg_Salary = c(15000, 400000)),
                              year_col = "Year", max_age = 3)
)

#' Reference rules for
#'
#' The rules for the reference table at `path` (by file name, without
#' `.csv`), from `reference_table_rules()`.
#' @param path See the description.
#' @param rules All rules to look in.
#' @export
reference_rules_for <- function(path, rules = reference_table_rules()) {
  r <- rules[[sub("\\.csv$", "", basename(path))]]
  if (is.null(r)) stop("No reference-table rules for ", basename(path),
                       " (REFERENCE_TABLE_RULES, or the state's state_config(reference_tables = ))")
  r
}

#' Reference table problem
#'
#' Why `new` shouldn't replace `old` (NULL when it's fine): fewer than
#' `min_share` of the current rows, a required column missing or mostly
#' blank, or a value out of bounds. With `old = NULL`, it checks a file on
#' its own.
#' @param new See the description.
#' @param rules See the description.
#' @param old See the description.
#' @param min_share See the description.
#' @export
reference_table_problem <- function(new, rules, old = NULL, min_share = 0.9) {
  if (is.null(new) || nrow(new) == 0) return("no rows")
  if (!is.null(old) && nrow(new) < min_share * nrow(old)) {
    return(sprintf("%d rows, down from %d", nrow(new), nrow(old)))
  }
  for (col in rules$required) {
    if (!col %in% names(new)) return(paste0("no ", col, " column"))
    blank <- mean(is.na(new[[col]]) | trimws(as.character(new[[col]])) == "")
    if (blank > 0.25) return(sprintf("%s is %.0f%% blank", col, 100 * blank))
  }
  for (col in intersect(names(rules$bounds), names(new))) {
    v <- suppressWarnings(as.numeric(new[[col]]))
    lim <- rules$bounds[[col]]
    out <- !is.na(v) & (v < lim[1] | v > lim[2])
    if (any(out)) {
      return(sprintf("%d %s value(s) outside %s-%s (e.g. %s)", sum(out), col,
                     format(lim[1], big.mark = ",", scientific = FALSE),
                     format(lim[2], big.mark = ",", scientific = FALSE), v[out][1]))
    }
  }
  NULL
}

#' Write reference file
#'
#' Writes a refreshed reference table only when reference_table_problem()
#' finds nothing wrong against the file it would replace; otherwise keeps
#' that file. Logs "Reference: <name>" to scrape_log.csv either way. Returns
#' TRUE when it wrote.
#' @param new See the description.
#' @param path See the description.
#' @param log_path See the description.
#' @export
write_reference_file <- function(new, path, log_path = "scrape_log.csv") {
  rules <- reference_rules_for(path)
  old <- if (file.exists(path)) utils::read.csv(path, stringsAsFactors = FALSE) else NULL
  problem <- reference_table_problem(new, rules, old)
  if (!is.null(problem)) {
    message(rules$name, ": kept the existing file, refreshed table rejected (", problem, ")")
    log_scrape_result(paste("Reference:", rules$name), "rejected", if (is.null(new)) 0L else nrow(new),
                      error_message = problem, log_path = log_path)
    return(invisible(FALSE))
  }
  utils::write.csv(new, path, row.names = FALSE)
  log_scrape_result(paste("Reference:", rules$name), "ok", nrow(new), log_path = log_path)
  invisible(TRUE)
}

#' Log reference result
#'
#' For a refresh that found the file already current, or failed outright.
#' @param path See the description.
#' @param status See the description.
#' @param error_message See the description.
#' @param log_path See the description.
#' @export
log_reference_result <- function(path, status, error_message = NA_character_, log_path = "scrape_log.csv") {
  rules <- reference_rules_for(path)
  n <- if (file.exists(path)) nrow(utils::read.csv(path)) else 0L
  if (!is.na(error_message)) message(rules$name, " refresh skipped: ", error_message)
  log_scrape_result(paste("Reference:", rules$name), status, n, error_message = error_message, log_path = log_path)
}

#' Reference table status
#'
#' One row per reference table in `app_dir` that needs a human look: its
#' refresh failed, was rejected or didn't run this run (per `scrape_log`
#' rows dated `today`), its newest figure is older than `max_age` years, or
#' the file itself fails its rules. check_salary_drift.R reports these.
#' @param app_dir See the description.
#' @param scrape_log See the description.
#' @param today See the description.
#' @param this_year See the description.
#' @export
reference_table_status <- function(app_dir, scrape_log, today = Sys.Date(),
                                   this_year = as.integer(format(today, "%Y"))) {
  todays_log <- scrape_log[as.Date(substr(scrape_log$timestamp, 1, 10)) == today, , drop = FALSE]
  if (is.null(current_state_config())) warning("reference_table_status(): no state_config() loaded, so only the shared tables are checked")
  all_rules <- reference_table_rules()
  rows <- lapply(names(all_rules), function(key) {
    rules <- all_rules[[key]]
    path <- file.path(app_dir, paste0(key, ".csv"))
    if (!file.exists(path)) return(NULL)   # tables are per state; verify_schema.R catches a lost one
    problems <- character(0)
    log <- todays_log[todays_log$source == paste("Reference:", rules$name), , drop = FALSE]
    if (nrow(log) == 0) {
      problems <- c(problems, "no refresh logged this run")
    } else {
      last <- log[nrow(log), ]
      if (last$status %in% c("error", "rejected")) {
        problems <- c(problems, paste0("refresh ", last$status, " (", last$error_message, "); the previous file was kept"))
      }
    }
    tbl <- utils::read.csv(path, stringsAsFactors = FALSE)
    problem <- reference_table_problem(tbl, rules)
    if (!is.null(problem)) problems <- c(problems, paste("file fails its rules:", problem))
    if (!is.na(rules$year_col) && rules$year_col %in% names(tbl)) {
      newest <- suppressWarnings(max(as.integer(sub("^\\D*(\\d{4}).*$", "\\1", tbl[[rules$year_col]])), na.rm = TRUE))
      if (is.finite(newest) && this_year - newest > rules$max_age) {
        problems <- c(problems, sprintf("newest figure is from %d", newest))
      }
    }
    if (length(problems) == 0) return(NULL)
    data.frame(name = rules$name, file = basename(path), problem = paste(problems, collapse = "; "),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) data.frame(name = character(0), file = character(0), problem = character(0)) else out
}
