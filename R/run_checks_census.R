# The Census coverage/plausibility check and the auto-fix PR's live check as package functions (2026-11-17 package
# review, step 6, slice 4). The states' check_census_drift.R hard-coded the counts they expected (MT 19 counties and
# 32 districts, WY 48 districts), which went stale as the registries grew (MT 56 counties and 201 districts on
# 2026-11-19): a check against a number below the real universe only fires after most of the coverage is gone. The
# expected counts now come from the committed files themselves.

#' The weekly Census coverage and value check
#'
#' Non-blocking and informational. Coverage: every county in `salarymap2.csv` (K-12) and `salarymap.csv` (colleges)
#' should have ACS county context (median household income), and every district a SAIPE child poverty rate (its own,
#' or for a school with no NCES id the surrounding district's), apart from `state_config(saipe_gaps = )`. The universe is read from the same files, so it
#' follows the registries. Values: the ACS and SAIPE figures should sit in a generous real-world range (a parser
#' reading the wrong Census variable, not unusual economics). Anything flagged is appended to `report_path`.
#' @param app_dir The app folder; defaults to the session's `state_config()`.
#' @param report_path The drift report the flags are appended to.
#' @return The coverage flags (a data frame, or NULL), invisibly.
#' @export
check_census_step <- function(app_dir = NULL, report_path = "/tmp/drift_report.md") {
  app_dir <- run_app_dir(app_dir)
  config <- current_state_config()
  k12 <- utils::read.csv(file.path(app_dir, "salarymap2.csv"), stringsAsFactors = FALSE)
  he <- utils::read.csv(file.path(app_dir, "salarymap.csv"), stringsAsFactors = FALSE)

  present <- function(x) !is.na(x) & nzchar(trimws(as.character(x)))
  counties <- function(d) unique(d$County[present(d$County)])
  covered <- function(d) unique(d$County[present(d$County) & !is.na(d$Median_Household_Income)])
  k12_counties <- counties(k12)
  he_counties <- counties(he)

  saipe_universe <- k12$District
  gaps <- intersect(config$saipe_gaps, saipe_universe)
  saipe_missing <- setdiff(k12$District[is.na(k12$Child_Poverty_Rate)], gaps)

  flags <- rbind(
    check_salary_coverage("K-12 county context (Census ACS)", length(covered(k12)), expected = length(k12_counties)),
    check_salary_coverage("HE county context (Census ACS)", length(covered(he)), expected = length(he_counties)),
    check_salary_coverage("K-12 district child poverty (Census SAIPE)",
                          sum(!is.na(k12$Child_Poverty_Rate) & !k12$District %in% gaps),
                          expected = length(saipe_universe) - length(gaps))
  )
  if (!is.null(flags) && nrow(flags) > 0) print(flags)

  coverage_lines <- if (is.null(flags) || nrow(flags) == 0) {
    character(0)
  } else {
    lines <- c(
      paste0("Automated Census source coverage check flagged ", nrow(flags), " source(s) as of ", Sys.Date(), "."),
      "",
      paste0("Like the salary coverage check, this is a hard assertion against the files' own universe (",
             length(k12_counties), " counties across the ", nrow(k12), " K-12 districts in salarymap2.csv, ",
             length(he_counties), " counties across the ", nrow(he), " colleges in salarymap.csv, SAIPE for ",
             length(saipe_universe) - length(gaps), " of the districts",
             if (length(gaps) > 0) paste0(" (not ", paste(gaps, collapse = " or "), ", which SAIPE has no rate for)") else "",
             ") rather than a trailing statistical baseline -- Census data updates at most once a year. A source ",
             "landing below its expected count usually means the Census API changed its response shape (a variable ",
             "code retired, a geography type renamed) and the parser is silently extracting less real data."),
      ""
    )
    for (i in seq_len(nrow(flags))) {
      r <- flags[i, ]
      lines <- c(lines, sprintf("- **%s**: expected >= %d, got %d", r$name, r$expected, r$actual))
    }
    if (length(saipe_missing) > 0) lines <- c(lines, paste0("  - No SAIPE rate: ", paste(saipe_missing, collapse = "; ")))
    c(lines, "")
  }

  # Bounds are deliberately generous (2024 ACS: MT county median income ~$52k-$85k, WY $59k-$90k; rent ~$600-$1,200
  # a month). Mining share and population change are ratios, so their bounds are on the -1 to 1 scale.
  k12_v <- function(col) stats::setNames(k12[[col]], k12$District)
  he_v <- function(col) stats::setNames(he[[col]], he$Name)
  plausibility_flags <- list(
    check_salary_value_bounds("K-12 county median income (Census ACS)", k12_v("Median_Household_Income"), min_ok = 20000, max_ok = 200000),
    check_salary_value_bounds("K-12 county median rent (Census ACS)", k12_v("Median_Gross_Rent"), min_ok = 200, max_ok = 3000),
    check_salary_value_bounds("K-12 county mining/energy employment share (Census ACS)", k12_v("Mining_Employment_Share"), min_ok = 0, max_ok = 1),
    check_salary_value_bounds("K-12 county population change, 5yr (Census ACS)", k12_v("Population_Change_Pct"), min_ok = -0.5, max_ok = 0.5),
    check_salary_value_bounds("K-12 district child poverty rate (Census SAIPE)", k12_v("Child_Poverty_Rate"), min_ok = 0, max_ok = 1),
    check_salary_value_bounds("HE county median income (Census ACS)", he_v("Median_Household_Income"), min_ok = 20000, max_ok = 200000),
    check_salary_value_bounds("HE county median rent (Census ACS)", he_v("Median_Gross_Rent"), min_ok = 200, max_ok = 3000),
    check_salary_value_bounds("HE county mining/energy employment share (Census ACS)", he_v("Mining_Employment_Share"), min_ok = 0, max_ok = 1),
    check_salary_value_bounds("HE county population change, 5yr (Census ACS)", he_v("Population_Change_Pct"), min_ok = -0.5, max_ok = 0.5)
  )
  plausibility_flags <- plausibility_flags[!vapply(plausibility_flags, is.null, logical(1))]
  if (length(plausibility_flags) > 0) for (f in plausibility_flags) print(f)

  plausibility_lines <- if (length(plausibility_flags) == 0) {
    character(0)
  } else {
    lines <- c(
      "Automated Census VALUE plausibility check flagged something worth a human look.",
      "",
      "Unlike the coverage check above (which only checks how many counties/districts came back), this checks whether the figures themselves look sane -- outside a generous real-world range. Usually means the Census API's response shape or a variable code changed just enough to misparse values while still returning a plausible count.",
      ""
    )
    for (f in plausibility_flags) {
      for (i in seq_len(nrow(f))) {
        r <- f[i, ]
        lines <- c(lines, sprintf("- **%s** (%s): %s is outside the expected %s-%s range", r$name, r$entity,
                                  format(r$value, big.mark = ","), format(r$min_ok, big.mark = ","), format(r$max_ok, big.mark = ",")))
      }
    }
    c(lines, "")
  }

  if (length(coverage_lines) == 0 && length(plausibility_lines) == 0) {
    cat("Census source coverage and value plausibility look healthy.\n")
    return(invisible(flags))
  }
  append_drift_report(c(coverage_lines, plausibility_lines), report_path)
  cat("Flagged items written to ", report_path, "\n", sep = "")
  invisible(flags)
}

#' The live check on a scraper auto-fix PR
#'
#' The fixture tests prove the parser handles the fixture; they can't prove the fixture is what the live site serves,
#' and on an auto-fix PR the same agent wrote both. This finds the source the PR claims to fix (its linked issue's
#' autofix-source marker), re-runs just that scraper against the live site with the PR's code, and writes the
#' result to `out_path` (posted as a PR comment by the workflow). A PR with no linked auto-fix issue is not
#' applicable. The state's scraper files must already be sourced into `envir`.
#' @param repo,pr The repository ("owner/name") and PR number.
#' @param out_path Where the markdown report goes.
#' @param gh A function taking `gh` CLI arguments and returning the parsed JSON (tests pass a stub).
#' @param session_factory Makes the headless browser session a chromote scraper needs.
#' @param envir Where the state's scraper functions are.
#' @return TRUE when the check passed or doesn't apply, FALSE on a failure [summarize_live_check()] defines.
#' @export
live_check_autofix_step <- function(repo = Sys.getenv("GITHUB_REPOSITORY"), pr = Sys.getenv("PR_NUMBER"),
                                    out_path = "/tmp/live_check.md", gh = NULL,
                                    session_factory = function() chromote::ChromoteSession$new(),
                                    envir = globalenv()) {
  if (is.null(gh)) {
    gh <- function(args) {
      out <- system2("gh", shQuote(c(args, "--repo", repo)), stdout = TRUE)
      jsonlite::fromJSON(paste(out, collapse = "\n"))
    }
  }
  pr_info <- gh(c("pr", "view", pr, "--json", "closingIssuesReferences,body"))
  linked <- pr_info$closingIssuesReferences$number
  # A PR that mentions its issue without a closing keyword.
  if (length(linked) == 0 && !is.null(pr_info$body)) {
    linked <- as.integer(unique(regmatches(pr_info$body, gregexpr("(?<=#)[0-9]+", pr_info$body, perl = TRUE))[[1]]))
  }

  source_name <- NA_character_
  issue_body <- ""
  for (n in linked) {
    body <- tryCatch(gh(c("issue", "view", n, "--json", "body"))$body, error = function(e) "")
    src <- parse_autofix_marker(body)
    if (!is.na(src)) { source_name <- src; issue_body <- body; break }
  }
  if (is.na(source_name)) {
    cat("No linked scraper auto-fix issue -- live check not applicable.\n")
    return(TRUE)
  }

  call <- resolve_scraper_call(source_name)
  if (is.null(call)) {
    writeLines(c(sprintf("### Live scraper check: %s", source_name), "",
                 ":grey_question: No single-source live check exists for this source (see scraper_sources in state_config.R) -- verify the fix against the live page by hand."),
               out_path)
    return(TRUE)
  }

  cat("Running", describe_scraper_call(call), "for", source_name, "against the live site\n")
  result <- tryCatch(run_scraper_call(call, session_factory = session_factory, envir = envir), error = function(e) e)
  summary <- summarize_live_check(source_name, result, parse_autofix_expected_titles(issue_body))
  writeLines(summary$markdown, out_path)
  cat(summary$markdown, sep = "\n")
  summary$pass
}
