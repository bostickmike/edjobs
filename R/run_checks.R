# The weekly run's own checks and wrappers, as package functions (2026-11-17 package review, step 6, slice 1).
# Each state's `.github/scripts/<name>.R` was the same script in both states apart from the app folder; it is now a
# call to one of these. Output and failure messages are the scripts' own, so the run logs read the same.

run_app_dir <- function(app_dir) {
  if (!is.null(app_dir)) return(app_dir)
  app_dir <- current_state_config()$app_dir
  if (is.null(app_dir)) stop("No state config set: source the state's state_config.R (set_state_config()) or pass app_dir", call. = FALSE)
  app_dir
}

#' Render the weekly Rmd, printing R's call stack if the render is interrupted
#'
#' If the workflow's `timeout -s INT` interrupts the render (a step stuck long enough to threaten the job's time
#' limit), the call stack goes to stderr first, so the run says where it was stuck instead of just "The operation
#' was canceled" (WY, 2026-11-13).
#' @param rmd The Rmd file.
#' @param envir Where the chunks run: the global environment, as when the script called [rmarkdown::render()] at top
#'   level (its own default, `parent.frame()`, would be this function's frame, inside the package namespace).
#' @param ... Passed to [rmarkdown::render()].
#' @return What [rmarkdown::render()] returns.
#' @export
render_with_stack <- function(rmd, envir = globalenv(), ...) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) stop("render_with_stack() needs the rmarkdown package", call. = FALSE)
  withCallingHandlers(
    rmarkdown::render(rmd, envir = envir, ...),
    interrupt = function(e) {
      cat("\n=== Render interrupted at ", format(Sys.time(), "%H:%M:%S"),
          " (time budget). R call stack, innermost last:\n", sep = "", file = stderr())
      for (cl in utils::tail(sys.calls(), 40)) {
        cat("  ", paste(deparse(cl, nlines = 1L, width.cutoff = 200L), collapse = ""), "\n", file = stderr())
      }
    }
  )
}

#' Stop the run when K-12 or college postings fell by more than half
#'
#' Guards against `safe_scrape()`'s per-source error handling masking a systemic failure (the runner's IP blocked, a
#' shared upstream outage): every source failing at once would render "successfully" while overwriting real data with
#' a near-empty dataset. Compares this run's `combinedclean.csv` and `hedata.xlsx` row counts with the copies the
#' workflow saved before the render, and stops (non-zero exit from `Rscript`) instead of letting the run commit.
#' @param app_dir The app folder; defaults to the session's `state_config()`.
#' @param baseline_dir Where the workflow saved `baseline_combinedclean.csv` and `baseline_hedata.xlsx`.
#' @param threshold The share of last run's rows below which the run stops.
#' @return The four counts, invisibly.
#' @export
check_run_row_counts <- function(app_dir = NULL, baseline_dir = "/tmp", threshold = 0.5) {
  app_dir <- run_app_dir(app_dir)
  old_k12 <- nrow(utils::read.csv(file.path(baseline_dir, "baseline_combinedclean.csv")))
  new_k12 <- nrow(utils::read.csv(file.path(app_dir, "combinedclean.csv")))
  old_he <- nrow(readxl::read_xlsx(file.path(baseline_dir, "baseline_hedata.xlsx")))
  new_he <- nrow(readxl::read_xlsx(file.path(app_dir, "hedata.xlsx")))

  cat(sprintf("K-12 rows: %d -> %d\n", old_k12, new_k12))
  cat(sprintf("HE rows:   %d -> %d\n", old_he, new_he))

  if (new_k12 < old_k12 * threshold) {
    stop(sprintf(
      "K-12 row count dropped from %d to %d (more than 50%%) -- looks like a systemic failure, not normal week-to-week churn. Refusing to commit.",
      old_k12, new_k12
    ), call. = FALSE)
  }
  if (new_he < old_he * threshold) {
    stop(sprintf(
      "HE row count dropped from %d to %d (more than 50%%) -- looks like a systemic failure, not normal week-to-week churn. Refusing to commit.",
      old_he, new_he
    ), call. = FALSE)
  }
  cat("Sanity check passed.\n")
  invisible(c(old_k12 = old_k12, new_k12 = new_k12, old_he = old_he, new_he = new_he))
}

#' Stop the run when a shipped file lacks a column the app reads
#'
#' The blocking schema check, run right after [check_run_row_counts()] and before anything is committed: app.R reads
#' these columns by name, so a missing one would crash the live dashboard on the next deploy.
#' @param csv_schemas,xlsx_schemas Named lists, file name -> required columns (the state's `REQUIRED_SCHEMAS` and
#'   `REQUIRED_SCHEMAS_XLSX`).
#' @param app_dir The app folder; defaults to the session's `state_config()`.
#' @return The problems found (a data frame, zero rows when none), invisibly.
#' @export
check_app_schemas <- function(csv_schemas, xlsx_schemas = list(), app_dir = NULL) {
  app_dir <- run_app_dir(app_dir)
  problems <- list()
  check_one <- function(f, required, read_cols) {
    path <- file.path(app_dir, f)
    if (!file.exists(path)) {
      return(data.frame(file = f, missing_column = "<entire file missing>", stringsAsFactors = FALSE))
    }
    check_file_schema(f, read_cols(path), required)
  }
  for (f in names(csv_schemas)) {
    problems[[length(problems) + 1]] <- check_one(f, csv_schemas[[f]], function(p) names(utils::read.csv(p, nrows = 1, check.names = FALSE)))
  }
  for (f in names(xlsx_schemas)) {
    problems[[length(problems) + 1]] <- check_one(f, xlsx_schemas[[f]], function(p) names(readxl::read_xlsx(p, n_max = 1)))
  }
  problems <- Filter(Negate(is.null), problems)

  if (length(problems) > 0) {
    all_problems <- do.call(rbind, problems)
    cat("Schema check FAILED:\n")
    print(all_problems)
    stop(
      "Schema check failed: ", nrow(all_problems), " missing column(s) across ",
      length(unique(all_problems$file)), " file(s) -- see above. app.R references these ",
      "columns by name, so this would crash the live dashboard for every user on the next ",
      "deploy. Refusing to commit. Fix whichever pipeline chunk produces the affected ",
      "file(s), or if this is an intentional schema change, update REQUIRED_SCHEMAS in ",
      "schema_check.R to match.",
      call. = FALSE
    )
  }
  cat("Schema check passed -- every shipped dataset has all the columns app.R expects.\n")
  invisible(data.frame(file = character(0), missing_column = character(0), stringsAsFactors = FALSE))
}

#' The weekly run's expectations step
#'
#' Non-blocking and informational, like the drift check: evaluates every row of `run_expectations.csv` against this
#' run's freshly written files. Always writes the summary to the run's step summary page (`GITHUB_STEP_SUMMARY`) and
#' `out_csv`. Appends to `report_path` (which files or updates the drift issue) when a row failed or errored, or when
#' the report already exists for another reason, so an all-pass week with nothing else to say files no issue.
#' @param path The expectations file.
#' @param run_date The run's date (`RUN_DATE`, else today).
#' @param out_csv,report_path Where the results table and the drift-report section go.
#' @return The results ([evaluate_run_expectations()]), invisibly; `NULL` when there is no expectations file.
#' @export
run_expectations_step <- function(path = "run_expectations.csv",
                                  run_date = as.Date(Sys.getenv("RUN_DATE", unset = format(Sys.Date()))),
                                  out_csv = "/tmp/run_expectations.csv", report_path = "/tmp/drift_report.md") {
  if (!file.exists(path)) {
    cat("No run_expectations.csv, nothing to check.\n")
    return(invisible(NULL))
  }
  exp <- read_run_expectations(path)
  probs <- validate_run_expectations(exp)
  if (length(probs) > 0) cat("Problems in run_expectations.csv (rows still evaluated):\n", paste0("  ", probs, "\n"), sep = "")

  res <- evaluate_run_expectations(exp, run_date = run_date)
  utils::write.csv(res, out_csv, row.names = FALSE)
  for (i in seq_len(nrow(res))) {
    cat(sprintf("%-8s %-40s %s%s\n", res$Status[i], res$id[i],
                format_expectation_value(res$Value[i]),
                if (res$Status[i] %in% c("pass", "fail", "error"))
                  paste0("  (expected ", format_expectation_range(res$min[i], res$max[i]), ")",
                         if (nzchar(res$Detail[i])) paste0(" ", res$Detail[i]) else "") else ""))
  }

  report_lines <- build_run_expectations_report_markdown(res)
  cat(report_lines[1], "\n")

  summary_path <- Sys.getenv("GITHUB_STEP_SUMMARY")
  if (nzchar(summary_path)) write(c(report_lines, ""), file = summary_path, append = TRUE)

  needs_look <- any(res$Status %in% c("fail", "error"))
  if (needs_look || file.exists(report_path)) {
    if (file.exists(report_path)) {
      write(c("", "---", "", report_lines), file = report_path, append = TRUE)
    } else {
      writeLines(report_lines, report_path)
    }
  }
  invisible(res)
}
