# The salary-source check as a package function (2026-11-17 package review, step 6, slice 7). The states'
# check_salary_drift.R compared each source's coverage with counts typed into the script, which went stale as the
# registries grew (MT expected 32 districts with CCD and OPI finance against 201 in the file on 2026-11-19, DLI 25-27
# against 149; WY counted WSBA's rows, 57 >= 48, with 10 districts lacking a base salary). Mike chose (2026-11-20) to
# compare each source with the last committed file instead: no hand-kept lists, and it follows the registries. Replayed
# on every committed version of both states' salary maps (75 commits): no false flags, the counts only ever grew.

#' What a state's salary check looks at
#'
#' The state's half of [salary_drift_step()], for `state_config(salary_checks = )`. Files are `salarymap2.csv` (K-12,
#' one row per `District`) and `salarymap.csv` (colleges, one row per `Name`).
#' @param coverage One row per source column whose coverage is watched: `name` (as the report shows it), `file`,
#'   `column`. A source is flagged when fewer rows have a value than in the last committed file.
#' @param bounds One row per value range: `name`, `file`, `column`, `min_ok`, `max_ok` ([check_salary_value_bounds()]).
#' @param yoy One row per year-over-year pair: `file`, `current`, `prior` (column names;
#'   [check_salary_yoy_plausibility()]).
#' @param extra A function of no arguments returning report lines (character(0) when there's nothing to say), for a
#'   check only one state has (Wyoming's Sheridan/Gillette split watch), or NULL.
#' @return A list of class `edjobs_salary_checks`.
#' @export
salary_checks <- function(coverage, bounds = NULL, yoy = NULL, extra = NULL) {
  need <- function(d, cols, what) {
    if (is.null(d)) return(invisible())
    if (!is.data.frame(d) || length(setdiff(cols, names(d))) > 0) stop(what, " needs columns ", paste(cols, collapse = ", "))
    bad <- setdiff(d$file, names(SALARY_FILE_KEYS))
    if (length(bad) > 0) stop(what, ": unknown file(s) ", paste(bad, collapse = ", "))
  }
  need(coverage, c("name", "file", "column"), "coverage")
  need(bounds, c("name", "file", "column", "min_ok", "max_ok"), "bounds")
  need(yoy, c("file", "current", "prior"), "yoy")
  if (!is.null(extra) && !is.function(extra)) stop("extra must be a function or NULL")
  structure(list(coverage = coverage, bounds = bounds, yoy = yoy, extra = extra), class = "edjobs_salary_checks")
}

SALARY_FILE_KEYS <- c(salarymap2.csv = "District", salarymap.csv = "Name")

#' The salary files from the last commit
#'
#' The default baseline for [salary_drift_step()]: `git show HEAD:<app_dir>/<file>`, run before the weekly run commits
#' its refreshed files. When the environment variable `EDJOBS_SALARY_BASELINE` names a folder, the files are read from
#' there instead (a local rehearsal's copy has no `.git`).
#' @param app_dir The app folder, relative to the repository root.
#' @return A function of a file name returning that file's data frame, or NULL when there is no committed copy.
#' @export
committed_salary_file <- function(app_dir) {
  function(file) {
    dir <- Sys.getenv("EDJOBS_SALARY_BASELINE")
    if (nzchar(dir)) {
      path <- file.path(dir, file)
      return(if (file.exists(path)) utils::read.csv(path, stringsAsFactors = FALSE) else NULL)
    }
    txt <- suppressWarnings(tryCatch(
      system2("git", c("show", shQuote(paste0("HEAD:", app_dir, "/", file))), stdout = TRUE, stderr = FALSE),
      error = function(e) NULL))
    if (length(txt) == 0 || !is.null(attr(txt, "status"))) return(NULL)
    utils::read.csv(text = paste(txt, collapse = "\n"), stringsAsFactors = FALSE)
  }
}

# Which rows of `d` have a value in `column`, by entity name.
salary_value_present <- function(d, column, key) {
  if (is.null(d) || !column %in% names(d)) return(character(0))
  x <- d[[column]]
  ok <- !is.na(x) & nzchar(trimws(as.character(x)))
  unique(as.character(d[[key]][ok]))
}

#' Compare salary-source coverage with the last committed file
#'
#' One row per `coverage` source whose count of rows with a value fell: `name`, `previous`, `actual`, and `lost` (the
#' entities that had a value and now don't, or whose row is gone, "; "-separated). A source that gained or held is not
#' returned. A file with no baseline is skipped (see the `baseline` attribute: the files that had none).
#' @param coverage The `coverage` table of [salary_checks()].
#' @param current,previous Functions of a file name returning its data frame (`previous` may return NULL).
#' @return A data frame (zero rows when nothing fell), with attribute `missing_baseline`.
#' @export
compare_salary_coverage <- function(coverage, current, previous) {
  out <- data.frame(name = character(0), previous = integer(0), actual = integer(0), lost = character(0),
                    stringsAsFactors = FALSE)
  missing <- character(0)
  for (f in unique(coverage$file)) {
    key <- SALARY_FILE_KEYS[[f]]
    now <- current(f); before <- previous(f)
    if (is.null(before)) { missing <- c(missing, f); next }
    for (i in which(coverage$file == f)) {
      col <- coverage$column[i]
      had <- salary_value_present(before, col, key)
      has <- salary_value_present(now, col, key)
      if (length(has) >= length(had)) next
      out <- rbind(out, data.frame(name = coverage$name[i], previous = length(had), actual = length(has),
                                   lost = paste(sort(setdiff(had, has)), collapse = "; "), stringsAsFactors = FALSE))
    }
  }
  attr(out, "missing_baseline") <- missing
  out
}

#' The weekly salary-source check
#'
#' Non-blocking and informational, run after the render on the refreshed `salarymap2.csv` and `salarymap.csv`.
#' Coverage: each source in `state_config(salary_checks = )$coverage` should have a value for at least as many rows as
#' in the last committed file ([compare_salary_coverage()]); a drop names the districts or colleges that lost data.
#' Salary data changes once a year at most, so a drop usually means a source changed its page, PDF or API and the
#' parser is extracting less. Values: the configured ranges and year-over-year outliers. Reference tables: the pay
#' estimates' tables whose refresh failed, was rejected or went stale ([reference_table_status()]). Then the state's
#' own `extra` lines. Anything flagged is appended to `report_path`.
#' @param app_dir The app folder; defaults to the session's `state_config()`.
#' @param report_path The drift report the flags are appended to.
#' @param previous A function of a file name returning last run's data frame or NULL; defaults to
#'   [committed_salary_file()].
#' @param scrape_log_path This run's scrape log, for the reference-table status.
#' @return The coverage drops (a data frame), invisibly.
#' @export
salary_drift_step <- function(app_dir = NULL, report_path = "/tmp/drift_report.md", previous = NULL,
                              scrape_log_path = "scrape_log.csv") {
  app_dir <- run_app_dir(app_dir)
  checks <- current_state_config()$salary_checks
  if (is.null(checks)) stop("No salary checks configured: set state_config(salary_checks = salary_checks(...))", call. = FALSE)
  if (is.null(previous)) previous <- committed_salary_file(app_dir)
  files <- list()
  current <- function(f) {
    if (is.null(files[[f]])) files[[f]] <<- utils::read.csv(file.path(app_dir, f), stringsAsFactors = FALSE)
    files[[f]]
  }

  drops <- compare_salary_coverage(checks$coverage, current, previous)
  missing <- attr(drops, "missing_baseline")
  if (nrow(drops) > 0) print(drops)
  coverage_lines <- character(0)
  if (nrow(drops) > 0 || length(missing) > 0) {
    coverage_lines <- c(
      paste0("Automated salary-source coverage check flagged ", nrow(drops), " source(s) as of ", Sys.Date(), "."),
      "",
      paste0("Each source is compared with the committed file from the last run: salary data updates once a year at ",
             "most, so fewer districts or colleges with a value usually means the source changed its page, PDF or API ",
             "and the parser is extracting less, not that the state lost them. A drop is flagged on the run it happens; ",
             "after that run's commit it is the new baseline, so check the names below this week."),
      ""
    )
    for (i in seq_len(nrow(drops))) {
      r <- drops[i, ]
      coverage_lines <- c(coverage_lines, sprintf("- **%s**: %d had a value last run, %d now", r$name, r$previous, r$actual),
                          paste0("  - Lost: ", r$lost))
    }
    if (length(missing) > 0) {
      coverage_lines <- c(coverage_lines, paste0("- No committed copy of ", paste0("`", missing, "`", collapse = " or "),
                                                 " to compare with, so its coverage wasn't checked."))
    }
    coverage_lines <- c(coverage_lines, "")
  }

  named <- function(f, col) {
    d <- current(f)
    stats::setNames(if (col %in% names(d)) d[[col]] else rep(NA_real_, nrow(d)), d[[SALARY_FILE_KEYS[[f]]]])
  }
  b <- checks$bounds
  plausibility_flags <- c(
    lapply(seq_len(if (is.null(b)) 0 else nrow(b)), function(i)
      check_salary_value_bounds(b$name[i], named(b$file[i], b$column[i]), min_ok = b$min_ok[i], max_ok = b$max_ok[i])),
    lapply(seq_len(if (is.null(checks$yoy)) 0 else nrow(checks$yoy)), function(i) {
      y <- checks$yoy[i, ]
      check_salary_yoy_plausibility(named(y$file, y$current), named(y$file, y$prior))
    })
  )
  plausibility_flags <- plausibility_flags[!vapply(plausibility_flags, is.null, logical(1))]
  if (length(plausibility_flags) > 0) for (f in plausibility_flags) print(f)
  plausibility_lines <- character(0)
  if (length(plausibility_flags) > 0) {
    plausibility_lines <- c(
      "Automated salary VALUE plausibility check flagged something worth a human look.",
      "",
      "Unlike the coverage check above (which only checks how many rows have a value), this checks whether the dollar figures themselves look sane -- either outside a generous real-world range, or a year-over-year change that's a statistical outlier against every other district's/institution's change this same run. Usually means a source's page/PDF/API layout shifted just enough to misparse values while still returning a plausible row count, not that the underlying number is really this different.",
      ""
    )
    for (f in plausibility_flags) {
      for (i in seq_len(nrow(f))) {
        r <- f[i, ]
        plausibility_lines <- c(plausibility_lines, if ("min_ok" %in% names(f)) {
          sprintf("- **%s** (%s): $%s is outside the expected $%s-$%s range", r$name, r$entity,
                  format(r$value, big.mark = ","), format(r$min_ok, big.mark = ","), format(r$max_ok, big.mark = ","))
        } else {
          sprintf("- **%s**: $%s -> $%s (%+.1f%%), a real outlier vs. every other entity's change this run",
                  r$name, format(r$prior, big.mark = ","), format(r$current, big.mark = ","), r$pct_change * 100)
        })
      }
    }
    plausibility_lines <- c(plausibility_lines, "")
  }

  scrape_log <- if (file.exists(scrape_log_path)) utils::read.csv(scrape_log_path, stringsAsFactors = FALSE) else
    data.frame(timestamp = character(0), source = character(0), status = character(0), error_message = character(0))
  reference_flags <- reference_table_status(app_dir, scrape_log)
  if (nrow(reference_flags) > 0) print(reference_flags)
  reference_lines <- if (nrow(reference_flags) == 0) character(0) else c(
    "Reference tables behind the pay estimates need a look.",
    "",
    "The dashboard keeps showing the previous file for each of these, so nothing is blank -- but its figures stop updating, and a rejected refresh means the source changed shape.",
    "",
    sprintf("- **%s** (`%s`): %s", reference_flags$name, reference_flags$file, reference_flags$problem),
    ""
  )

  extra_lines <- if (is.null(checks$extra)) character(0) else checks$extra()
  report_lines <- c(coverage_lines, plausibility_lines, reference_lines, extra_lines)
  if (length(report_lines) == 0) {
    cat("Salary source coverage and value plausibility look healthy.\n")
    return(invisible(drops))
  }
  append_drift_report(report_lines, report_path)
  cat("Flagged items written to ", report_path, "\n", sep = "")
  invisible(drops)
}
