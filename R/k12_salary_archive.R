# A state's K-12 salary source publishes only the current year (WSBA's PDF in WY, the OPI/DLI tables in MT), so a
# multi-year trend exists only if each run keeps its own snapshot. One row per district per salary year: a weekly
# run appends only the district/year pairs the archive lacks, which also repairs a run that wrote part of a year.
# Moved from both states' salary_scrapers.R (phase 5 close-out, 2026-11-16); the columns kept are the state's.

#' Missing K-12 salary snapshot rows
#'
#' The rows of `snapshot` whose District and Salary_Year pair is not already in `existing` (one per pair).
#' @param existing The archive so far (columns District, Salary_Year, ...).
#' @param snapshot This run's rows for the current salary year.
#' @export
missing_k12_salary_snapshot_rows <- function(existing, snapshot) {
  snapshot <- snapshot[!duplicated(snapshot[, c("District", "Salary_Year")]), , drop = FALSE]
  existing_keys <- paste(existing$District, existing$Salary_Year, sep = "\r")
  snapshot_keys <- paste(snapshot$District, snapshot$Salary_Year, sep = "\r")
  snapshot[!(snapshot_keys %in% existing_keys), , drop = FALSE]
}

#' Archive a K-12 salary snapshot
#'
#' Appends this run's salary year to the state's K-12 salary history, one row per district per year. Skips when
#' `Salary_Year` isn't a single value this run, and when every district/year pair is already recorded.
#' @param salarymap2 This run's district table, with District, Salary_Year and `columns`.
#' @param history_path The history CSV (created on the first run).
#' @param columns The state's salary columns to keep, e.g. WY `c("Teacher_Base_Salary", "Superintendent_Salary")`.
#' @return `TRUE` (invisibly) when rows were appended, otherwise `FALSE`.
#' @export
archive_k12_salary_snapshot <- function(salarymap2, history_path, columns) {
  current_year <- unique(stats::na.omit(salarymap2$Salary_Year))
  if (length(current_year) != 1) {
    message("K-12 salary archive: Salary_Year isn't a single consistent value this run, skipping archive.")
    return(invisible(FALSE))
  }
  current_year <- current_year[1]
  keep <- c("District", "Salary_Year", columns)

  existing <- if (file.exists(history_path)) {
    utils::read.csv(history_path, stringsAsFactors = FALSE)
  } else {
    empty <- c(list(District = character(0), Salary_Year = character(0)),
               stats::setNames(rep(list(numeric(0)), length(columns)), columns))
    as.data.frame(empty, stringsAsFactors = FALSE)
  }

  snapshot <- salarymap2[!is.na(salarymap2$Salary_Year) & salarymap2$Salary_Year == current_year, keep, drop = FALSE]
  missing_snapshot <- missing_k12_salary_snapshot_rows(existing, snapshot)
  if (nrow(missing_snapshot) == 0) {
    message("K-12 salary archive: every district/year pair for ", current_year,
            " is already recorded, no snapshot update needed.")
    return(invisible(FALSE))
  }

  updated <- dplyr::bind_rows(existing, missing_snapshot)
  utils::write.csv(updated, history_path, row.names = FALSE)
  message("K-12 salary archive: appended ", nrow(missing_snapshot),
          " missing district snapshot(s) for ", current_year, ".")
  invisible(TRUE)
}
