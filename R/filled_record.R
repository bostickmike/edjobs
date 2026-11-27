# Postings a public record shows as filled (2026-11-26). A board can keep an ad up after the job is filled (MT's
# Custer: PE and Vo-Ag hires on the 07-22 and 08-03 board agendas, both still on its page 11-26), and OPI-style
# statewide feeds keep spring ads up for months. A state's own table lists each such posting with the record that
# shows the hire; a matching row is flagged (not counted) until the entry's `Until` date, so a job advertised again
# next school year counts. One posting at a time, each with its source: not a rule.

FILLED_RECORD_COLUMNS <- c("District", "Title", "Posted_Date", "Evidence", "Source_URL", "Record_Date", "Until")

#' Read and check a filled-postings record
#'
#' One row per posting a public record shows as filled. `District` and `Title` are matched to a row's district and
#' title (case and spacing ignored); `Posted_Date` (optional, YYYY-MM-DD) narrows it to the ad posted that day (a
#' statewide feed's spring ad). `Evidence` (what the record says, without names), `Source_URL` and `Record_Date` (the
#' record's own date) are required; `Until` (YYYY-MM-DD) is the last run date the entry applies to.
#' @param record A data frame, or the path of a CSV.
#' @return The record, every column present, checked.
#' @export
filled_record <- function(record) {
  if (is.character(record)) record <- utils::read.csv(record, stringsAsFactors = FALSE, colClasses = "character",
                                                     na.strings = character(0))
  if (!is.data.frame(record)) stop("filled_record(): a data frame or a CSV path")
  unknown <- setdiff(names(record), FILLED_RECORD_COLUMNS)
  if (length(unknown)) stop("filled_record(): unknown column(s): ", paste(unknown, collapse = ", "))
  for (col in setdiff(FILLED_RECORD_COLUMNS, names(record))) record[[col]] <- rep("", nrow(record))
  record <- record[FILLED_RECORD_COLUMNS]
  for (col in FILLED_RECORD_COLUMNS) { v <- trimws(as.character(record[[col]])); v[is.na(v)] <- ""; record[[col]] <- v }
  for (col in c("District", "Title", "Evidence", "Source_URL", "Record_Date", "Until")) {
    bad <- !nzchar(record[[col]])
    if (any(bad)) stop("filled_record(): ", col, " is blank in row(s) ", paste(which(bad), collapse = ", "))
  }
  for (col in c("Posted_Date", "Record_Date", "Until")) {
    v <- record[[col]]; bad <- nzchar(v) & is.na(suppressWarnings(as.Date(v, "%Y-%m-%d")))
    if (any(bad)) stop("filled_record(): ", col, " must be YYYY-MM-DD in row(s) ", paste(which(bad), collapse = ", "))
  }
  bad <- !grepl("^https?://", record$Source_URL)
  if (any(bad)) stop("filled_record(): Source_URL must be a web address in row(s) ", paste(which(bad), collapse = ", "))
  bad <- as.Date(record$Until) < as.Date(record$Record_Date)
  if (any(bad)) stop("filled_record(): Until is before Record_Date in row(s) ", paste(which(bad), collapse = ", "))
  rownames(record) <- NULL
  record
}

#' Why a row is a filled posting, by the record
#'
#' @param df Postings with the district, title and (optionally) posted-date columns.
#' @param record A [filled_record()] result (or anything it accepts).
#' @param run_date The run's date: an entry applies through its `Until`.
#' @param district_col,title_col,posted_col The columns of `df` to match on (`posted_col` only for entries that give
#'   a `Posted_Date`; a row without the column never matches those).
#' @return One reason per row ("filled per public record (<Record_Date>): <Evidence>"), NA where no entry matches.
#' @export
filled_record_reason <- function(df, record, run_date, district_col = "District", title_col = "title",
                                 posted_col = "date_posted") {
  record <- if (is.data.frame(record) && identical(names(record), FILLED_RECORD_COLUMNS)) record else filled_record(record)
  out <- rep(NA_character_, nrow(df))
  if (nrow(df) == 0 || nrow(record) == 0) return(out)
  norm <- function(x) tolower(gsub("\\s+", " ", trimws(as.character(x))))
  live <- record[as.Date(record$Until) >= as.Date(run_date), , drop = FALSE]
  posted <- if (posted_col %in% names(df)) substr(as.character(df[[posted_col]]), 1, 10) else rep(NA_character_, nrow(df))
  for (i in seq_len(nrow(live))) {
    e <- live[i, ]
    hit <- is.na(out) & norm(df[[district_col]]) == norm(e$District) & norm(df[[title_col]]) == norm(e$Title)
    if (nzchar(e$Posted_Date)) hit <- hit & posted %in% e$Posted_Date
    out[hit] <- sprintf("filled per public record (%s): %s", e$Record_Date, e$Evidence)
  }
  out
}
