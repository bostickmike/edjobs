# When each outside data source was last retrieved (Mike, 2026-11-16: BLS asks citations to give the date data were
# accessed; show a date for every source). The pipeline records a date only when a fetch succeeds and its result is
# kept, so a failed or rejected refresh leaves the date of the data still on the page.

#' Source retrieved dates file
#'
#' Path of the file holding each source's last retrieval date in an app folder.
#' @param app_dir The dashboard's app folder.
#' @keywords internal
source_retrieved_path <- function(app_dir) file.path(app_dir, "source_retrieved_dates.csv")

#' Read source retrieved dates
#'
#' Reads `source_retrieved_dates.csv` (columns Source, Retrieved); an empty frame when the file is missing.
#' @param app_dir The dashboard's app folder.
#' @keywords internal
read_source_retrieved <- function(app_dir) {
  path <- source_retrieved_path(app_dir)
  if (!file.exists(path)) return(data.frame(Source = character(0), Retrieved = character(0), stringsAsFactors = FALSE))
  d <- utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character")
  d[, c("Source", "Retrieved")]
}

#' Record source retrieved
#'
#' Sets `source`'s retrieval date to `date` in `source_retrieved_dates.csv` (one row per source, sorted by name).
#' Call it only after the source's data was fetched and kept.
#' @param source Stable source key, e.g. "BLS OEWS", "Census ACS", "Urban IPEDS".
#' @param app_dir The dashboard's app folder.
#' @param date Retrieval date (default today).
#' @export
record_source_retrieved <- function(source, app_dir, date = Sys.Date()) {
  stopifnot(length(source) == 1, nzchar(source))
  d <- read_source_retrieved(app_dir)
  d <- d[d$Source != source, , drop = FALSE]
  d <- rbind(d, data.frame(Source = source, Retrieved = format(as.Date(date)), stringsAsFactors = FALSE))
  d <- d[order(d$Source), , drop = FALSE]
  utils::write.csv(d, source_retrieved_path(app_dir), row.names = FALSE)
  invisible(d)
}

#' Source retrieved label
#'
#' Footer text for one or more source keys: "retrieved 2026-11-10", "retrieved 2026-11-03 to 2026-11-10" when the
#' sources' dates differ, or "" when none is recorded.
#' @param dates Output of read_source_retrieved().
#' @param sources Source keys whose dates the label covers.
#' @keywords internal
source_retrieved_label <- function(dates, sources) {
  x <- sort(unique(dates$Retrieved[dates$Source %in% sources & !is.na(dates$Retrieved) & nzchar(dates$Retrieved)]))
  if (length(x) == 0) return("")
  if (length(x) == 1) return(paste("retrieved", x))
  paste("retrieved", x[1], "to", x[length(x)])
}
