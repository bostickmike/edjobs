#' Which columns a CSV file quotes
#'
#' Reads a CSV file as written by `write.csv()` and reports, for each column, whether any of its data fields is
#' wrapped in double quotes. Quoted fields may hold commas, doubled quotes and line breaks.
#' @param path A CSV file with a header row.
#' @return A named logical vector, one element per column (named by the header).
#' @keywords internal
csv_quoted_columns <- function(path) {
  txt <- readChar(path, file.size(path), useBytes = TRUE)
  # Records, not lines: a line ending inside a quoted field (an odd running count of quotes) continues the record.
  # One regex per short record keeps this linear; one regex over a multi-MB file was quadratic.
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  inside <- cumsum(nchar(gsub('[^"]', "", lines, useBytes = TRUE), type = "bytes")) %% 2 == 1
  rec <- cumsum(c(TRUE, !utils::head(inside, -1L)))
  records <- sub("\r$", "", vapply(split(lines, rec), paste, "", collapse = "\n", USE.NAMES = FALSE))
  # Each match is one field and the comma after it; zero-length matches (only ever at a record's end) are dropped.
  fields <- regmatches(records, gregexpr('(?:"(?:[^"]|"")*"|[^,"]*)(?:,|$)', records, perl = TRUE))
  fields <- lapply(fields, function(f) f[nzchar(f)])
  header <- gsub('""', '"', sub('^"(.*)"$', "\\1", sub(",$", "", fields[[1]])))
  data <- unlist(fields[-1], use.names = FALSE)
  col <- unlist(lapply(fields[-1], seq_along), use.names = FALSE)
  quoted_cols <- unique(col[startsWith(data, '"')])
  stats::setNames(seq_along(header) %in% quoted_cols, header)
}

#' Write a CSV with the quoting an existing file uses
#'
#' `write.csv()` quotes every character column, so a data frame read with `colClasses = "character"` and written
#' back quotes dates and numbers its file had left bare, and every line of the file shows as changed. This writes
#' `df` to `path` quoting only the columns `like` quotes (by name), plus any other column holding a comma, quote or
#' line break, so unchanged rows stay byte-identical.
#' @param df The data frame to write (row names are not written).
#' @param path Where to write it.
#' @param like The file whose quoting to copy; by default `path` itself, read before it is overwritten.
#' @return `path`, invisibly; called for writing the file.
#' @export
write_csv_keep_quoting <- function(df, path, like = path) {
  q <- csv_quoted_columns(like)
  needs <- vapply(df, function(x) is.character(x) && any(grepl('[,"\r\n]', x[!is.na(x)])), logical(1))
  quote_cols <- which(names(df) %in% names(q)[q] | needs)
  utils::write.csv(df, path, row.names = FALSE, quote = if (length(quote_cols)) quote_cols else FALSE)
  invisible(path)
}
