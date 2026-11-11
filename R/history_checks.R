# Shared checks used by the history accumulator and the rebuild-parity
# check. The accumulator itself (append_weekly_rows, build_k12_*) differs
# between the states and stays in each repo for now. Moved in phase 2
# (2026-11-11).

#' Check total matches parts
#'
#' Consistency check for the incremental-append mechanism itself (as opposed
#' to the underlying scrape data drifting, which drift_check.R already
#' covers separately) -- verifies this week's freshly computed "Total" rows
#' actually equal the sum of this week's own per-district/per-institution
#' "parts" rows, mirroring the same invariant
#' tests/testthat/test-data-integrity.R already checks on the committed
#' files, but run proactively here BEFORE anything is written to the
#' growing accumulated history, not just caught after the fact. A mismatch
#' means a bug in this week's summary computation -- stop() rather than
#' silently appending a corrupted week onto years of otherwise-good history.
#' @param total_rows See the description.
#' @param part_rows See the description.
#' @param group_cols See the description.
#' @param value_col See the description.
#' @export
check_total_matches_parts <- function(total_rows, part_rows, group_cols, value_col = "sum") {
  parts_agg <- part_rows %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarize(parts_sum = sum(.data[[value_col]]), .groups = "drop")

  merged <- total_rows %>%
    dplyr::select(dplyr::all_of(group_cols), total_value = dplyr::all_of(value_col)) %>%
    dplyr::full_join(parts_agg, by = group_cols)

  mismatches <- merged %>%
    dplyr::filter(is.na(total_value) | is.na(parts_sum) | total_value != parts_sum)

  if (nrow(mismatches) > 0) {
    stop(
      "check_total_matches_parts(): this week's Total row(s) don't equal the sum of their ",
      "own parts -- a bug in this week's summary computation, not a data-source problem. ",
      "Refusing to append a corrupted week onto the accumulated history.\n",
      paste(utils::capture.output(print(mismatches)), collapse = "\n")
    )
  }
  invisible(TRUE)
}

#' Rebuild parity row
#'
#' Rebuild parity (classification safeguards, item 12): how one rebuilt file
#' differs from the committed one, ignoring row order, NA vs "" and \\r\\n vs \\n (CSV
#' round-trips blur them). Both sides compared as text. Returns one row:
#' File, Committed_Rows, Rebuilt_Rows, Missing_Cols (in committed, not rebuilt),
#' Extra_Cols, Only_Committed / Only_Rebuilt (row counts on the shared columns)
#' and Example (one differing row from each side).
#' @param file See the description.
#' @param committed See the description.
#' @param rebuilt See the description.
#' @export
rebuild_parity_row <- function(file, committed, rebuilt) {
  as_text <- function(d) {
    d <- as.data.frame(d, stringsAsFactors = FALSE)
    d[] <- lapply(d, function(x) {
      x <- gsub("\r\n", "\n", as.character(x), fixed = TRUE)   # read.csv() turns a quoted \r\n into \n
      x[is.na(x) | x == ""] <- "<NA>"
      x
    })
    d
  }
  committed <- as_text(committed); rebuilt <- as_text(rebuilt)
  common <- intersect(names(committed), names(rebuilt))
  key <- function(d) if (length(common)) do.call(paste, c(unname(d[common]), sep = " | ")) else character(nrow(d))
  a <- key(committed); b <- key(rebuilt)
  # Multiset difference: a row twice in one file and once in the other differs.
  tab <- function(x) table(factor(x, levels = unique(c(a, b))))
  diff_count <- tab(a) - tab(b)
  only_c <- names(diff_count)[diff_count > 0]; only_r <- names(diff_count)[diff_count < 0]
  data.frame(File = file, Committed_Rows = nrow(committed), Rebuilt_Rows = nrow(rebuilt),
             Missing_Cols = paste(setdiff(names(committed), names(rebuilt)), collapse = ", "),
             Extra_Cols = paste(setdiff(names(rebuilt), names(committed)), collapse = ", "),
             Only_Committed = sum(diff_count[diff_count > 0]), Only_Rebuilt = -sum(diff_count[diff_count < 0]),
             Example = paste(c(if (length(only_c)) paste("committed:", only_c[1]), if (length(only_r)) paste("rebuilt:", only_r[1])),
                             collapse = " // "),
             stringsAsFactors = FALSE)
}
