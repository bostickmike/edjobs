# Column-presence contract check for the files each state ships. Each state
# keeps its own REQUIRED_SCHEMAS / REQUIRED_SCHEMAS_XLSX (its own columns).
# Moved from the MT and WY repos' schema_check.R in phase 2 (2026-11-11).

#' Check file schema
#'
#' Pure function: given one file's actual column names, which of the
#' required ones (if any) are missing? NULL if the schema is fine, so
#' callers can filter with Filter(Negate(is.null), ...) the same way other
#' check_*() functions in this repo do.
#' @param file_name See the description.
#' @param actual_cols See the description.
#' @param required_cols See the description.
#' @export
check_file_schema <- function(file_name, actual_cols, required_cols) {
  missing <- setdiff(required_cols, actual_cols)
  if (length(missing) == 0) return(NULL)
  data.frame(file = file_name, missing_column = missing, stringsAsFactors = FALSE)
}
