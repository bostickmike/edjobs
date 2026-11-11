# tools/roxygenize_moved_file.R -- for the state -> edjobs migration.
# Usage: Rscript tools/roxygenize_moved_file.R <state file.R> <out R/file.R>
# Copies a state repo's .R file into the package, turning the comment block
# directly above each top-level definition into its roxygen documentation
# (title from the name, the comments as the description, one @param per
# argument pointing at it) and exporting it. Comments not directly above a
# definition (file headers, notes inside functions) stay plain comments.
# Rd-special characters in converted comments are escaped (% and backslash).
args <- commandArgs(trailingOnly = TRUE)
src <- args[1]; out <- args[2]
lines <- readLines(src, warn = FALSE)
pd <- parse(src, keep.source = TRUE)
sr <- attr(pd, "srcref")
title_of <- function(nm) { t <- gsub("_", " ", nm); if (grepl("^[A-Z0-9 ]+$", t)) paste0("`", nm, "`") else paste0(toupper(substr(t, 1, 1)), substr(t, 2, nchar(t))) }
esc <- function(x) { x <- gsub("\\\\", "\\\\\\\\", x); gsub("%", "\\\\%", x) }
header_at <- list()
for (i in seq_along(pd)) {
  e <- pd[[i]]
  if (!(is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]))) next
  nm <- as.character(e[[2]]); start <- sr[[i]][1]
  k <- start - 1; while (k >= 1 && grepl("^\\s*#", lines[k]) && !grepl("^\\s*#'", lines[k])) k <- k - 1
  block <- if (k + 1 <= start - 1) (k + 1):(start - 1) else integer(0)
  desc <- esc(sub("^\\s*# ?", "", lines[block]))
  is_fn <- is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))
  params <- if (is_fn) names(eval(e[[3]]))[TRUE] else character(0)
  if (is_fn) params <- names(as.list(e[[3]][[2]]))
  h <- c(paste("#'", title_of(nm)), if (length(desc)) c("#'", paste("#'", desc)) else character(0),
         if (length(params)) paste0("#' @param ", params, " See the description.") else character(0),
         "#' @export")
  header_at[[as.character(start)]] <- list(block = block, h = h)
}
drop <- unlist(lapply(header_at, `[[`, "block"))
res <- character(0)
for (j in seq_along(lines)) {
  if (j %in% drop) next
  if (!is.null(header_at[[as.character(j)]])) res <- c(res, header_at[[as.character(j)]]$h)
  res <- c(res, lines[j])
}
writeLines(res, out)
cat(sprintf("%s -> %s: %d definitions documented and exported\n", src, out, length(header_at)))
