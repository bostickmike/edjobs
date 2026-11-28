#' The shared registry schemas
#'
#' The columns every state's registries have. `K12_REGISTRY_COLUMNS`: one row per K-12 source in
#' `k12_district_registry.csv` (the district, its NCES LEAID, where it is, and how its board is read: `Platform`,
#' `Platform_Key`, `Job_Link` and the list rules). `HE_REGISTRY_COLUMNS`: one row per college in
#' `he_institution_registry.csv`. A state may add its own columns after these.
#' @export
K12_REGISTRY_COLUMNS <- c("District", "NCES_LEAID", "City", "County", "Latitude", "Longitude", "Platform",
                          "Platform_Key", "Job_Link", "List_Start", "List_Stop", "List_Skip")

#' @rdname K12_REGISTRY_COLUMNS
#' @export
HE_REGISTRY_COLUMNS <- c("Institution", "County", "Latitude", "Longitude", "Platform", "Feed_URL", "Job_Link")

#' Read a state's registry
#'
#' Reads `k12_district_registry.csv` or `he_institution_registry.csv` the same way in every state: every column as
#' text, blank cells kept as `""` (not `NA`), except `Latitude` and `Longitude`, which are numbers. A column that is
#' blank in every row therefore stays text (a plain `read.csv()` makes it logical `NA`). `read_k12_registry()` also
#' gives `NCES_LEAID` as a 7-digit, zero-padded string, so a state whose FIPS code starts with 0 keeps its leading
#' zero; a blank id (a school outside CCD) stays `""`. Stops, naming them, when the file lacks any of the schema's
#' columns; the state's own extra columns come through as text.
#'
#' A relative `path` that isn't in the working directory is looked for in its parent directories, so the same call
#' works from a state's repository root (the weekly run) and from `tests/testthat` (its tests).
#' @param path The registry CSV.
#' @return A data frame.
#' @examples
#' reg <- read_k12_registry(system.file("toystate", "k12_district_registry.csv", package = "edjobs"))
#' reg[, c("District", "NCES_LEAID", "Platform")]
#' @export
read_k12_registry <- function(path = "k12_district_registry.csv") {
  r <- read_registry_table(path, K12_REGISTRY_COLUMNS, "K-12 registry")
  r$NCES_LEAID <- pad_nces_id(r$NCES_LEAID)
  r
}

#' @rdname read_k12_registry
#' @export
read_he_registry <- function(path = "he_institution_registry.csv") read_registry_table(path, HE_REGISTRY_COLUMNS, "college registry")

# Every column text with blanks as "", Latitude/Longitude numeric; `required`: the columns that must be there (NULL:
# no check, for scraper_registry()'s path tables, which may be any registry).
read_registry_table <- function(path, required = NULL, what = "registry") {
  path <- find_registry_file(path, what)
  r <- utils::read.csv(path, colClasses = "character", na.strings = character(0), check.names = FALSE,
                       fileEncoding = "UTF-8")
  missing <- setdiff(required, names(r))
  if (length(missing)) stop(what, " ", path, " lacks column(s): ", paste(missing, collapse = ", "))
  for (col in intersect(c("Latitude", "Longitude"), names(r))) {
    v <- trimws(r[[col]])
    bad <- nzchar(v) & is.na(suppressWarnings(as.numeric(v)))
    if (any(bad)) stop(what, " ", path, ": ", col, " is not a number in row(s) ", paste(which(bad), collapse = ", "))
    r[[col]] <- as.numeric(v)
  }
  r
}

# `path` as given when it exists; a relative path that doesn't is looked for in the parent directories (the state's repo
# root, seen from tests/testthat). Stops when it is nowhere.
find_registry_file <- function(path, what = "registry") {
  if (!is.character(path) || length(path) != 1 || is.na(path)) stop(what, ": no file at ", format(path))
  if (file.exists(path)) return(path)
  if (!grepl("^(/|~|[A-Za-z]:)", path)) {
    dir <- normalizePath(getwd(), mustWork = FALSE)
    repeat {
      parent <- dirname(dir)
      if (identical(parent, dir)) break
      dir <- parent
      if (file.exists(file.path(dir, path))) return(file.path(dir, path))
    }
  }
  stop(what, ": no file at ", path)
}
