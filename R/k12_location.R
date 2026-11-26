# A K-12 posting's place (2026-11-26). Many district pages and some boards give no place, and a statewide feed's town
# is typed in by whoever posts the ad (Wyoming's WSBA list: "Laramie" for Laramie County School District 1, whose schools
# are all in Cheyenne; "CCSD#2"). CCD's directories say where each district's office and schools are, so a district whose
# schools are all in one town gives its postings that town with nothing to guess, and a feed town that is none of the
# district's towns is corrected to its office town. Measured on Wyoming's rows: claude-notes/2026-11-26_wy-location.

#' A district's towns from CCD's directories
#'
#' One row per district (NCES LEAID): the town of its office and of each of its schools, as CCD writes them.
#' @param schools Rows from [fetch_ccd_school_directory()] (`leaid`, `city_location`).
#' @param districts Rows from CCD's district directory (`leaid`, `city_location`), e.g.
#'   [find_latest_ccd_directory_year()]`$data`.
#' @param year The directory year, stored as `CCD_Year`.
#' @return NCES_LEAID (7 digits, text), Office_Town, School_Towns (sorted, "; " between), CCD_Year.
#' @export
ccd_district_towns <- function(schools, districts, year) {
  id <- function(x) sprintf("%07d", as.integer(x))
  town <- function(x) { x <- trimws(as.character(x)); ifelse(is.na(x) | !nzchar(x), NA_character_, x) }
  st <- data.frame(leaid = id(schools$leaid), town = town(schools$city_location), stringsAsFactors = FALSE)
  st <- st[!is.na(st$town), , drop = FALSE]
  school_towns <- tapply(st$town, st$leaid, function(t) paste(sort(unique(t)), collapse = "; "))
  out <- data.frame(NCES_LEAID = id(districts$leaid), Office_Town = town(districts$city_location), stringsAsFactors = FALSE)
  out$School_Towns <- unname(school_towns[out$NCES_LEAID])
  out$CCD_Year <- as.character(year)
  out <- out[!duplicated(out$NCES_LEAID), , drop = FALSE]
  rownames(out) <- NULL
  out[order(out$NCES_LEAID), , drop = FALSE]
}

#' Fetch a state's district towns from the latest CCD directories
#'
#' [ccd_district_towns()] for the latest year whose district directory answers ([find_latest_ccd_directory_year()]),
#' with that year's school directory.
#' @inheritParams find_latest_ccd_directory_year
#' @return As [ccd_district_towns()].
#' @export
fetch_ccd_district_towns <- function(fips = state_fips()) {
  latest <- find_latest_ccd_directory_year(fips)
  if (is.na(latest$year)) stop("fetch_ccd_district_towns(): no CCD district directory year answered")
  ccd_district_towns(fetch_ccd_school_directory(fips, latest$year), latest$data, latest$year)
}

#' Pipeline stage: refresh the district towns
#'
#' `district_towns.csv` in the app folder ([fetch_ccd_district_towns()]); a failed or short refresh keeps the committed
#' file ([write_reference_file()]).
#' @param config The state's config; the session's by default.
#' @param fetch The fetch, for tests.
#' @return The table on file afterwards (NULL when there is none).
#' @export
refresh_district_towns <- function(config = current_state_config(), fetch = function() fetch_ccd_district_towns(config$fips)) {
  if (is.null(config)) stop("refresh_district_towns(): no state config (set_state_config())")
  path <- file.path(config$app_dir, "district_towns.csv")
  refresh_or_keep(path, function(p) write_reference_file(fetch(), p, retrieved_source = "Urban CCD"))
  if (file.exists(path)) read_district_towns(path) else NULL
}

#' Read a district-towns table
#' @param path The CSV ([refresh_district_towns()]).
#' @return The table, every column text.
#' @export
read_district_towns <- function(path) {
  utils::read.csv(path, colClasses = "character", na.strings = c("", "NA"))
}

# A town written two ways is one town: "Ft. Washakie" (CCD) is "Fort Washakie" (WSBA).
town_key <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("^ft\\.?\\s+", "fort ", x)
  x <- gsub("^st\\.?\\s+", "saint ", x)
  gsub("[^a-z0-9]+", "", x)
}

#' Give K-12 postings their place from the district's towns
#'
#' For each row, with its district's CCD towns (office and schools):
#' * a row from the feed (`feed_rows`) whose town is none of the district's is given the office town (a feed's town is
#'   typed in with the ad);
#' * a row with no place, in a district whose schools are all in one town, is given that town.
#' A place a board gives (often a building) is kept; a row in a district with schools in several towns, or with no CCD
#' match, keeps what it has.
#' @param df K-12 rows: `District`, `location`.
#' @param towns [read_district_towns()].
#' @param district_ids Named character vector, District -> NCES LEAID (from the state's registry).
#' @param feed_rows Logical, one per row: the row's place came from the statewide feed's town field.
#' @return `df` with `location` filled or corrected. The counts are in the attribute `"location_changes"` (`filled`,
#'   `corrected`).
#' @export
fill_k12_locations <- function(df, towns, district_ids, feed_rows = rep(FALSE, nrow(df))) {
  stopifnot(length(feed_rows) == nrow(df))
  changes <- c(filled = 0L, corrected = 0L)
  if (nrow(df) == 0 || is.null(towns) || nrow(towns) == 0) return(structure(df, location_changes = changes))
  lea <- unname(district_ids[as.character(df$District)])
  lea <- ifelse(is.na(lea) | !nzchar(lea), NA_character_, sprintf("%07d", suppressWarnings(as.integer(lea))))
  hit <- match(lea, towns$NCES_LEAID)
  office <- towns$Office_Town[hit]
  school_towns <- strsplit(ifelse(is.na(towns$School_Towns[hit]), "", towns$School_Towns[hit]), "; ", fixed = TRUE)
  only_town <- vapply(school_towns, function(t) if (length(t) == 1 && nzchar(t)) t else NA_character_, "")
  loc <- as.character(df$location)
  blank <- is.na(loc) | !nzchar(trimws(loc))
  known <- mapply(function(l, o, t) town_key(l) %in% town_key(c(o, t)), loc, office, school_towns, USE.NAMES = FALSE)
  correct <- feed_rows & !blank & !is.na(hit) & !is.na(office) & !known
  fill <- blank & !is.na(only_town)
  loc[correct] <- office[correct]
  loc[fill] <- only_town[fill]
  df$location <- loc
  structure(df, location_changes = c(filled = sum(fill), corrected = sum(correct)))
}
