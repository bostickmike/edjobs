# Where a college posting is (Mike, 2026-11-16: "we need the locations to be accurate on the map"). Colleges post
# jobs at branch campuses, outreach centers and field offices, and the map used to put every posting at the
# institution's one point. A posting is placed at another town only when its Location writes that town; anything
# else (no town, several sites, out of state) stays at the main campus and is counted for the map's footnote. A wrong
# town is worse than the main campus. Measured on the archives: claude-notes/2026-11-16_college-campus-locations.md.

#' Town written in a posting's Location
#'
#' Returns the state place a Location names, or NA. A town counts only where a place is written: the whole value
#' ("Riverton"), the words right before ", <ST>" ("Cheyenne, WY", "KALISPELL, MT 59901"), the words after
#' "US-<ST>-", or either side of " - " ("Wind River - Ft. Washakie"). Never a word inside other text, so a street
#' address ("1115 N Roberts St") is not a town. "Ft." reads as "Fort". A name two places share is not placed.
#' @param location Character vector of Location values.
#' @param places The state's places (column `Town`), from [read_state_places()].
#' @param state_abbrev Two-letter state abbreviation.
#' @return Character vector: the place's `Town`, or NA.
#' @export
location_town <- function(location, places, state_abbrev) {
  towns <- places$Town[!places$Town %in% places$Town[duplicated(tolower(places$Town))]]
  key <- tolower(towns)
  one <- function(loc) {
    if (is.na(loc)) return(NA_character_)
    loc <- gsub("[[:space:]]+", " ", trimws(loc))
    if (!nzchar(loc)) return(NA_character_)
    loc <- gsub("\\bFt\\.? ", "Fort ", loc)
    cands <- c(regmatches(loc, regexpr(paste0("[A-Za-z .'-]+(?=, ", state_abbrev, "\\b)"), loc, perl = TRUE)),
               sub(paste0("^US-", state_abbrev, "-"), "",
                   regmatches(loc, regexpr(paste0("^US-", state_abbrev, "-[A-Za-z .'-]+"), loc))),
               trimws(strsplit(sub("\\(.*$", "", loc), " - ", fixed = TRUE)[[1]]), loc)
    cands <- trimws(sub("^.*( - |, )", "", cands))
    hit <- towns[match(tolower(cands), key)]
    hit <- hit[!is.na(hit)]
    if (length(hit)) hit[1] else NA_character_
  }
  unname(vapply(as.character(location), one, character(1)))
}

#' Does a Location name another state?
#'
#' TRUE when a Location writes another state's abbreviation as a place ("Denver, CO, US", "US-HI-Honolulu",
#' "St. Clair County, IL").
#' @inheritParams location_town
#' @export
location_out_of_state <- function(location, state_abbrev) {
  other <- setdiff(c(datasets::state.abb, "DC"), state_abbrev)
  grepl(paste0("(^|, |US-)(", paste(other, collapse = "|"), ")(,|-| |$)"), coalesce_chr(location))
}

coalesce_chr <- function(x) ifelse(is.na(x), "", as.character(x))

#' Read a state's places table
#'
#' Reads `places.csv` (Town, County, Latitude, Longitude, ...): every Census place in the state, its internal point
#' (Census Gazetteer) and its county.
#' @param path Path to the state's places file.
#' @export
read_state_places <- function(path) {
  p <- utils::read.csv(path, stringsAsFactors = FALSE, colClasses = c(Town = "character", County = "character"))
  stopifnot(all(c("Town", "County", "Latitude", "Longitude") %in% names(p)))
  p
}

#' Place each college posting at a campus town
#'
#' Adds where each posting goes on the map. `Campus_Town` is the town when the Location writes a town in the state
#' other than the institution's main campus town (`Town` in the HE registry); otherwise NA, and the posting is shown
#' at the main campus. `Place_Basis` says why: "main" (the main campus town, or a registry with no `Town`),
#' "other_town", "out_of_state", "no_town" (nothing placeable: a department, "United States", several sites).
#' @param postings Data frame with `Institution` and `Location`.
#' @param institutions The HE registry (columns `Institution`, `Town`).
#' @param places The state's places, from [read_state_places()].
#' @inheritParams location_town
#' @return `postings` with `Campus_Town`, `Campus_County`, `Campus_Latitude`, `Campus_Longitude`, `Place_Basis`.
#' @export
place_he_postings <- function(postings, institutions, places, state_abbrev) {
  stopifnot(all(c("Institution", "Location") %in% names(postings)), all(c("Institution", "Town") %in% names(institutions)))
  main <- institutions$Town[match(postings$Institution, institutions$Institution)]
  town <- location_town(postings$Location, places, state_abbrev)
  oos <- is.na(town) & location_out_of_state(postings$Location, state_abbrev)
  no_main <- is.na(main) | !nzchar(main)
  basis <- ifelse(oos, "out_of_state", ifelse(is.na(town), "no_town",
                  ifelse(no_main | tolower(town) == tolower(coalesce_chr(main)), "main", "other_town")))
  at <- ifelse(basis == "other_town", town, NA_character_)
  i <- match(at, places$Town)
  postings$Campus_Town <- at
  postings$Campus_County <- places$County[i]
  postings$Campus_Latitude <- places$Latitude[i]
  postings$Campus_Longitude <- places$Longitude[i]
  postings$Place_Basis <- basis
  postings
}

#' `HE_PLACE_COLUMNS`
#'
#' The columns [place_he_postings()] adds. They belong to the week's postings (hedata.xlsx and its archive), not to
#' accumulated histories such as facultydata.csv, which leave them out the way they leave out the pay columns.
#' @export
HE_PLACE_COLUMNS <- c("Campus_Town", "Campus_County", "Campus_Latitude", "Campus_Longitude", "Place_Basis")
