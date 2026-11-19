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
#' @keywords internal
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
#' @keywords internal
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

# Stage 2 (edjobs 0.34.0): places a posting names in its title or on its own page, for boards whose Location is a
# department or a fixed campus label (the PeopleAdmin colleges). Only explicit phrases count, never a bare town
# name: pages carry boilerplate ("Gillette College is located in Gillette, WY" on a Wright posting; EWC's footer
# address) and application checkboxes ("Interested in Billings Campus"). Measured on 360 live PeopleAdmin pages and
# MT's archives: claude-notes/2026-11-16_college-campus-locations.md, "Stage 2 measured".
PLACE_NAME_RX <- "([A-Z][A-Za-z.'-]+(?: [A-Z][A-Za-z.'-]+){0,2})"

#' The ", ST" / ", State" that follows a town in a title marker or page sentence
#'
#' Only the state being placed: a Wyoming posting naming "(Big Horn, MT)" is not in Big Horn, Wyoming (twelve town
#' names exist in both states). Until 2026-11-17 the pattern accepted MT, Montana, WY and Wyoming in either state and
#' no other state at all.
#' @param state_abbrev Two-letter abbreviation; by default the state config's (from its `state` name). With neither,
#'   any US state.
#' @return A regular expression.
#' @keywords internal
place_state_rx <- function(state_abbrev = NULL) {
  if (is.null(state_abbrev)) {
    config <- current_state_config()
    if (!is.null(config)) state_abbrev <- datasets::state.abb[match(config$state, datasets::state.name)]
  }
  states <- if (is.null(state_abbrev) || all(is.na(state_abbrev))) c(datasets::state.abb, datasets::state.name) else
    c(state_abbrev, datasets::state.name[match(state_abbrev, datasets::state.abb)])
  # A comma or a space before the state: "(GJSSC)" is an acronym, not "GJS, SC".
  paste0("(?:,\\s*|\\s+)(?:", paste(stats::na.omit(states), collapse = "|"), ")\\b")
}

# Every first capture group of `rx` in each string.
all_captures <- function(x, rx) {
  m <- regmatches(x, gregexpr(rx, x, perl = TRUE))
  lapply(m, function(hits) if (length(hits)) sub(rx, "\\1", hits, perl = TRUE) else character(0))
}

#' Places a posting title marks
#'
#' The town in a title marker: `*Havre, MT*`, `(Conrad, MT)`, `**Havre, MT**`, `(Rock Springs Campus)`. A marker
#' is a town and either a state or "Campus" inside asterisks or parentheses; a town elsewhere in a title doesn't
#' count ("Director of ... the Green River Center").
#' @param title Character vector of titles.
#' @inheritParams place_state_rx
#' @return A list of character vectors (candidate names, not yet checked against the state's places).
#' @keywords internal
title_place_names <- function(title, state_abbrev = NULL) {
  rx <- paste0("[*(]\\s*", PLACE_NAME_RX, "(?:", place_state_rx(state_abbrev), "|\\s+Campus)\\s*[*)]")
  all_captures(coalesce_chr(title), rx)
}

#' Places a PeopleAdmin posting page names
#'
#' Reads only explicit phrases from the page's text (navigation, header, footer and scripts removed): "This position
#' is located (or based) in <Town>, MT|WY"; "The Extension office is located in <Town>" (MSU Extension: an agent's
#' office is not always the county seat, e.g. Carbon County's is in Joliet); "at/on the <Town> Campus|Center"; and
#' "main campus" in lower case (a site banner's "Main Campus" is not about the job), returned as "main campus".
#' Names are not yet checked against the state's places.
#' @param html_text A posting page, or NA.
#' @inheritParams place_state_rx
#' @return Character: the names found, "; "-separated, or NA.
#' @keywords internal
peopleadmin_page_places <- function(html_text, state_abbrev = NULL) {
  if (is.na(html_text)) return(NA_character_)
  doc <- rvest::read_html(html_text)
  xml2::xml_remove(rvest::html_elements(doc, "script, style, nav, header, footer"))
  body <- rvest::html_element(doc, "body")
  if (inherits(body, "xml_missing")) return(NA_character_)
  txt <- gsub("\\s+", " ", rvest::html_text2(body))
  rx <- c(paste0("\\bThis position is (?:located|based) in ", PLACE_NAME_RX, place_state_rx(state_abbrev)),
          paste0("\\bThe Extension office is located in ", PLACE_NAME_RX, "\\b"),
          paste0("\\b(?:at|on) the ", PLACE_NAME_RX, " (?:Campus|campus|Center)\\b"))
  found <- unlist(lapply(rx, function(r) all_captures(txt, r)[[1]]))
  # Lower case only, as measured: EWC's site banner reads "Eastern Wyoming College Main Campus & Douglas Campus".
  if (grepl("\\bmain (?:[A-Z]{2,5} )?campus\\b", txt, perl = TRUE)) found <- c(found, "main campus")
  found <- unique(trimws(found))
  if (length(found)) paste(found, collapse = "; ") else NA_character_
}

#' Place each college posting at a campus town
#'
#' Adds where each posting goes on the map. A posting's towns come from: its Location ([location_town()]); its
#' title's markers ([title_place_names()]); a department table (`campus_departments`: an institution's Location
#' value -> town, e.g. MSU's research centers); and its page (`Page_Places`, from [peopleadmin_page_places()], where
#' "main campus" means the main campus town). Only places in the state count, and never a name two places share.
#' One town other than the main campus town -> `Campus_Town`; anything else stays at the main campus. `Place_Basis`
#' says why: "main" (the main campus town, or a registry with no `Town`), "other_town", "out_of_state", "no_town"
#' (nothing placeable: a department, "United States", or several towns at once: Mike 2026-11-16, a posting for
#' several sites goes at the main campus). `Page_Places`, when present, is dropped.
#' @param postings Data frame with `Institution` and `Location` (and optionally `Title`, `Page_Places`).
#' @param institutions The HE registry (columns `Institution`, `Town`).
#' @param places The state's places, from [read_state_places()].
#' @param campus_departments Optional data frame (`Institution`, `Location`, `Town`): Location values that are a
#'   fixed off-campus site.
#' @inheritParams location_town
#' @return `postings` with `Campus_Town`, `Campus_County`, `Campus_Latitude`, `Campus_Longitude`, `Place_Basis`.
#' @export
place_he_postings <- function(postings, institutions, places, state_abbrev, campus_departments = NULL) {
  stopifnot(all(c("Institution", "Location") %in% names(postings)), all(c("Institution", "Town") %in% names(institutions)))
  main <- institutions$Town[match(postings$Institution, institutions$Institution)]
  no_main <- is.na(main) | !nzchar(main)
  n <- nrow(postings)
  unique_towns <- places$Town[!places$Town %in% places$Town[duplicated(tolower(places$Town))]]
  as_place <- function(x) {
    x <- gsub("\\bFt\\.? ", "Fort ", trimws(x))
    unique_towns[match(tolower(x), tolower(unique_towns))]
  }
  dept <- rep(NA_character_, n)
  if (!is.null(campus_departments) && nrow(campus_departments)) {
    stopifnot(all(c("Institution", "Location", "Town") %in% names(campus_departments)))
    dept <- campus_departments$Town[match(paste(postings$Institution, postings$Location, sep = "\r"),
                                          paste(campus_departments$Institution, campus_departments$Location, sep = "\r"))]
  }
  titles <- if ("Title" %in% names(postings)) title_place_names(postings$Title, state_abbrev) else vector("list", n)
  page <- if ("Page_Places" %in% names(postings)) strsplit(coalesce_chr(postings$Page_Places), "; ", fixed = TRUE) else vector("list", n)
  loc_town <- location_town(postings$Location, places, state_abbrev)
  towns <- lapply(seq_len(n), function(i) {
    pg <- page[[i]]
    pg[pg == "main campus"] <- if (no_main[i]) NA_character_ else main[i]
    t <- as_place(c(loc_town[i], dept[i], titles[[i]], pg))
    unique(t[!is.na(t)])
  })
  n_towns <- lengths(towns)
  town <- ifelse(n_towns == 1, vapply(towns, function(t) if (length(t)) t[1] else NA_character_, ""), NA_character_)
  oos <- n_towns == 0 & location_out_of_state(postings$Location, state_abbrev)
  basis <- ifelse(oos, "out_of_state", ifelse(is.na(town), "no_town",
                  ifelse(no_main | tolower(town) == tolower(coalesce_chr(main)), "main", "other_town")))
  at <- ifelse(basis == "other_town", town, NA_character_)
  i <- match(at, places$Town)
  postings$Campus_Town <- at
  postings$Campus_County <- places$County[i]
  postings$Campus_Latitude <- places$Latitude[i]
  postings$Campus_Longitude <- places$Longitude[i]
  postings$Place_Basis <- basis
  postings$Page_Places <- NULL
  postings
}

#' `HE_PLACE_COLUMNS`
#'
#' The columns [place_he_postings()] adds. They belong to the week's postings (hedata.xlsx and its archive), not to
#' accumulated histories such as facultydata.csv, which leave them out the way they leave out the pay columns.
#' @export
HE_PLACE_COLUMNS <- c("Campus_Town", "Campus_County", "Campus_Latitude", "Campus_Longitude", "Place_Basis")
