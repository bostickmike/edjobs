# A link to each record's own page at its source, where one exists (Mike, 2026-11-16: "each row in each data set
# could have a url link to its source whenever possible ... anyone is easily able to look up any data point
# themselves"). Measured 11-16 (Jobs_Dash_repos claude-notes/2026-11-16_source-links-per-row.md): NCES's CCD district
# page and IPEDS College Navigator are stable pages keyed by the ids the profile files already carry; the state
# salary files, SAIPE and ACS have no page per district, so they get no per-row link. Both NCES pages show NCES's
# newest year, which can be newer than the file's CCD_Year / Salary_Year.

#' NCES CCD district page
#'
#' The NCES Common Core of Data district page for each 7-digit NCES LEA id; NA for a missing or malformed id.
#' @param leaid NCES_LEAID values (character or numeric).
#' @export
nces_district_url <- function(leaid) {
  id <- sub("\\.0+$", "", trimws(as.character(leaid)))
  # A LEAID read as a number loses the leading zero of a state with FIPS 01-09 (Alabama's 0100005 -> 100005). Only
  # numbers are padded: a 6-digit string is a malformed id, not a lost zero.
  short <- is.numeric(leaid) & !is.na(id) & grepl("^[0-9]{6}$", id)
  id[short] <- paste0("0", id[short])
  ifelse(!is.na(id) & grepl("^[0-9]{7}$", id),
         paste0("https://nces.ed.gov/ccd/districtsearch/district_detail.asp?ID2=", id), NA_character_)
}

#' IPEDS College Navigator page
#'
#' The NCES College Navigator page for each 6-digit IPEDS unit id; NA for a missing or malformed id.
#' @param unitid IPEDS_UNITID values (character or numeric).
#' @export
college_navigator_url <- function(unitid) {
  id <- sub("\\.0+$", "", trimws(as.character(unitid)))
  ifelse(!is.na(id) & grepl("^[0-9]{6}$", id), paste0("https://nces.ed.gov/collegenavigator/?id=", id), NA_character_)
}

#' Add source record links
#'
#' Adds `CCD_Link` (from `NCES_LEAID`, [nces_district_url()]) and `IPEDS_Link` (from `IPEDS_UNITID`,
#' [college_navigator_url()]), each right after its id column, to a frame that has that id. Rebuilt from the id on
#' every call, so running it on last week's file gives the same columns in the same place.
#' @param df A data frame, e.g. salarymap2.csv (districts) or salarymap.csv (colleges).
#' @export
add_source_record_links <- function(df) {
  links <- list(c(id = "NCES_LEAID", link = "CCD_Link", fn = "nces_district_url"),
                c(id = "IPEDS_UNITID", link = "IPEDS_Link", fn = "college_navigator_url"))
  for (l in links) {
    if (!l[["id"]] %in% names(df)) next
    df[[l[["link"]]]] <- NULL
    url <- match.fun(l[["fn"]])(df[[l[["id"]]]])
    at <- match(l[["id"]], names(df))
    df <- cbind(df[seq_len(at)], stats::setNames(data.frame(url, stringsAsFactors = FALSE), l[["link"]]),
                df[setdiff(seq_along(df), seq_len(at))])
  }
  df
}
