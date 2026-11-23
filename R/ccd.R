# ---------------------------------------------------------------------------
# NCES Common Core of Data: district staffing and enrollment, school-level
# staffing and free/reduced-price lunch share, and the geocoder lookup of the
# districts a point lies in
# ---------------------------------------------------------------------------
#
# From the Urban Institute Education Data Portal
# (https://educationdata.urban.org/documentation/). Moved from the states'
# ccd_staff_scraper.R (package phase 5, 2026-11-15). The states differed only
# in how a dashboard district found its CCD records: Montana by hand-written
# pairs of CCD district names (an elementary district plus a high school
# district, e.g. "Billings Elem" + "Billings H S") and crosswalk LEAIDs,
# Wyoming by rewriting "X #1" to "X 1". Both now pass NCES ids: `district_ids`
# is a named list, dashboard district -> its LEAIDs (7 digits). A district
# with two LEAIDs (Montana's elementary + high school pairs) gets their sum.
# Measured 11-15 on the 2024 directory: by id and by name agree for every
# district in both states (claude-notes/2026-11-15_phase5-federal/).
#
# CCD reports teachers as FTE; negative values are sentinel codes (missing,
# not applicable) and become NA. Only regular districts with at least one
# school count (agency_type 1, number_of_schools >= 1): supervisory unions,
# state agencies and districts with no schools have no staff of their own.

#' `CCD_DISTRICT_DIRECTORY_ENDPOINT`
#' @keywords internal
CCD_DISTRICT_DIRECTORY_ENDPOINT <- "https://educationdata.urban.org/api/v1/school-districts/ccd/directory"

#' `CCD_SCHOOL_DIRECTORY_ENDPOINT`
#' @keywords internal
CCD_SCHOOL_DIRECTORY_ENDPOINT <- "https://educationdata.urban.org/api/v1/schools/ccd/directory"

#' `FRL_MIN_REPORTED_SHARE`
#'
#' A free/reduced-price lunch share is shown only when the schools reporting
#' counts hold at least this share of the entity's enrollment.
#' @keywords internal
FRL_MIN_REPORTED_SHARE <- 0.8

#' Find the latest CCD district directory year for a state
#'
#' `list(year, data)` for the first year back from `start_year` with rows.
#' A failed request stops the search (the caller's safe_scrape() logs it).
#' @param fips Two-digit state FIPS code; defaults to the current state config's.
#' @param start_year,years_back Years searched: `start_year` down to `start_year - years_back`.
#' @return `list(year, data)` for the first year with rows, or `list(year = NA, data = data.frame())`.
#' @export
find_latest_ccd_directory_year <- function(fips = state_fips(), start_year = as.integer(format(Sys.Date(), "%Y")),
                                           years_back = 5) {
  for (year in seq(start_year, start_year - years_back)) {
    df <- fetch_json_paginated(paste0(CCD_DISTRICT_DIRECTORY_ENDPOINT, "/", year, "/?fips=", fips))
    if (nrow(df) > 0) return(list(year = year, data = df))
  }
  list(year = NA_integer_, data = data.frame())
}

#' Parse district teacher FTE and enrollment
#'
#' One row per `district_ids` entry: District, Teachers_Total_FTE and
#' Enrollment summed over the district's LEAIDs (NA when none of them is in
#' `df`, or none reports the figure).
#' @param df Rows from the CCD district directory.
#' @param district_ids Named list, district -> LEAIDs.
#' @return Data frame with one row per `district_ids` entry: `District`, `Teachers_Total_FTE`, `Enrollment`.
#' @export
parse_ccd_teacher_fte <- function(df, district_ids) {
  empty <- data.frame(District = character(0), Teachers_Total_FTE = numeric(0), Enrollment = numeric(0),
                      stringsAsFactors = FALSE)
  if (nrow(df) == 0 || length(district_ids) == 0) return(empty)
  keep <- which(df$agency_type == 1 & !is.na(df$number_of_schools) & df$number_of_schools >= 1)
  lea <- data.frame(leaid = as.character(df$leaid[keep]),
                    fte = ifelse(is.na(df$teachers_total_fte[keep]) | df$teachers_total_fte[keep] < 0, NA_real_,
                                 df$teachers_total_fte[keep]),
                    enr = ifelse(is.na(df$enrollment[keep]) | df$enrollment[keep] < 0, NA_real_, df$enrollment[keep]),
                    stringsAsFactors = FALSE)
  total <- function(v) if (length(v) == 0 || all(is.na(v))) NA_real_ else sum(v, na.rm = TRUE)
  dplyr::bind_rows(lapply(names(district_ids), function(d) {
    m <- lea[lea$leaid %in% as.character(district_ids[[d]]), , drop = FALSE]
    data.frame(District = d, Teachers_Total_FTE = total(m$fte), Enrollment = total(m$enr), stringsAsFactors = FALSE)
  }))
}

#' Fetch district teacher FTE and enrollment for the latest CCD year
#'
#' [parse_ccd_teacher_fte()] plus CCD_Year.
#' @inheritParams parse_ccd_teacher_fte
#' @inheritParams find_latest_ccd_directory_year
#' @return As `parse_ccd_teacher_fte()`, plus `CCD_Year`.
#' @export
fetch_ccd_teacher_fte <- function(district_ids, fips = state_fips()) {
  latest <- find_latest_ccd_directory_year(fips)
  result <- parse_ccd_teacher_fte(latest$data, district_ids)
  if (nrow(result) > 0) result$CCD_Year <- as.character(latest$year)
  result
}

#' Fetch the CCD school directory for a state and year
#' @param fips State FIPS code.
#' @param year Directory year.
#' @return Data frame of the CCD school directory records for that state and year.
#' @export
fetch_ccd_school_directory <- function(fips, year) {
  fetch_json_paginated(paste0(CCD_SCHOOL_DIRECTORY_ENDPOINT, "/", year, "/?fips=", fips))
}

#' Parse the free/reduced-price lunch share
#'
#' `id_map`: one entry per map entity (district or school) -> NCES ids. A
#' 7-digit id is a district (all its schools); a 12-digit id is one school (a
#' charter inside a host district). Share = FRL students / enrollment over the
#' schools reporting counts, NA when those hold under [FRL_MIN_REPORTED_SHARE]
#' of the entity's enrollment.
#' @param schools Rows from [fetch_ccd_school_directory()].
#' @param id_map Named list: map entity -> its NCES ids (7-digit district or 12-digit school).
#' @keywords internal
parse_ccd_frl_share <- function(schools, id_map) {
  empty <- data.frame(District = character(0), Free_Reduced_Lunch_Share = numeric(0),
                      FRL_Schools_Reporting = integer(0), stringsAsFactors = FALSE)
  if (nrow(schools) == 0 || length(id_map) == 0) return(empty)
  s <- data.frame(ncessch = as.character(schools$ncessch), leaid = as.character(schools$leaid),
                  frl = suppressWarnings(as.numeric(schools$free_or_reduced_price_lunch)),
                  enrollment = suppressWarnings(as.numeric(schools$enrollment)), stringsAsFactors = FALSE)
  s <- s[!is.na(s$enrollment) & s$enrollment > 0, , drop = FALSE]
  rows <- lapply(names(id_map), function(entity) {
    ids <- as.character(id_map[[entity]])
    mine <- s[s$leaid %in% ids[nchar(ids) == 7] | s$ncessch %in% ids[nchar(ids) == 12], , drop = FALSE]
    rep <- mine[!is.na(mine$frl) & mine$frl >= 0, , drop = FALSE]
    share <- if (nrow(rep) == 0 || sum(rep$enrollment) < FRL_MIN_REPORTED_SHARE * sum(mine$enrollment)) {
      NA_real_
    } else {
      min(1, sum(rep$frl) / sum(rep$enrollment))
    }
    data.frame(District = entity, Free_Reduced_Lunch_Share = share,
               FRL_Schools_Reporting = nrow(rep), stringsAsFactors = FALSE)
  })
  dplyr::bind_rows(rows)
}

#' Fetch the free/reduced-price lunch share
#'
#' Each entity takes the newest year in which it has a share (FRL_Year says
#' which): some districts skip a year (WY 2024: 8 of 48 reported no counts).
#' When no year's directory answers at all (CCD down), it stops instead of
#' returning blank rows, so safe_scrape() logs an error and the pipeline keeps
#' last run's values (2026-11-09 review: an outage had logged "ok" and blanked
#' every district's share).
#' @param fips State FIPS code.
#' @param id_map See [parse_ccd_frl_share()].
#' @param start_year,years_back Years searched.
#' @return Data frame with one row per `id_map` entity: `District`, `Free_Reduced_Lunch_Share`, `FRL_Schools_Reporting`, `FRL_Year`.
#' @export
fetch_ccd_frl_share <- function(fips, id_map, start_year = as.integer(format(Sys.Date(), "%Y")), years_back = 4) {
  out <- NULL
  answered <- 0L
  errors <- character(0)
  for (year in seq(start_year, start_year - years_back)) {
    todo <- setdiff(names(id_map), out$District)
    if (length(todo) == 0) break
    schools <- tryCatch(fetch_ccd_school_directory(fips, year),
                        error = function(e) { errors <<- c(errors, conditionMessage(e)); data.frame() })
    if (nrow(schools) == 0 || !"free_or_reduced_price_lunch" %in% names(schools)) next
    answered <- answered + 1L
    r <- parse_ccd_frl_share(schools, id_map[todo])
    r <- r[!is.na(r$Free_Reduced_Lunch_Share), , drop = FALSE]
    if (nrow(r) > 0) out <- dplyr::bind_rows(out, transform(r, FRL_Year = as.character(year)))
  }
  if (answered == 0L) {
    stop("fetch_ccd_frl_share(): no CCD school-directory year answered",
         if (length(errors)) paste0(" (", errors[1], ")") else "")
  }
  missing <- setdiff(names(id_map), out$District)
  dplyr::bind_rows(out, data.frame(District = missing, Free_Reduced_Lunch_Share = rep(NA_real_, length(missing)),
                                   FRL_Schools_Reporting = rep(0L, length(missing)),
                                   FRL_Year = rep(NA_character_, length(missing)), stringsAsFactors = FALSE))
}

#' Parse school-level staffing
#'
#' For an entity that isn't its own district in CCD's district directory (a
#' charter inside a host district): teachers and students summed over its
#' schools in the school directory. Same `id_map` as [parse_ccd_frl_share()].
#' @inheritParams parse_ccd_frl_share
#' @return Data frame with one row per `id_map` entity: `District`, `Teachers_Total_FTE`, `Enrollment`.
#' @export
parse_ccd_school_staffing <- function(schools, id_map) {
  if (nrow(schools) == 0 || length(id_map) == 0) {
    return(data.frame(District = character(0), Teachers_Total_FTE = numeric(0), Enrollment = numeric(0)))
  }
  fte <- suppressWarnings(as.numeric(schools$teachers_fte)); enr <- suppressWarnings(as.numeric(schools$enrollment))
  fte[!is.na(fte) & fte < 0] <- NA; enr[!is.na(enr) & enr < 0] <- NA
  dplyr::bind_rows(lapply(names(id_map), function(entity) {
    ids <- as.character(id_map[[entity]])
    i <- as.character(schools$leaid) %in% ids[nchar(ids) == 7] | as.character(schools$ncessch) %in% ids[nchar(ids) == 12]
    data.frame(District = entity,
               Teachers_Total_FTE = if (any(i) && any(!is.na(fte[i]))) sum(fte[i], na.rm = TRUE) else NA_real_,
               Enrollment = if (any(i) && sum(enr[i], na.rm = TRUE) > 0) sum(enr[i], na.rm = TRUE) else NA_real_,
               stringsAsFactors = FALSE)
  }))
}

#' The school districts a point lies in
#'
#' LEAIDs from the Census Bureau's public geocoder (no key): one unified
#' district, or an elementary and a secondary district where a town is split
#' (Butte Elem + Butte H S). NA when the geocoder finds none.
#' @param lat,lon Coordinates.
#' @return Character vector of NCES district ids (LEAIDs) at the point, or NA.
#' @export
geocode_school_district <- function(lat, lon) {
  if (is.na(lat) || is.na(lon)) return(NA_character_)
  resp <- httr2::request("https://geocoding.geo.census.gov/geocoder/geographies/coordinates") %>%
    httr2::req_url_query(x = lon, y = lat, benchmark = "Public_AR_Current", vintage = "Current_Current",
                         layers = "Unified School Districts,Elementary School Districts,Secondary School Districts",
                         format = "json") %>%
    perform_with_retry()
  geo <- jsonlite::fromJSON(httr2::resp_body_string(resp))$result$geographies
  ids <- unlist(lapply(c("Unified School Districts", "Elementary School Districts", "Secondary School Districts"),
                       function(layer) { g <- geo[[layer]]; if (is.data.frame(g) && nrow(g) > 0 && !is.null(g$GEOID)) as.character(g$GEOID) }))
  if (length(ids) == 0) NA_character_ else unique(ids)
}
