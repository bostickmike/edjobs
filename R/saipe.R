# ---------------------------------------------------------------------------
# Census SAIPE school-district child poverty
# ---------------------------------------------------------------------------
#
# Small Area Income and Poverty Estimates, school district level
# (api.census.gov/data/timeseries/poverty/saipe/schdist; needs CENSUS_API_KEY).
# SAIPE estimates "relevant children": ages 5-17 living in the district and of
# the grades it serves. Each row gives the rate (SAEPOVRAT5_17RV_PT, percent),
# the number of relevant children in poverty (SAEPOV5_17RV_PT) and the number
# of relevant children (SAEPOV5_17V_PT); the published rate is the second over
# the third (all 397 Montana rows, 2024). Rows come in three geographies:
# unified (K-12), elementary and secondary districts; a state without
# secondary districts (Wyoming) gets an empty 204 answer for that one.
#
# Matching is by NCES id: the SAIPE district code is the LEAID without its
# two-digit state prefix. `district_ids` is a named list, dashboard district
# -> LEAIDs. Moved from the states' census_saipe_scraper.R (package phase 5,
# 2026-11-15), which matched by name (Montana's 33 hand-written pairs) or by a
# name rewrite (Wyoming); by id gives the same districts in both states.
#
# A dashboard district made of two SAIPE districts (Montana's elementary +
# high school pairs, e.g. Billings Elementary + Billings High School) gets the
# pooled rate: children in poverty over all relevant children, summed across
# its districts -- the share of the combined area's relevant children who are
# in poverty. Census allocates school-age children between separate
# elementary and secondary districts "on the basis of the grade range of the
# district" (SAIPE "2010 - 2024 Overview of School District Estimates",
# https://www.census.gov/programs-surveys/saipe/technical-documentation/methodology/school-districts/overview-school-district.html,
# revised 2026-01-12), so an equal average of the two rates, Montana's rule
# before 2026-11-15, gave the high school grades (9-12) the same weight as the
# elementary grades (K-8). Mike's decision 11-15; on 2024 data the 90
# combined districts move by a median 0.4 and at most 2.5 percentage points.
# One SAIPE district keeps its published rate. The high school district can
# cover more ground than the one elementary district it is paired with, so a
# combined rate still counts some children from outside the dashboard
# district. The same page cautions that "a true poverty 'rate' for children
# cannot be determined from these figures" (the estimates are model-based, for
# related children in families).

#' `SAIPE_SCHDIST_ENDPOINT`
#' @export
SAIPE_SCHDIST_ENDPOINT <- "https://api.census.gov/data/timeseries/poverty/saipe/schdist"

#' `SAIPE_GEOGRAPHIES`
#' @export
SAIPE_GEOGRAPHIES <- c("unified", "elementary", "secondary")

#' Parse a SAIPE API response
#'
#' Header row to column names, values kept as text. An empty or header-only
#' response gives an empty data frame.
#' @param json_text The response body.
#' @export
parse_saipe_json <- function(json_text) {
  if (is.null(json_text) || !nzchar(trimws(json_text))) return(data.frame())
  raw <- jsonlite::fromJSON(json_text)
  if (is.null(raw) || nrow(raw) < 2) return(data.frame())
  body <- as.data.frame(raw[-1, , drop = FALSE], stringsAsFactors = FALSE)
  names(body) <- raw[1, ]
  body
}

#' Fetch one SAIPE school-district geography
#'
#' SD_NAME, the rate and both counts for every district of one geography in
#' the state. A 204 (no districts of that kind) gives an empty data frame.
#' @param geocat One of [SAIPE_GEOGRAPHIES].
#' @param year SAIPE year.
#' @param api_key See [census_api_key()].
#' @param fips Two-digit state FIPS code; defaults to the current state config's.
#' @export
fetch_saipe_geography <- function(geocat, year, api_key = census_api_key(), fips = state_fips()) {
  resp <- httr2::request(SAIPE_SCHDIST_ENDPOINT) %>%
    httr2::req_url_query(get = "SD_NAME,SAEPOVRAT5_17RV_PT,SAEPOV5_17RV_PT,SAEPOV5_17V_PT",
                         `for` = paste0("school district (", geocat, ")"), `in` = paste0("state:", fips),
                         time = year, key = api_key) %>%
    perform_with_retry()
  if (!httr2::resp_has_body(resp)) return(data.frame())
  parse_saipe_json(httr2::resp_body_string(resp))
}

#' Latest SAIPE year with data for a state
#'
#' Walks back from `start_year` on the unified geography; errors when none of
#' the years answers.
#' @param start_year,years_back Years tried.
#' @inheritParams fetch_saipe_geography
#' @export
latest_saipe_year <- function(start_year = as.integer(format(Sys.Date(), "%Y")), years_back = 4,
                              api_key = census_api_key(), fips = state_fips()) {
  for (year in seq(start_year, start_year - years_back)) {
    result <- tryCatch(fetch_saipe_geography("unified", year, api_key, fips), error = function(e) data.frame())
    if (nrow(result) > 0) return(year)
  }
  stop("latest_saipe_year(): no working SAIPE vintage found in the last ", years_back, " years.")
}

#' SAIPE rows keyed by NCES id
#'
#' One row per SAIPE district: LEAID, SD_NAME, Child_Poverty_Rate (0-1),
#' SAIPE_Poor_5_17, SAIPE_Children_5_17.
#' @param geos A named list of [fetch_saipe_geography()] results (names from [SAIPE_GEOGRAPHIES]).
#' @param fips State FIPS code, the LEAID prefix.
#' @export
saipe_rows_by_leaid <- function(geos, fips) {
  rows <- lapply(names(geos), function(g) {
    df <- geos[[g]]
    code_col <- paste0("school district (", g, ")")
    if (is.null(df) || nrow(df) == 0 || !code_col %in% names(df)) return(NULL)
    num <- function(col) if (col %in% names(df)) clean_acs_value(suppressWarnings(as.numeric(df[[col]]))) else rep(NA_real_, nrow(df))
    data.frame(LEAID = paste0(fips, df[[code_col]]), SD_NAME = trimws(df$SD_NAME),
               Child_Poverty_Rate = num("SAEPOVRAT5_17RV_PT") / 100,
               SAIPE_Poor_5_17 = num("SAEPOV5_17RV_PT"), SAIPE_Children_5_17 = num("SAEPOV5_17V_PT"),
               stringsAsFactors = FALSE)
  })
  out <- dplyr::bind_rows(rows)
  if (nrow(out) == 0) {
    return(data.frame(LEAID = character(0), SD_NAME = character(0), Child_Poverty_Rate = numeric(0),
                      SAIPE_Poor_5_17 = numeric(0), SAIPE_Children_5_17 = numeric(0), stringsAsFactors = FALSE))
  }
  out
}

#' Fetch SAIPE child poverty for every district in a state, by NCES id
#'
#' [saipe_rows_by_leaid()] over all three geographies, plus SAIPE_Year.
#' @param year SAIPE year; the latest when NULL.
#' @inheritParams fetch_saipe_geography
#' @export
fetch_saipe_by_leaid <- function(year = NULL, api_key = census_api_key(), fips = state_fips()) {
  year <- year %||% latest_saipe_year(api_key = api_key, fips = fips)
  geos <- stats::setNames(lapply(SAIPE_GEOGRAPHIES, function(g) fetch_saipe_geography(g, year, api_key, fips)),
                          SAIPE_GEOGRAPHIES)
  out <- saipe_rows_by_leaid(geos, fips)
  out$SAIPE_Year <- rep(as.integer(year), nrow(out))
  out
}

#' Pool child poverty over several SAIPE districts
#'
#' One district: its published rate. Several: children in poverty over
#' relevant children, summed over the districts with both counts; when no
#' district has both, the mean of their rates. NA when none has a rate.
#' @param rows Rows of [saipe_rows_by_leaid()] (rate, and the counts when known).
#' @export
pool_child_poverty <- function(rows) {
  ok <- rows[!is.na(rows$Child_Poverty_Rate), , drop = FALSE]
  if (nrow(ok) == 0) return(NA_real_)
  if (nrow(ok) == 1) return(ok$Child_Poverty_Rate)
  if (all(c("SAIPE_Poor_5_17", "SAIPE_Children_5_17") %in% names(ok))) {
    counted <- ok[!is.na(ok$SAIPE_Poor_5_17) & !is.na(ok$SAIPE_Children_5_17), , drop = FALSE]
    if (nrow(counted) > 0 && sum(counted$SAIPE_Children_5_17) > 0) {
      return(sum(counted$SAIPE_Poor_5_17) / sum(counted$SAIPE_Children_5_17))
    }
  }
  mean(ok$Child_Poverty_Rate)
}

#' Child poverty per dashboard district
#'
#' District, Child_Poverty_Rate ([pool_child_poverty()] over the district's
#' LEAIDs; NA when none is in `saipe`), SAIPE_Year (NA where the rate is).
#' @param saipe Output of [fetch_saipe_by_leaid()].
#' @param district_ids Named list, district -> LEAIDs.
#' @export
parse_census_saipe_child_poverty <- function(saipe, district_ids) {
  if (length(district_ids) == 0 || nrow(saipe) == 0) {
    return(data.frame(District = character(0), Child_Poverty_Rate = numeric(0), SAIPE_Year = integer(0),
                      stringsAsFactors = FALSE))
  }
  year <- if ("SAIPE_Year" %in% names(saipe)) saipe$SAIPE_Year[1] else NA_integer_
  rate <- vapply(district_ids, function(ids) pool_child_poverty(saipe[saipe$LEAID %in% as.character(ids), , drop = FALSE]),
                 numeric(1), USE.NAMES = FALSE)
  # No year for a district with no rate (WY's Northeast Wyoming BOCES has an NCES id but no SAIPE geography).
  data.frame(District = names(district_ids), Child_Poverty_Rate = rate,
             SAIPE_Year = ifelse(is.na(rate), NA_integer_, as.integer(year)), stringsAsFactors = FALSE)
}

#' Fetch child poverty per dashboard district
#' @inheritParams parse_census_saipe_child_poverty
#' @inheritParams fetch_saipe_by_leaid
#' @export
fetch_census_saipe_child_poverty <- function(district_ids, year = NULL, api_key = census_api_key(), fips = state_fips()) {
  parse_census_saipe_child_poverty(fetch_saipe_by_leaid(year, api_key, fips), district_ids)
}

#' A school's surrounding-district child poverty rate
#'
#' For a charter, private, tribal or early-childhood school with no SAIPE
#' geography of its own: the districts `geocode()` finds at its point, pooled
#' with [pool_child_poverty()] where a town is split into elementary and
#' secondary districts. `saipe`: LEAID, Child_Poverty_Rate (and SAIPE_Year,
#' and the counts for pooling). `district_names`: LEAID -> display name.
#' Returns status "ok", "none" (no district found / no rate) or "failed" (the
#' geocoder errored: the caller keeps last run's value and logs it).
#' @param lat,lon The school's coordinates.
#' @param saipe See the description.
#' @param district_names See the description.
#' @param geocode The lookup; [geocode_school_district()].
#' @export
surrounding_district_poverty <- function(lat, lon, saipe, district_names = NULL, geocode = geocode_school_district) {
  res <- list(status = "none", rate = NA_real_, year = NA, basis = NA_character_)
  host <- tryCatch(geocode(lat, lon), error = function(e) structure(NA_character_, failed = TRUE))
  if (isTRUE(attr(host, "failed"))) { res$status <- "failed"; return(res) }
  h <- match(host, as.character(saipe$LEAID))
  h <- h[!is.na(h) & !is.na(saipe$Child_Poverty_Rate[h])]
  if (length(h) == 0) return(res)
  ids <- as.character(saipe$LEAID[h])
  nm <- if (!is.null(district_names)) unique(ifelse(is.na(district_names[ids]), ids, district_names[ids])) else ids
  res$status <- "ok"
  res$rate <- pool_child_poverty(saipe[h, , drop = FALSE])
  if ("SAIPE_Year" %in% names(saipe)) res$year <- saipe$SAIPE_Year[h[1]]
  res$basis <- paste("Surrounding district:", paste(nm, collapse = " / "))
  res
}
