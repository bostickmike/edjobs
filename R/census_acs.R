# ---------------------------------------------------------------------------
# Census ACS 5-year county context
# ---------------------------------------------------------------------------
#
# County median household income (B19013), median gross rent (B25064),
# population (B01003), the share of civilian employed people 16 and over who
# work in mining, quarrying, and oil and gas extraction (S2403_C01_004E over
# S2403_C01_001E; the Census API's own labels, checked 2026-11-15 -- line 4 is
# mining only, not the combined agriculture-and-mining line), and the
# population change over five years. Joined to K-12
# districts and to colleges by county. Moved from the states'
# census_acs_scraper.R (package phase 5); the copies differed only in the state
# FIPS code and in Montana stripping ", Montana" from the county name
# (Wyoming's tables keep "Albany County, Wyoming"), now `county_suffix`.
#
# Needs a free Census API key in CENSUS_API_KEY (a GitHub Actions secret for
# the weekly runs, .Renviron locally). ACS responses are a JSON array of
# arrays with a header row, every value a string; negative values are Census
# sentinel codes (e.g. -666666666 for "not available") and become NA.

#' `ACS_DETAIL_ENDPOINT`
#' @export
ACS_DETAIL_ENDPOINT <- "https://api.census.gov/data/%d/acs/acs5"

#' `ACS_SUBJECT_ENDPOINT`
#' @export
ACS_SUBJECT_ENDPOINT <- "https://api.census.gov/data/%d/acs/acs5/subject"

#' Census API key
#'
#' `CENSUS_API_KEY`, or an error saying where to get one.
#' @export
census_api_key <- function() {
  key <- Sys.getenv("CENSUS_API_KEY")
  if (!nzchar(key)) {
    stop(
      "CENSUS_API_KEY environment variable is not set -- get a free key at ",
      "https://api.census.gov/data/key_signup.html and set it as an environment ",
      "variable (a GitHub Actions secret for the weekly pipeline, .Renviron locally)."
    )
  }
  key
}

#' Parse a Census API JSON response
#'
#' The header row becomes the column names; every column except NAME, state
#' and county becomes numeric. An empty or header-only response gives an
#' empty data frame.
#' @param json_text The response body.
#' @export
parse_acs_json <- function(json_text) {
  raw <- jsonlite::fromJSON(json_text)
  if (is.null(raw) || nrow(raw) < 2) return(data.frame())
  body <- as.data.frame(raw[-1, , drop = FALSE], stringsAsFactors = FALSE)
  names(body) <- raw[1, ]
  for (col in setdiff(names(body), c("NAME", "state", "county"))) {
    body[[col]] <- suppressWarnings(as.numeric(body[[col]]))
  }
  body
}

#' Clean a Census value
#'
#' Negative sentinel codes and NA become NA.
#' @param x A numeric vector.
#' @export
clean_acs_value <- function(x) ifelse(is.na(x) | x < 0, NA_real_, x)

#' Fetch ACS variables for every county in a state
#' @param endpoint_template [ACS_DETAIL_ENDPOINT] or [ACS_SUBJECT_ENDPOINT].
#' @param get_vars Variable names, NAME included.
#' @param year ACS 5-year vintage.
#' @param api_key See [census_api_key()].
#' @param fips Two-digit state FIPS code; defaults to the current state config's.
#' @export
fetch_acs_counties <- function(endpoint_template, get_vars, year, api_key, fips = state_fips()) {
  resp <- httr2::request(sprintf(endpoint_template, year)) %>%
    httr2::req_url_query(get = paste(get_vars, collapse = ","), `for` = "county:*",
                         `in` = paste0("state:", fips), key = api_key) %>%
    perform_with_retry()
  parse_acs_json(httr2::resp_body_string(resp))
}

county_label <- function(name, county_suffix) {
  if (is.null(county_suffix) || !nzchar(county_suffix)) return(name)
  n <- nchar(county_suffix)
  ends <- substr(name, nchar(name) - n + 1, nchar(name)) == county_suffix
  ifelse(ends, substr(name, 1, nchar(name) - n), name)
}

#' Parse ACS income, rent and population
#'
#' County, Median_Household_Income, Median_Gross_Rent, Population, ACS_Year.
#' @param raw Output of [fetch_acs_counties()] for B19013_001E, B25064_001E, B01003_001E.
#' @param year ACS vintage.
#' @param county_suffix Text removed from the end of Census's county NAME
#'   (", Montana"); NULL keeps the full name.
#' @export
parse_census_income_rent_population <- function(raw, year, county_suffix = NULL) {
  if (nrow(raw) == 0) {
    return(data.frame(County = character(0), Median_Household_Income = numeric(0),
                      Median_Gross_Rent = numeric(0), Population = numeric(0),
                      ACS_Year = integer(0), stringsAsFactors = FALSE))
  }
  data.frame(County = county_label(raw$NAME, county_suffix),
             Median_Household_Income = clean_acs_value(raw$B19013_001E),
             Median_Gross_Rent = clean_acs_value(raw$B25064_001E),
             Population = clean_acs_value(raw$B01003_001E),
             ACS_Year = rep(year, nrow(raw)), stringsAsFactors = FALSE)
}

#' Parse the ACS mining/energy employment share
#'
#' County, Mining_Employment_Share (S2403 line 4, mining, quarrying, and oil and
#' gas extraction, over all civilian employed 16 and over).
#' @param raw Output of [fetch_acs_counties()] for S2403_C01_001E, S2403_C01_004E.
#' @inheritParams parse_census_income_rent_population
#' @export
parse_census_mining_employment_share <- function(raw, county_suffix = NULL) {
  if (nrow(raw) == 0) {
    return(data.frame(County = character(0), Mining_Employment_Share = numeric(0), stringsAsFactors = FALSE))
  }
  total <- clean_acs_value(raw$S2403_C01_001E)
  mining <- clean_acs_value(raw$S2403_C01_004E)
  data.frame(County = county_label(raw$NAME, county_suffix),
             Mining_Employment_Share = ifelse(!is.na(total) & total > 0, mining / total, NA_real_),
             stringsAsFactors = FALSE)
}

#' Fetch ACS income, rent and population
#' @inheritParams parse_census_income_rent_population
#' @inheritParams fetch_acs_counties
#' @export
fetch_census_income_rent_population <- function(year = NULL, api_key = census_api_key(), fips = state_fips(),
                                                 county_suffix = NULL) {
  year <- year %||% latest_acs5_year(api_key = api_key, fips = fips)
  raw <- fetch_acs_counties(ACS_DETAIL_ENDPOINT, c("NAME", "B19013_001E", "B25064_001E", "B01003_001E"),
                            year, api_key, fips)
  parse_census_income_rent_population(raw, year, county_suffix)
}

#' Fetch the ACS mining/energy employment share
#' @inheritParams fetch_census_income_rent_population
#' @export
fetch_census_mining_employment_share <- function(year = NULL, api_key = census_api_key(), fips = state_fips(),
                                                 county_suffix = NULL) {
  year <- year %||% latest_acs5_year(api_key = api_key, fips = fips)
  raw <- fetch_acs_counties(ACS_SUBJECT_ENDPOINT, c("NAME", "S2403_C01_001E", "S2403_C01_004E"), year, api_key, fips)
  parse_census_mining_employment_share(raw, county_suffix)
}

#' Compute the change in county population
#'
#' County, Population_Change_Pct ((current - prior) / prior; NA when either
#' is missing or the prior is 0), for counties in both frames.
#' @param current,prior Outputs of [parse_census_income_rent_population()].
#' @export
compute_population_change <- function(current, prior) {
  if (nrow(current) == 0 || nrow(prior) == 0) {
    return(data.frame(County = character(0), Population_Change_Pct = numeric(0), stringsAsFactors = FALSE))
  }
  both <- dplyr::inner_join(as.data.frame(current)[, c("County", "Population")],
                            stats::setNames(as.data.frame(prior)[, c("County", "Population")], c("County", "Population_Prior")),
                            by = "County")
  ok <- !is.na(both$Population) & !is.na(both$Population_Prior) & both$Population_Prior > 0
  data.frame(County = both$County,
             Population_Change_Pct = ifelse(ok, (both$Population - both$Population_Prior) / both$Population_Prior, NA_real_),
             stringsAsFactors = FALSE)
}

#' Latest ACS 5-year vintage with data
#'
#' Walks back from `start_year`; errors when no vintage in the window answers.
#' @param start_year,years_back Vintages tried: `start_year` down to `start_year - years_back`.
#' @inheritParams fetch_acs_counties
#' @export
latest_acs5_year <- function(start_year = as.integer(format(Sys.Date(), "%Y")), years_back = 3,
                             api_key = census_api_key(), fips = state_fips()) {
  for (year in seq(start_year, start_year - years_back)) {
    result <- tryCatch(fetch_acs_counties(ACS_DETAIL_ENDPOINT, c("NAME", "B01003_001E"), year, api_key, fips),
                       error = function(e) data.frame())
    if (nrow(result) > 0) return(year)
  }
  stop("latest_acs5_year(): no working ACS 5-Year vintage found in the last ", years_back, " years.")
}

#' Fetch the county context table
#'
#' The latest vintage's income, rent and population, joined with the mining
#' share and the population change from `trend_years_back` vintages earlier.
#' @inheritParams fetch_census_income_rent_population
#' @param trend_years_back See the description.
#' @export
fetch_census_county_context <- function(api_key = census_api_key(), trend_years_back = 5, fips = state_fips(),
                                        county_suffix = NULL) {
  year <- latest_acs5_year(api_key = api_key, fips = fips)
  current <- fetch_census_income_rent_population(year, api_key, fips, county_suffix)
  prior <- fetch_census_income_rent_population(year - trend_years_back, api_key, fips, county_suffix)
  mining <- fetch_census_mining_employment_share(year, api_key, fips, county_suffix)
  current %>%
    dplyr::left_join(mining, by = "County") %>%
    dplyr::left_join(compute_population_change(current, prior), by = "County")
}
