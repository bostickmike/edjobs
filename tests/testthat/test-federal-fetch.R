# The fetch layer of ipeds_he.R and census_acs.R: which URL and FIPS code
# each call uses, the latest-year search, and the state config default.

test_that("the IPEDS fetchers put the state's FIPS code in the URL and walk back to the first year with rows", {
  urls <- character(0)
  local_mocked_bindings(fetch_json_paginated = function(url, ...) {
    urls <<- c(urls, url)
    if (grepl("/2024/", url)) read.csv(test_path("fixtures", "ipeds_mt_salaries_2024.csv")) else data.frame()
  })
  out <- fetch_ipeds_he_salaries(MT_TEST_UNITIDS, fips = "30")
  expect_equal(urls, paste0(IPEDS_SALARY_ENDPOINT, "/", seq(as.integer(format(Sys.Date(), "%Y")), 2024), "/?fips=30"))
  expect_equal(nrow(out), 22)
  expect_equal(unique(out$Salary_Year), "2024")
})

test_that("a failing salary year counts as empty, but a failing enrollment year stops the search (both states' behavior)", {
  local_mocked_bindings(fetch_json_paginated = function(url, ...) stop("HTTP 524"))
  expect_true(is.na(find_latest_ipeds_salary_year("56", start_year = 2024, years_back = 1)$year))
  expect_error(find_latest_ipeds_enrollment_year("56", start_year = 2024, years_back = 1), "524")
})

test_that("the Pell share uses enrollment from the grants year, not the latest enrollment year", {
  urls <- character(0)
  local_mocked_bindings(fetch_json_paginated = function(url, ...) {
    urls <<- c(urls, url)
    if (grepl("fsa/grants/2021/", url)) return(read.csv(test_path("fixtures", "fsa_wy_grants_2021.csv")))
    if (grepl("fall-enrollment/2021/", url)) return(read.csv(test_path("fixtures", "ipeds_wy_fall_enrollment_2021.csv")))
    data.frame()
  })
  out <- fetch_ipeds_he_pell_share(WY_TEST_UNITIDS, fips = "56")
  expect_true(paste0(IPEDS_ENROLLMENT_ENDPOINT, "/2021/?fips=56") %in% urls)
  expect_equal(out$Pell_Recipient_Share[out$Name == "Casper College"], 932 / 2277, tolerance = 1e-6)
})

test_that("the enrollment trend compares the latest year with trend_years_back years earlier", {
  local_mocked_bindings(fetch_json_paginated = function(url, ...) {
    if (grepl("/2023/", url)) return(read.csv(test_path("fixtures", "ipeds_wy_fall_enrollment_2023.csv")))
    if (grepl("/2018/", url)) return(read.csv(test_path("fixtures", "ipeds_wy_fall_enrollment_2018.csv")))
    data.frame()
  })
  out <- fetch_ipeds_he_enrollment_trend(WY_TEST_UNITIDS, fips = "56")
  expect_equal(out$Enrollment_Change_Pct[out$Name == "University of Wyoming"], (9385 - 10878) / 10878, tolerance = 1e-6)
})

test_that("fips defaults to the state config's, with a clear error when none is set", {
  old <- current_state_config()
  withr::defer(set_state_config(old))
  set_state_config(NULL)
  expect_error(state_fips(), "No state config set")
  set_state_config(state_config("WY", "56", "Wy_Ed_Jobs"))
  expect_equal(state_fips(), "56")
  urls <- character(0)
  local_mocked_bindings(fetch_json_paginated = function(url, ...) { urls <<- c(urls, url); data.frame() })
  fetch_ipeds_he_enrollment_trend(WY_TEST_UNITIDS, trend_years_back = 5)
  expect_true(all(grepl("fips=56$", urls)))
})

test_that("parse functions refuse a unitid map without unitid and Name", {
  expect_error(parse_ipeds_he_enrollment(data.frame(x = 1), 2023, data.frame(id = 1)), "unitid and Name")
})

test_that("fetch_census_county_context joins the latest vintage, the mining share and the 5-year population change", {
  calls <- list()
  latest <- as.integer(format(Sys.Date(), "%Y")) - 1L   # the fixtures stand in for "last year" and 5 years before
  fixture <- function(f) parse_acs_json(paste(readLines(test_path("fixtures", f), warn = FALSE), collapse = "\n"))
  local_mocked_bindings(fetch_acs_counties = function(endpoint_template, get_vars, year, api_key, fips) {
    calls[[length(calls) + 1]] <<- list(endpoint = endpoint_template, year = year, fips = fips)
    if (year > latest) return(data.frame())
    if (endpoint_template == ACS_SUBJECT_ENDPOINT) return(fixture("acs_mt_subject_2022.json"))
    if (year == latest) fixture("acs_mt_detail_2022.json") else fixture("acs_mt_detail_2017.json")
  })
  out <- fetch_census_county_context(api_key = "test", fips = "30", county_suffix = ", Montana")
  expect_true(all(vapply(calls, `[[`, "", "fips") == "30"))
  expect_equal(unique(out$ACS_Year), latest)
  expect_true((latest - 5L) %in% vapply(calls, `[[`, 0, "year"))
  expect_equal(names(out), c("County", "Median_Household_Income", "Median_Gross_Rent", "Population", "ACS_Year",
                             "Mining_Employment_Share", "Population_Change_Pct"))
  ys <- out[out$County == "Yellowstone County", ]
  expect_equal(ys$Median_Household_Income, 72300)
  expect_false(is.na(ys$Population_Change_Pct))
})

test_that("fetch_acs_counties asks the Census API for the given state's counties", {
  seen <- NULL
  local_mocked_bindings(perform_with_retry = function(req, ...) {
    seen <<- req$url
    httr2::response(200, body = charToRaw('[["NAME","B01003_001E","state","county"],["Albany County, Wyoming","38000","56","001"]]'))
  })
  out <- fetch_acs_counties(ACS_DETAIL_ENDPOINT, c("NAME", "B01003_001E"), 2024, "k", fips = "56")
  expect_match(seen, "api.census.gov/data/2024/acs/acs5\\?")
  expect_match(seen, "in=state%3A56")
  expect_match(seen, "for=county%3A%2A")
  expect_equal(out$B01003_001E, 38000)
})
