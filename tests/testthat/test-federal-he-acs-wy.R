# Moved from the state repos (package phase 5, 2026-11-15) with their real
# Urban Institute / Census fixtures; the unitid maps are the states' own, copied
# into helper-unitid-maps.R as test data.

# ---- from WY's tests/testthat/test-ipeds-salary-scraper.R ----
# Real fixture: the actual Wyoming (fips=56) response from the Urban
# Institute's Education Data Portal salaries-instructional-staff/2024
# endpoint, captured 2026-08-04 -- all 1008 rows across 9 institutions'
# academic_rank x contract_length x sex combinations, not synthetic.

test_that("parse_ipeds_he_salaries extracts all 9 Wyoming institutions with correct headline salaries", {
  df <- read.csv(test_path("fixtures", "ipeds_wy_salaries_2024.csv"))
  result <- parse_ipeds_he_salaries(df, 2024, WY_TEST_UNITIDS)

  expect_equal(nrow(result), 9)
  expect_setequal(result$Name, c(
    "Casper College", "Central Wyoming College", "Eastern Wyoming Community College",
    "Laramie County Community College", "Northwest College", "Sheridan College",
    "Gillette College", "Western Wyoming Community College", "University of Wyoming"
  ))
  expect_true(all(result$Salary_Year == "2024"))

  uw <- result[result$Name == "University of Wyoming", ]
  expect_equal(uw$Faculty_Avg_Salary, 98302.828571, tolerance = 1e-4)
  expect_equal(uw$Faculty_Avg_Salary_Professor, 127093.120879, tolerance = 1e-4)
  expect_equal(uw$Faculty_Count, 700)
  expect_true(is.na(uw$Salary_Note))
})

test_that("parse_ipeds_he_salaries surfaces the Sheridan/Gillette shared-reporting caveat instead of hiding it", {
  df <- read.csv(test_path("fixtures", "ipeds_wy_salaries_2024.csv"))
  result <- parse_ipeds_he_salaries(df, 2024, WY_TEST_UNITIDS)

  sheridan <- result[result$Name == "Sheridan College", ]
  gillette <- result[result$Name == "Gillette College", ]

  # Same underlying IPEDS unitid (Northern Wyoming Community College
  # District) -- figures are identical, and both carry a note explaining why.
  expect_equal(sheridan$Faculty_Avg_Salary, gillette$Faculty_Avg_Salary)
  expect_false(is.na(sheridan$Salary_Note))
  expect_false(is.na(gillette$Salary_Note))
  expect_match(sheridan$Salary_Note, "Northern Wyoming Community College District")
})

test_that("parse_ipeds_he_salaries treats IPEDS negative sentinel codes as NA, not real values", {
  df <- read.csv(test_path("fixtures", "ipeds_wy_salaries_2024.csv"))
  result <- parse_ipeds_he_salaries(df, 2024, WY_TEST_UNITIDS)

  # Casper College and Laramie County Community College are two-year
  # institutions with no full "Professor" rank reported in the source data
  # (confirmed against the raw fixture) -- this should surface as a genuine
  # NA, not a negative sentinel value or a silently dropped row.
  casper <- result[result$Name == "Casper College", ]
  lccc <- result[result$Name == "Laramie County Community College", ]
  expect_true(is.na(casper$Faculty_Avg_Salary_Professor))
  expect_true(is.na(lccc$Faculty_Avg_Salary_Professor))
  expect_false(is.na(casper$Faculty_Avg_Salary))
})

test_that("parse_ipeds_he_salaries returns an empty, correctly-shaped frame when given no rows", {
  result <- parse_ipeds_he_salaries(data.frame(), 2024, WY_TEST_UNITIDS)
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("Name", "Faculty_Avg_Salary", "Faculty_Avg_Salary_Professor",
                                 "Faculty_Count", "Salary_Year", "Salary_Note"))
})

test_that("parse_ipeds_salary_trend_year extracts one row per institution with the given year attached", {
  df <- read.csv(test_path("fixtures", "ipeds_wy_salaries_2024.csv"))
  result <- parse_ipeds_salary_trend_year(df, 2024, WY_TEST_UNITIDS)

  expect_equal(nrow(result), 9)
  expect_true(all(result$Year == 2024))

  uw <- result[result$Name == "University of Wyoming", ]
  expect_equal(uw$Faculty_Avg_Salary, 98302.828571, tolerance = 1e-4)
})

test_that("parse_ipeds_salary_trend_year returns an empty, correctly-shaped frame when given no rows", {
  result <- parse_ipeds_salary_trend_year(data.frame(), 2024, WY_TEST_UNITIDS)
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("Name", "Year", "Faculty_Avg_Salary"))
})

test_that("fetch_json_paginated handles JSON null fields across paginated pages without error", {
  page1 <- "{\"count\": 2, \"next\": \"https://api.example.com/page2\", \"results\": [{\"unitid\": 240727, \"year\": 2023, \"average_salary\": 98000, \"number_applied\": null}]}"
  page2 <- "{\"count\": 2, \"next\": null, \"results\": [{\"unitid\": 240693, \"year\": 2023, \"average_salary\": 65000, \"number_applied\": 100}]}"

  httr2::local_mocked_responses(list(
    httr2::response(200, headers = list("Content-Type" = "application/json"), body = charToRaw(page1)),
    httr2::response(200, headers = list("Content-Type" = "application/json"), body = charToRaw(page2))
  ))

  result <- fetch_json_paginated("https://api.example.com/page1")
  expect_equal(nrow(result), 2)
  expect_equal(result$unitid, c(240727, 240693))
  expect_true(is.na(result$number_applied[1]))
  expect_equal(result$number_applied[2], 100)
})

# ---- from WY's tests/testthat/test-ipeds-enrollment-scraper.R ----
# Real fixture: the actual Wyoming (fips=56) response from the Urban
# Institute's Education Data Portal fall-enrollment/2023 endpoint,
# captured 2026-08-06 -- all 30 rows across 10 unitids' level_of_study
# breakdowns (8 of which are in IPEDS_UNITID_MAP; the other 2 -- 240709,
# 240718 -- are real WY institutions outside this project's 9-institution
# scope and correctly dropped by the left_join), not synthetic.

test_that("parse_ipeds_he_enrollment sums FTE across level_of_study into one Enrollment figure per institution", {
  df <- read.csv(test_path("fixtures", "ipeds_wy_fall_enrollment_2023.csv"))
  result <- parse_ipeds_he_enrollment(df, 2023, WY_TEST_UNITIDS)

  expect_equal(nrow(result), 9)
  expect_setequal(result$Name, c(
    "Casper College", "Central Wyoming College", "Eastern Wyoming Community College",
    "Laramie County Community College", "Northwest College", "Sheridan College",
    "Gillette College", "Western Wyoming Community College", "University of Wyoming"
  ))
  expect_true(all(result$Enrollment_Year == "2023"))

  # University of Wyoming has real FTE reported across all three levels in
  # the fixture (Undergraduate 7465, Graduate 1503, First professional --
  # est_fte is -1/missing but rep_fte is a real 417) -- summing them is the
  # whole point of this function, and specifically requires coalescing to
  # rep_fte for the level where est_fte is missing, not silently dropping it.
  uw <- result[result$Name == "University of Wyoming", ]
  expect_equal(uw$Enrollment, 7465 + 1503 + 417)

  # Casper College only offers undergraduate (its Graduate/First-
  # professional rows are -2 "not applicable" in the real fixture) --
  # Enrollment should be exactly that one real level's FTE, not NA.
  casper <- result[result$Name == "Casper College", ]
  expect_equal(casper$Enrollment, 2192)
})

test_that("parse_ipeds_he_enrollment gives Sheridan and Gillette the same shared Enrollment figure", {
  df <- read.csv(test_path("fixtures", "ipeds_wy_fall_enrollment_2023.csv"))
  result <- parse_ipeds_he_enrollment(df, 2023, WY_TEST_UNITIDS)

  sheridan <- result[result$Name == "Sheridan College", ]
  gillette <- result[result$Name == "Gillette College", ]
  expect_equal(sheridan$Enrollment, gillette$Enrollment)
  expect_equal(sheridan$Enrollment, 1852)
})

test_that("parse_ipeds_he_enrollment treats IPEDS negative sentinel codes as NA, not real values", {
  # A level_of_study an institution doesn't offer at all (both est_fte and
  # rep_fte negative) must contribute nothing to the sum, not be coerced
  # into 0 counted as a real reported figure or NA'ing out the whole total.
  df <- data.frame(
    unitid = c(240505L, 240505L, 240505L),
    level_of_study = c(1L, 2L, 3L),
    est_fte = c(2192L, -2L, -1L),
    rep_fte = c(2192L, -2L, -2L)
  )
  result <- parse_ipeds_he_enrollment(df, 2023, WY_TEST_UNITIDS)
  casper <- result[result$Name == "Casper College", ]
  expect_equal(casper$Enrollment, 2192)
})

test_that("parse_ipeds_he_enrollment returns NA (not 0) for an institution with no real FTE data at all", {
  df <- data.frame(
    unitid = c(240505L, 240505L),
    level_of_study = c(1L, 2L),
    est_fte = c(-1L, -2L),
    rep_fte = c(-1L, -2L)
  )
  result <- parse_ipeds_he_enrollment(df, 2023, WY_TEST_UNITIDS)
  casper <- result[result$Name == "Casper College", ]
  expect_true(is.na(casper$Enrollment))
})

test_that("parse_ipeds_he_enrollment returns an empty, correctly-shaped frame when given no rows", {
  result <- parse_ipeds_he_enrollment(data.frame(), 2023, WY_TEST_UNITIDS)
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("Name", "Enrollment", "Enrollment_Year"))
})

# Real fixture: the actual Wyoming (fips=56) response from the same
# endpoint for 2018, captured 2026-08-06 -- used together with the 2023
# fixture above for a real 5-year enrollment-trend comparison.

test_that("compute_enrollment_change computes a real 5-year trend matching independently-verified figures", {
  current_df <- read.csv(test_path("fixtures", "ipeds_wy_fall_enrollment_2023.csv"))
  prior_df <- read.csv(test_path("fixtures", "ipeds_wy_fall_enrollment_2018.csv"))
  current <- parse_ipeds_he_enrollment(current_df, 2023, WY_TEST_UNITIDS)
  prior <- parse_ipeds_he_enrollment(prior_df, 2018, WY_TEST_UNITIDS)

  result <- compute_enrollment_change(current, prior)
  expect_equal(nrow(result), 9)

  # University of Wyoming: 10,878 FTE (2018) -> 9,385 FTE (2023), a real,
  # independently-computed -13.7% decline -- cross-checked by hand against
  # the raw fixtures before this test was written, not derived from the
  # function under test.
  uw <- result[result$Name == "University of Wyoming", ]
  expect_equal(uw$Enrollment_Change_Pct, (9385 - 10878) / 10878, tolerance = 1e-6)

  # Central Wyoming College is the one WY institution that GREW over this
  # window (1,006 -> 1,030 FTE) -- confirms this isn't hardcoded to always
  # be negative.
  cwc <- result[result$Name == "Central Wyoming College", ]
  expect_true(cwc$Enrollment_Change_Pct > 0)
})

test_that("compute_enrollment_change returns an empty, correctly-shaped frame when either side has no rows", {
  some_rows <- data.frame(Name = "Casper College", Enrollment = 2192, Enrollment_Year = "2023")
  expect_equal(nrow(compute_enrollment_change(data.frame(), some_rows)), 0)
  expect_equal(nrow(compute_enrollment_change(some_rows, data.frame())), 0)
  expect_equal(names(compute_enrollment_change(data.frame(), data.frame())), c("Name", "Enrollment_Change_Pct"))
})

# ---- from WY's tests/testthat/test-fsa-pell-scraper.R ----
# Real fixtures: the actual Wyoming (fips=56) responses from the Urban
# Institute's Education Data Portal fsa/grants/2021 and ipeds/fall-
# enrollment/2021 endpoints, captured 2026-08-06 -- 2021 is FSA's latest
# available year (confirmed live: 2022/2023 both return zero results),
# matched to the SAME year's enrollment fixture rather than the more
# recent 2023 one used elsewhere, per this scraper's own reasoning for
# why the years must line up.

test_that("parse_ipeds_he_pell_share computes a real recipients/enrollment share matching independently-verified figures", {
  grants_df <- read.csv(test_path("fixtures", "fsa_wy_grants_2021.csv"))
  enrollment_raw <- read.csv(test_path("fixtures", "ipeds_wy_fall_enrollment_2021.csv"))
  enrollment_df <- parse_ipeds_he_enrollment(enrollment_raw, 2021, WY_TEST_UNITIDS)

  result <- parse_ipeds_he_pell_share(grants_df, enrollment_df, 2021, WY_TEST_UNITIDS)
  expect_equal(nrow(result), 9)
  expect_true(all(result$Pell_Year == "2021"))

  # Casper College: 932 real Pell recipients / 2,277 real FTE enrollment --
  # both cross-checked by hand against the raw fixtures before this test
  # was written, not derived from the function under test.
  casper <- result[result$Name == "Casper College", ]
  expect_equal(casper$Pell_Recipient_Share, 932 / 2277, tolerance = 1e-6)

  # University of Wyoming has the lowest share of the 9 (a 4-year flagship,
  # not a community college) -- confirms this isn't a flat/hardcoded value.
  uw <- result[result$Name == "University of Wyoming", ]
  expect_true(uw$Pell_Recipient_Share < casper$Pell_Recipient_Share)
})

test_that("parse_ipeds_he_pell_share gives Sheridan and Gillette the same shared Pell share", {
  grants_df <- read.csv(test_path("fixtures", "fsa_wy_grants_2021.csv"))
  enrollment_raw <- read.csv(test_path("fixtures", "ipeds_wy_fall_enrollment_2021.csv"))
  enrollment_df <- parse_ipeds_he_enrollment(enrollment_raw, 2021, WY_TEST_UNITIDS)
  result <- parse_ipeds_he_pell_share(grants_df, enrollment_df, 2021, WY_TEST_UNITIDS)

  sheridan <- result[result$Name == "Sheridan College", ]
  gillette <- result[result$Name == "Gillette College", ]
  expect_equal(sheridan$Pell_Recipient_Share, gillette$Pell_Recipient_Share)
  expect_false(is.na(sheridan$Pell_Recipient_Share))
})

test_that("parse_ipeds_he_pell_share only counts grant_type == 4 (Pell), not other federal grant programs", {
  # A synthetic record with a non-Pell grant_type (e.g. 1 = Academic
  # Competitiveness Grant) and no real grant_type == 4 row for that
  # unitid must NOT be picked up as if it were Pell data.
  grants_df <- data.frame(
    unitid = 240505L, grant_type = 1L, grant_recipients_unitid = 999
  )
  enrollment_df <- data.frame(Name = "Casper College", Enrollment = 2277, Enrollment_Year = "2021")
  result <- parse_ipeds_he_pell_share(grants_df, enrollment_df, 2021, WY_TEST_UNITIDS)
  casper <- result[result$Name == "Casper College", ]
  expect_true(is.na(casper$Pell_Recipient_Share))
})

test_that("parse_ipeds_he_pell_share returns an empty, correctly-shaped frame when given no rows", {
  result <- parse_ipeds_he_pell_share(data.frame(), data.frame(), 2021, WY_TEST_UNITIDS)
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("Name", "Pell_Recipient_Share", "Pell_Year"))

  result2 <- parse_ipeds_he_pell_share(
    data.frame(unitid = 240505L, grant_type = 4L, grant_recipients_unitid = 932),
    data.frame(), 2021
  , WY_TEST_UNITIDS)
  expect_equal(nrow(result2), 0)
})

# ---- from WY's tests/testthat/test-census-acs-scraper.R ----
# All fixtures are real captures from the live Census ACS 5-Year API
# (Albany/Campbell/Teton counties -- a deliberate mix of a low-mining
# college town, a high-mining energy county, and a high-cost resort county,
# so the tests exercise real range, not three similar rows).

read_fixture <- function(name) paste(readLines(test_path("fixtures", name), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

test_that("parse_acs_json converts the Census array-of-arrays shape into a properly-named, numeric-coerced data frame", {
  result <- parse_acs_json(read_fixture("census_acs_detail_2024.json"))

  expect_equal(nrow(result), 3)
  expect_equal(names(result), c("NAME", "B19013_001E", "B25064_001E", "B01003_001E", "state", "county"))
  expect_true(is.numeric(result$B19013_001E))
  expect_true(is.character(result$NAME))
  expect_true(is.character(result$county))  # FIPS codes stay character, not coerced

  campbell <- result[result$NAME == "Campbell County, Wyoming", ]
  expect_equal(campbell$B19013_001E, 89869)
  expect_equal(campbell$B01003_001E, 47240)
})

test_that("parse_acs_json returns an empty data frame (not an error) for a header-only response", {
  result <- parse_acs_json('[["NAME","B19013_001E","state","county"]]')
  expect_equal(nrow(result), 0)
})

test_that("clean_acs_value treats negative sentinel codes as NA, matching clean_ipeds_value()'s pattern", {
  expect_true(is.na(clean_acs_value(-666666666)))
  expect_true(is.na(clean_acs_value(NA_real_)))
  expect_equal(clean_acs_value(61917), 61917)
  expect_equal(clean_acs_value(0), 0)  # a real zero must NOT be treated as missing
})

test_that("parse_census_income_rent_population extracts real income/rent/population for all 3 fixture counties", {
  raw <- parse_acs_json(read_fixture("census_acs_detail_2024.json"))
  result <- parse_census_income_rent_population(raw, 2024)

  expect_equal(nrow(result), 3)
  expect_equal(names(result), c("County", "Median_Household_Income", "Median_Gross_Rent", "Population", "ACS_Year"))
  expect_true(all(result$ACS_Year == 2024))

  teton <- result[result$County == "Teton County, Wyoming", ]
  expect_equal(teton$Median_Household_Income, 124172)
  expect_equal(teton$Median_Gross_Rent, 1904)
  expect_equal(teton$Population, 23396)
})

test_that("parse_census_mining_employment_share computes a real, meaningfully different rate per county", {
  raw <- parse_acs_json(read_fixture("census_acs_subject_2024.json"))
  result <- parse_census_mining_employment_share(raw)

  expect_equal(nrow(result), 3)
  albany <- result$Mining_Employment_Share[result$County == "Albany County, Wyoming"]
  campbell <- result$Mining_Employment_Share[result$County == "Campbell County, Wyoming"]
  teton <- result$Mining_Employment_Share[result$County == "Teton County, Wyoming"]

  expect_equal(round(campbell, 4), round(3626 / 23829, 4))
  # Confirms this figure actually discriminates real counties, not just
  # computes something -- Campbell (coal) is a real energy county, Teton
  # (Jackson Hole, tourism-based) genuinely has almost no mining employment.
  expect_true(campbell > 10 * teton)
  expect_true(campbell > albany)
})

test_that("parse_census_mining_employment_share returns NA (not an error or Inf) when total employment is 0", {
  raw <- data.frame(NAME = "Fake County, Wyoming", S2403_C01_001E = 0, S2403_C01_004E = 0, stringsAsFactors = FALSE)
  result <- parse_census_mining_employment_share(raw)
  expect_true(is.na(result$Mining_Employment_Share))
})

test_that("compute_population_change_pct joins current vs. prior by county and computes real, signed percent change", {
  current <- parse_census_income_rent_population(parse_acs_json(read_fixture("census_acs_detail_2024.json")), 2024)
  prior <- parse_census_income_rent_population(parse_acs_json(read_fixture("census_acs_detail_2019.json")), 2019)

  result <- compute_population_change(current, prior)
  expect_equal(nrow(result), 3)

  # Real values: Teton grew (23396 vs 23280 five years earlier), Campbell
  # and Albany both shrank slightly -- confirms this isn't just always
  # positive/always the same sign.
  teton <- result$Population_Change_Pct[result$County == "Teton County, Wyoming"]
  campbell <- result$Population_Change_Pct[result$County == "Campbell County, Wyoming"]
  expect_true(teton > 0)
  expect_true(campbell < 0)
  expect_equal(round(teton, 5), round((23396 - 23280) / 23280, 5))
})

test_that("compute_population_change_pct returns an empty frame (not an error) when either side has no rows", {
  current <- parse_census_income_rent_population(parse_acs_json(read_fixture("census_acs_detail_2024.json")), 2024)
  empty <- data.frame(County = character(0), Population = numeric(0), stringsAsFactors = FALSE)

  expect_equal(nrow(compute_population_change(current, empty)), 0)
  expect_equal(nrow(compute_population_change(empty, current)), 0)
})

test_that("census_api_key errors with a clear, actionable message when CENSUS_API_KEY isn't set", {
  withr::local_envvar(CENSUS_API_KEY = "")
  expect_error(census_api_key(), "CENSUS_API_KEY")
  expect_error(census_api_key(), "key_signup")
})
