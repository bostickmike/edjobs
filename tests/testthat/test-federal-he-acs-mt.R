# Moved from the state repos (package phase 5, 2026-11-15) with their real
# Urban Institute / Census fixtures; the unitid maps are the states' own, copied
# into helper-unitid-maps.R as test data.

# ---- from MT's tests/testthat/test-ipeds-salary-scraper.R ----
# Real fixture: the actual Montana (fips=30) response from the Urban
# Institute's Education Data Portal
# college-university/ipeds/salaries-instructional-staff/2024 endpoint --
# the original 6 unitids captured 2026-08-06, the 7 added when
# MT_IPEDS_UNITID_MAP was extended to cover the rest of the registry
# captured 2026-08-07, the 4 tribal colleges (Fort Peck CC, Little Big Horn
# College, Salish Kootenai College, Stone Child College) appended the same
# day when their own fast-follow gap closed, and Aaniiih Nakoda College /
# Chief Dull Knife College appended once their own new heuristic scrapers
# (misc_college_scrapers.R) went live -- all trimmed to just the unitids
# MT_IPEDS_UNITID_MAP actually needs (all academic_rank x contract_length
# x sex combinations for each, real data not synthetic).

test_that("parse_ipeds_he_salaries extracts real overall + Professor-rank salaries for all 22 institutions", {
  df <- read.csv(test_path("fixtures", "ipeds_mt_salaries_2024.csv"))
  result <- parse_ipeds_he_salaries(df, 2024, MT_TEST_UNITIDS)

  expect_equal(nrow(result), 22)

  msu <- result[result$Name == "Montana State University", ]
  expect_equal(round(msu$Faculty_Avg_Salary), 99427)
  expect_equal(msu$Faculty_Count, 626)
  expect_equal(round(msu$Faculty_Avg_Salary_Professor), 131791)
  expect_equal(msu$Salary_Year, "2024")

  mtech <- result[result$Name == "Montana Tech", ]
  expect_equal(round(mtech$Faculty_Avg_Salary), 78730)
  expect_equal(mtech$Faculty_Count, 125)

  um <- result[result$Name == "University of Montana", ]
  expect_equal(round(um$Faculty_Avg_Salary), 93938)
  expect_equal(um$Faculty_Count, 442)

  # Blackfeet CC, Dawson CC, and Miles CC (all small 2-year colleges) have
  # genuinely no academic_rank=1 "Professor" record in the real 2024 data
  # -- confirmed live, not a fixture-trimming artifact -- so
  # Faculty_Avg_Salary_Professor must be real NA for all three, not 0 or
  # a silently-wrong fallback.
  blackfeet <- result[result$Name == "Blackfeet Community College", ]
  expect_equal(round(blackfeet$Faculty_Avg_Salary), 25621)
  expect_equal(blackfeet$Faculty_Count, 53)
  expect_true(is.na(blackfeet$Faculty_Avg_Salary_Professor))

  # The 4 tribal colleges also have no academic_rank=1 "Professor" record
  # in the real 2024 data -- same real absence as Blackfeet/Dawson/Miles
  # above, not a fixture-trimming artifact.
  skc <- result[result$Name == "Salish Kootenai College", ]
  expect_equal(round(skc$Faculty_Avg_Salary), 54230)
  expect_equal(skc$Faculty_Count, 69)
  expect_true(is.na(skc$Faculty_Avg_Salary_Professor))

  stone_child <- result[result$Name == "Stone Child College", ]
  expect_equal(round(stone_child$Faculty_Avg_Salary), 58915)
  expect_equal(stone_child$Faculty_Count, 12)
  expect_true(is.na(stone_child$Faculty_Avg_Salary_Professor))

  # Highlands College has a real, permanent gap unlike every institution
  # above: confirmed live across 2020-2024, IPEDS's salaries-instructional-
  # staff endpoint has zero rows for its unitid in any year -- not a
  # missing rank tier, no salary data reported under its own unitid at
  # all (see MT_IPEDS_UNITID_MAP's own comment in ipeds_salary_scraper.R).
  highlands <- result[result$Name == "Highlands College", ]
  expect_equal(nrow(highlands), 1)
  expect_true(is.na(highlands$Faculty_Avg_Salary))
  expect_true(is.na(highlands$Faculty_Avg_Salary_Professor))

  # Helena College and UM Western both report real salary data under
  # their own unitids -- unlike Highlands/Missoula College, no gap here.
  helena <- result[result$Name == "Helena College", ]
  expect_equal(round(helena$Faculty_Avg_Salary), 58911)
  expect_equal(helena$Faculty_Count, 33)

  um_western <- result[result$Name == "University of Montana Western", ]
  expect_equal(round(um_western$Faculty_Avg_Salary), 70907)
  expect_equal(round(um_western$Faculty_Avg_Salary_Professor), 80230)
})

test_that("parse_ipeds_he_salaries returns real NA for a unitid missing from that year's response", {
  df <- read.csv(test_path("fixtures", "ipeds_mt_salaries_2024.csv"))
  df_no_fvcc <- df[df$unitid != 180197, ]

  result <- parse_ipeds_he_salaries(df_no_fvcc, 2024, MT_TEST_UNITIDS)

  fvcc <- result[result$Name == "Flathead Valley Community College", ]
  expect_equal(nrow(fvcc), 1)
  expect_true(is.na(fvcc$Faculty_Avg_Salary))
})

test_that("parse_ipeds_he_salaries returns an empty, correctly-shaped frame when given no rows", {
  result <- parse_ipeds_he_salaries(data.frame(), 2024, MT_TEST_UNITIDS)
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("Name", "Faculty_Avg_Salary", "Faculty_Avg_Salary_Professor",
                                 "Faculty_Count", "Salary_Year"))
})

test_that("clean_ipeds_value treats negative sentinel codes as real NA, not real values", {
  expect_true(is.na(clean_ipeds_value(-1)))
  expect_true(is.na(clean_ipeds_value(-2)))
  expect_true(is.na(clean_ipeds_value(-3)))
  expect_equal(clean_ipeds_value(50000), 50000)
})

test_that("parse_ipeds_salary_trend_year extracts just the headline overall figure per institution", {
  df <- read.csv(test_path("fixtures", "ipeds_mt_salaries_2024.csv"))
  result <- parse_ipeds_salary_trend_year(df, 2024, MT_TEST_UNITIDS)

  expect_equal(nrow(result), 22)
  expect_equal(names(result), c("Name", "Year", "Faculty_Avg_Salary"))
  msu <- result[result$Name == "Montana State University", ]
  expect_equal(round(msu$Faculty_Avg_Salary), 99427)
})

test_that("fetch_json_paginated handles JSON null fields across paginated pages without error", {
  page1 <- "{\"count\": 2, \"next\": \"https://api.example.com/page2\", \"results\": [{\"unitid\": 180160, \"year\": 2022, \"average_salary\": 65000, \"number_applied\": null}]}"
  page2 <- "{\"count\": 2, \"next\": null, \"results\": [{\"unitid\": 180161, \"year\": 2022, \"average_salary\": 72000, \"number_applied\": 150}]}"

  httr2::local_mocked_responses(list(
    httr2::response(200, headers = list("Content-Type" = "application/json"), body = charToRaw(page1)),
    httr2::response(200, headers = list("Content-Type" = "application/json"), body = charToRaw(page2))
  ))

  result <- fetch_json_paginated("https://api.example.com/page1")
  expect_equal(nrow(result), 2)
  expect_equal(result$unitid, c(180160, 180161))
  expect_true(is.na(result$number_applied[1]))
  expect_equal(result$number_applied[2], 150)
})

# ---- from MT's tests/testthat/test-ipeds-enrollment-scraper.R ----
# Real fixtures: the actual Montana (fips=30) responses from the Urban
# Institute's Education Data Portal college-university/ipeds/fall-enrollment
# endpoint -- the original 6 unitids captured 2026-08-06 for 2023 (latest
# available) and 2018 (5 years prior, for the trend test), the 7 added
# when MT_IPEDS_UNITID_MAP was extended captured 2026-08-07, the 4
# tribal colleges (Fort Peck CC, Little Big Horn College, Salish Kootenai
# College, Stone Child College) appended the same day when their own
# fast-follow gap closed, and Aaniiih Nakoda College / Chief Dull Knife
# College appended once their own new heuristic scrapers
# (misc_college_scrapers.R) went live.

test_that("parse_ipeds_he_enrollment sums FTE across every level_of_study for all 22 institutions", {
  df <- read.csv(test_path("fixtures", "ipeds_mt_fall_enrollment_2023.csv"))
  result <- parse_ipeds_he_enrollment(df, 2023, MT_TEST_UNITIDS)

  expect_equal(nrow(result), 22)
  msu <- result[result$Name == "Montana State University", ]
  expect_equal(msu$Enrollment, 15586)
  expect_equal(msu$Enrollment_Year, "2023")

  fvcc <- result[result$Name == "Flathead Valley Community College", ]
  expect_equal(fvcc$Enrollment, 1208)

  um <- result[result$Name == "University of Montana", ]
  expect_equal(um$Enrollment, 16888)

  skc <- result[result$Name == "Salish Kootenai College", ]
  expect_equal(skc$Enrollment, 1078)

  # Highlands College reports fall enrollment separately from Montana
  # Tech even though it has no separate salary/Pell data (see
  # MT_IPEDS_UNITID_MAP's comment) -- a real, structured number, not NA.
  highlands <- result[result$Name == "Highlands College", ]
  expect_equal(highlands$Enrollment, 864)

  # Helena College and UM Western also report enrollment separately from
  # University of Montana -- no gap, unlike Missoula College (which has
  # no row here at all, not even NA -- it's simply absent from
  # MT_IPEDS_UNITID_MAP, see that map's own comment).
  helena <- result[result$Name == "Helena College", ]
  expect_equal(helena$Enrollment, 1432)
  expect_false("Missoula College" %in% result$Name)
})

test_that("parse_ipeds_he_enrollment returns an empty, correctly-shaped frame when given no rows", {
  result <- parse_ipeds_he_enrollment(data.frame(), 2023, MT_TEST_UNITIDS)
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("Name", "Enrollment", "Enrollment_Year"))
})

test_that("compute_enrollment_change computes a real 5-year trend from real 2018 vs 2023 data", {
  current_raw <- read.csv(test_path("fixtures", "ipeds_mt_fall_enrollment_2023.csv"))
  prior_raw <- read.csv(test_path("fixtures", "ipeds_mt_fall_enrollment_2018.csv"))
  current <- parse_ipeds_he_enrollment(current_raw, 2023, MT_TEST_UNITIDS)
  prior <- parse_ipeds_he_enrollment(prior_raw, 2018, MT_TEST_UNITIDS)

  result <- compute_enrollment_change(current, prior)

  msu <- result[result$Name == "Montana State University", ]
  # Real 2018->2023 change: (15586 - 15327) / 15327
  expect_equal(msu$Enrollment_Change_Pct, (15586 - 15327) / 15327, tolerance = 1e-6)

  # Real data confirms a mix, not a uniform trend -- Montana Tech shrank
  # over this window while MSU grew.
  mtech <- result[result$Name == "Montana Tech", ]
  expect_true(mtech$Enrollment_Change_Pct < 0)

  # Same real mix among the tribal colleges: Fort Peck CC grew while
  # Salish Kootenai College shrank over this window.
  fpcc <- result[result$Name == "Fort Peck Community College", ]
  expect_true(fpcc$Enrollment_Change_Pct > 0)
  skc <- result[result$Name == "Salish Kootenai College", ]
  expect_true(skc$Enrollment_Change_Pct < 0)
})

test_that("compute_enrollment_change returns an empty frame when either side has no rows", {
  result <- compute_enrollment_change(data.frame(), data.frame(Name = "X", Enrollment = 1))
  expect_equal(nrow(result), 0)
})

# ---- from MT's tests/testthat/test-fsa-pell-scraper.R ----
# Real fixtures: the actual Montana (fips=30) responses from the Urban
# Institute's Education Data Portal college-university/fsa/grants/2021 and
# college-university/ipeds/fall-enrollment/2021 endpoints -- the original 6
# unitids captured 2026-08-06, the 7 added when MT_IPEDS_UNITID_MAP was
# extended captured 2026-08-07, the 4 tribal colleges appended the
# same day when their own fast-follow gap closed, and Aaniiih Nakoda
# College / Chief Dull Knife College appended once their own new
# heuristic scrapers (misc_college_scrapers.R) went live. Both from the SAME year
# (2021) on purpose -- Pell share must be computed against enrollment from
# the same year as the grants data (see this file's source header), not
# the separately-fixtured 2023 "latest" enrollment used elsewhere.

test_that("parse_ipeds_he_pell_share computes a real recipients-over-enrollment ratio for all 22 institutions", {
  grants <- read.csv(test_path("fixtures", "fsa_mt_grants_2021.csv"))
  enrollment_raw <- read.csv(test_path("fixtures", "ipeds_mt_fall_enrollment_2021.csv"))
  enrollment <- parse_ipeds_he_enrollment(enrollment_raw, 2021, MT_TEST_UNITIDS)

  result <- parse_ipeds_he_pell_share(grants, enrollment, 2021, MT_TEST_UNITIDS)

  expect_equal(nrow(result), 22)
  msu <- result[result$Name == "Montana State University", ]
  expect_equal(msu$Pell_Recipient_Share, 2852 / 15083, tolerance = 1e-6)
  expect_equal(msu$Pell_Year, "2021")

  um <- result[result$Name == "University of Montana", ]
  expect_true(um$Pell_Recipient_Share > 0 && um$Pell_Recipient_Share < 1)

  skc <- result[result$Name == "Salish Kootenai College", ]
  expect_true(skc$Pell_Recipient_Share > 0 && skc$Pell_Recipient_Share < 1)
})

test_that("parse_ipeds_he_pell_share returns real NA when enrollment for that institution is missing or zero", {
  grants <- read.csv(test_path("fixtures", "fsa_mt_grants_2021.csv"))
  enrollment <- data.frame(Name = "Montana State University", Enrollment = 0, stringsAsFactors = FALSE)

  result <- parse_ipeds_he_pell_share(grants, enrollment, 2021, MT_TEST_UNITIDS)

  msu <- result[result$Name == "Montana State University", ]
  expect_true(is.na(msu$Pell_Recipient_Share))
})

test_that("parse_ipeds_he_pell_share returns an empty, correctly-shaped frame when given no rows", {
  result <- parse_ipeds_he_pell_share(data.frame(), data.frame(), 2021, MT_TEST_UNITIDS)
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("Name", "Pell_Recipient_Share", "Pell_Year"))
})

test_that("clean_fsa_value treats negative sentinel codes as real NA", {
  expect_true(is.na(clean_fsa_value(-1)))
  expect_equal(clean_fsa_value(500), 500)
})

# ---- from MT's tests/testthat/test-census-acs-scraper.R ----
# Real fixtures: actual Montana (fips=30) responses from the Census Data
# API's ACS 5-Year Estimates endpoints, captured 2026-08-06 (2022 detail +
# subject tables, plus 2017 detail for the 5-year population trend). All 56
# counties, real data.

test_that("parse_census_income_rent_population strips the state suffix so County matches the registry format", {
  json <- paste(readLines(test_path("fixtures", "acs_mt_detail_2022.json"), warn = FALSE), collapse = "\n")
  raw <- parse_acs_json(json)
  result <- parse_census_income_rent_population(raw, 2022, county_suffix = ", Montana")

  expect_equal(nrow(result), 56)
  yellowstone <- result[result$County == "Yellowstone County", ]
  expect_equal(nrow(yellowstone), 1)
  expect_equal(yellowstone$Median_Household_Income, 72300)
  expect_equal(yellowstone$Median_Gross_Rent, 1055)
  expect_equal(yellowstone$Population, 165524)
  expect_equal(yellowstone$ACS_Year, 2022)
})

test_that("parse_census_mining_employment_share computes a real share and shows real county-to-county spread", {
  json <- paste(readLines(test_path("fixtures", "acs_mt_subject_2022.json"), warn = FALSE), collapse = "\n")
  raw <- parse_acs_json(json)
  result <- parse_census_mining_employment_share(raw, county_suffix = ", Montana")

  yellowstone <- result[result$County == "Yellowstone County", ]
  expect_equal(round(yellowstone$Mining_Employment_Share, 6), round(1192 / 84122, 6))

  # Real data confirms meaningful county-to-county variation, not noise --
  # same spirit as Wyoming's Sublette/Campbell vs. Teton spread.
  expect_true(max(result$Mining_Employment_Share, na.rm = TRUE) > 0.02)
})

test_that("compute_population_change computes a real 5-year trend from real 2017 vs 2022 data", {
  json_2022 <- paste(readLines(test_path("fixtures", "acs_mt_detail_2022.json"), warn = FALSE), collapse = "\n")
  json_2017 <- paste(readLines(test_path("fixtures", "acs_mt_detail_2017.json"), warn = FALSE), collapse = "\n")
  current <- parse_census_income_rent_population(parse_acs_json(json_2022), 2022, county_suffix = ", Montana")
  prior <- parse_census_income_rent_population(parse_acs_json(json_2017), 2017, county_suffix = ", Montana")

  result <- compute_population_change(current, prior)

  yellowstone <- result[result$County == "Yellowstone County", ]
  expect_equal(nrow(yellowstone), 1)
  expect_true(!is.na(yellowstone$Population_Change_Pct))
})

test_that("parse_acs_json returns an empty data frame for a header-only or empty response", {
  expect_equal(nrow(parse_acs_json('[["NAME","B19013_001E"]]')), 0)
})

test_that("clean_acs_value treats negative sentinel codes as real NA", {
  expect_true(is.na(clean_acs_value(-666666666)))
  expect_equal(clean_acs_value(50000), 50000)
})

test_that("county_label removes the trailing state name but leaves other text alone", {
  expect_equal(county_label("Yellowstone County, Montana", ", Montana"), "Yellowstone County")
  expect_equal(county_label("Not A State Suffix", ", Montana"), "Not A State Suffix")
})

# ---- ACS margins of error (Mike's decision 2026-11-16, option B) ----------------

acs_fixture <- function(f) parse_acs_json(paste(readLines(test_path("fixtures", f), warn = FALSE), collapse = "\n"))

test_that("income and rent carry Census's own 90% MOEs (Meagher income, Treasure rent; 2024)", {
  d <- parse_census_income_rent_population(acs_fixture("acs_mt_detail_moe_2024.json"), 2024, county_suffix = ", Montana")
  expect_equal(nrow(d), 56)
  expect_equal(unlist(d[d$County == "Meagher County", c("Median_Household_Income", "Median_Household_Income_MOE")], use.names = FALSE),
               c(71932, 21850))
  expect_equal(unlist(d[d$County == "Treasure County", c("Median_Gross_Rent", "Median_Gross_Rent_MOE")], use.names = FALSE),
               c(850, 509))
  expect_false(anyNA(d$Median_Household_Income_MOE))
})

test_that("an estimates-only response gives NA MOEs, not an error", {
  d <- parse_census_income_rent_population(acs_fixture("acs_mt_detail_2022.json"), 2022, county_suffix = ", Montana")
  expect_true(all(is.na(d$Median_Household_Income_MOE)) && all(is.na(d$Median_Gross_Rent_MOE)))
  m <- parse_census_mining_employment_share(acs_fixture("acs_mt_subject_2022.json"), county_suffix = ", Montana")
  expect_true(all(is.na(m$Mining_Employment_Share_MOE)))
})

test_that("the mining share's MOE follows the handbook's proportion formula (Big Horn, 2024)", {
  m <- parse_census_mining_employment_share(acs_fixture("acs_mt_subject_moe_2024.json"), county_suffix = ", Montana")
  b <- m[m$County == "Big Horn County", ]
  p <- 178 / 4700
  expect_equal(b$Mining_Employment_Share, p)
  expect_equal(b$Mining_Employment_Share_MOE, sqrt(101^2 - p^2 * 325^2) / 4700)
  # Treasure: 5 +/- 6 of 392 -- the MOE is at least the estimate, Census's "not significantly different from zero"
  t <- m[m$County == "Treasure County", ]
  expect_gte(t$Mining_Employment_Share_MOE, t$Mining_Employment_Share)
})

test_that("acs_proportion_moe switches to plus when the value under the root is negative, and handles bad input", {
  # num 50 +/- 5 of 100 +/- 20: 25 - 0.25 * 400 < 0, so sqrt(25 + 100) / 100
  expect_equal(acs_proportion_moe(50, 5, 100, 20), sqrt(25 + 0.25 * 400) / 100)
  expect_true(all(is.na(acs_proportion_moe(c(1, NA, 1), c(1, 1, 1), c(0, 10, 10), c(1, 1, NA)))))
})
