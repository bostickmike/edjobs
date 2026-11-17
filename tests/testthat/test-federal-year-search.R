# The year searches may only fall back past a year that isn't published yet (a 404, or no rows). A server error on
# the newest year must propagate, so the pipeline keeps last run's file instead of shipping an older year as the
# newest (2026-11-17 review).
http_error <- function(status) {
  resp <- httr2::response(status)
  tryCatch(httr2::resp_check_status(resp), error = function(e) e)
}

test_that("try_federal_year reads a 404 as not published and lets anything else through", {
  expect_equal(try_federal_year(stop(http_error(404))), data.frame())
  expect_null(try_federal_year(stop(http_error(404)), NULL))
  expect_error(try_federal_year(stop(http_error(504))), class = "httr2_http_504")
  expect_error(try_federal_year(stop("timeout")), "timeout")
  expect_equal(try_federal_year(data.frame(a = 1)), data.frame(a = 1))
})

test_that("a 504 on the newest year stops the IPEDS staff search instead of returning an older year", {
  local_mocked_bindings(fetch_ipeds_staff_year = function(year, fips) {
    if (year == 2025) stop(http_error(504))
    if (year > 2025) return(data.frame())
    data.frame(unitid = 1, year = year)
  })
  expect_error(fetch_ipeds_staff_salaries("56", c(`1` = "A"), start_year = 2026), class = "httr2_http_504")
})

test_that("a not-yet-published year still moves the search back", {
  local_mocked_bindings(fetch_urban_college_year = function(endpoint, year, fips) {
    if (year == 2026) stop(http_error(404))
    if (year == 2025) return(data.frame())
    data.frame(unitid = 1, year = year)
  })
  r <- find_latest_urban_college_year(IPEDS_SALARY_ENDPOINT, "56", start_year = 2026, quiet_errors = TRUE)
  expect_equal(r$year, 2024)
  local_mocked_bindings(fetch_urban_college_year = function(endpoint, year, fips) {
    if (year == 2025) stop(http_error(503))
    data.frame()
  })
  expect_error(find_latest_urban_college_year(IPEDS_SALARY_ENDPOINT, "56", start_year = 2026, quiet_errors = TRUE),
               class = "httr2_http_503")
})

test_that("an older year never replaces a newer reference file", {
  rules <- REFERENCE_TABLE_RULES$qcew_county_school_pay
  old <- data.frame(County = c("A", "B"), Avg_Annual_Pay = c(50000, 52000), Year = 2024)
  older <- transform(old, Year = 2023)
  newer <- transform(old, Year = 2025)
  expect_match(reference_table_problem(older, rules, old), "older than the current file's 2024")
  expect_null(reference_table_problem(newer, rules, old))
  expect_null(reference_table_problem(old, rules, old))
})
