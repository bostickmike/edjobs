# Pre-flight canary and rebuild-parity report (run_checks_canary.R; package review step 6, slice 3). The validator
# tests are the states' test-canary-api-check.R, which these functions replace.

json_resp <- function(txt) httr2::response(200, headers = list("Content-Type" = "application/json"), body = charToRaw(txt))
html_resp <- function(txt) httr2::response(200, headers = list("Content-Type" = "text/html"), body = charToRaw(txt))

test_that("validate_urban_ipeds validates presence of results, unitid, and year", {
  expect_true(validate_urban_ipeds(json_resp('{"results": [{"unitid": 180160, "year": 2024}]}'))$ok)
  expect_false(validate_urban_ipeds(json_resp('{"results": []}'))$ok)
  expect_false(validate_urban_ipeds(json_resp('{"results": [{"some_other_field": 123}]}'))$ok)
})

test_that("validate_urban_ccd validates presence of results, leaid, and year", {
  expect_true(validate_urban_ccd(json_resp('{"results": [{"leaid": 3000001, "year": 2023}]}'))$ok)
  expect_false(validate_urban_ccd(json_resp('{"results": [{"unitid": 180160, "year": 2023}]}'))$ok)
})

test_that("the feed page must show one of its signature words", {
  expect_true(validate_feed_page(html_resp("<html><body>Montana TeacherSearch</body></html>"), c("TeacherSearch", "Montana"), "OPI")$ok)
  expect_true(validate_feed_page(html_resp("<html><body>Wyoming WSBA Jobs</body></html>"), c("wsba", "wyoming"), "WSBA")$ok)
  bad <- validate_feed_page(html_resp("<html><body>502 Bad Gateway</body></html>"), c("wsba", "wyoming"), "WSBA")
  expect_false(bad$ok)
  expect_match(bad$error, "WSBA page's signature")
})

test_that("check_endpoint handles network timeouts and 5xx errors gracefully", {
  httr2::local_mocked_responses(list(
    httr2::response(503, headers = list("Content-Type" = "text/plain"), body = charToRaw("Service Unavailable"))
  ))
  res <- check_endpoint("Mock Service", "https://api.example.com", validate_urban_ipeds)
  expect_false(res$ok)
  expect_match(res$msg, "503")
})

test_that("run_canaries checks the state's FIPS and its feed, named from the config", {
  withr::local_envvar(CENSUS_API_KEY = "")
  seen <- character(0)
  httr2::local_mocked_responses(function(req) {
    seen <<- c(seen, req$url)
    if (grepl("ipeds", req$url)) json_resp('{"results": [{"unitid": 1, "year": 2024}]}')
    else if (grepl("ccd", req$url)) json_resp('{"results": [{"leaid": 1, "year": 2023}]}')
    else html_resp("<html>Teststate Feed board</html>")
  })
  feed <- list(short_name = "TF", label = "TF statewide", url = "https://feed.example.org/jobs", discrepancy_file = "x.csv")
  res <- run_canaries(state_config("Teststate", "99", "app", feed = feed))
  expect_equal(vapply(res, `[[`, "", "name"), c("Urban Institute IPEDS Salary", "Urban Institute CCD Directory", "TF Statewide Feed"))
  expect_true(all(vapply(res, `[[`, TRUE, "ok")))     # default signature: short name or state name
  expect_true(all(grepl("fips=99", seen[1:2])))
  expect_equal(seen[3], "https://feed.example.org/jobs")
  out <- capture.output(canary_step(strict = FALSE, config = state_config("Teststate", "99", "app", feed = feed)))
  expect_equal(out[length(out)], "All pre-flight API canary checks passed.")
})

test_that("strict mode stops on a failed check", {
  withr::local_envvar(CENSUS_API_KEY = "")
  httr2::local_mocked_responses(function(req) httr2::response(503))
  cfg <- state_config("Teststate", "99", "app")
  expect_error(capture.output(canary_step(strict = TRUE, config = cfg)), "^API Canary checks failed in strict mode\\.$")
  out <- capture.output(canary_step(strict = FALSE, config = cfg))
  expect_equal(out[length(out)], "Warning: One or more canary checks failed (non-strict mode; continuing).")
})
