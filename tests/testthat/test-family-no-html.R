# A 200 response with no HTML ("Site under maintenance") is a failed fetch: every family reader must error so
# safe_scrape() logs "error", not "empty" with zero postings (2026-11-17 review; Edlio had the guard, the other
# four didn't).
test_that("every family reader errors on a body with no HTML", {
  body <- "Site under maintenance"
  u <- "https://district.example/jobs"
  expect_error(parse_edlio_postings(body, u, "Town"), "no HTML")
  expect_error(parse_googlesites_postings(body, u, "Town"), "no HTML")
  expect_error(parse_finalsite_postings(body, u, "Town"), "no HTML")
  expect_error(parse_wordpress_postings(body, u, "Town"), "no HTML")
  expect_error(parse_cyberschool_postings(body, u, "Town"), "no HTML")
  expect_error(parse_cyberschool_postings(body, u, "Town", mode = "text"), "no HTML")
})

test_that("an empty body is still no postings, not an error", {
  u <- "https://district.example/jobs"
  expect_equal(nrow(parse_googlesites_postings("", u, "Town")), 0)
  expect_equal(nrow(parse_wordpress_postings("", u, "Town")), 0)
  expect_equal(nrow(parse_finalsite_postings("", u, "Town")), 0)
  expect_equal(nrow(parse_cyberschool_postings("", u, "Town")), 0)
})
