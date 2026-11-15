# The Google Sites family routine (R/googlesites.R). Fixtures are MT district pages captured for the
# per-district scrapers this routine replaced (claude-notes/2026-11-14_googlesites-family/).

gs_fixture <- function(name) paste(readLines(test_path("fixtures", name), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

test_that("googlesites_page_text drops the site menu, header, footer and Google's footer", {
  t <- googlesites_page_text(gs_fixture("googlesites_cutbank_employment.html"))
  expect_false(grepl("In-district Transfer Application\nCut Bank School District #15\nEmployment", t, fixed = TRUE))
  expect_false(grepl("Report abuse", t, fixed = TRUE))
  expect_match(t, "Open Positions (as of August 13, 2026)", fixed = TRUE)
  expect_true(is.na(googlesites_page_text(NA)))
  expect_true(is.na(googlesites_page_text("")))
})

test_that("parse_googlesites_postings: Cut Bank (start/stop rule)", {
  r <- parse_googlesites_postings(gs_fixture("googlesites_cutbank_employment.html"), "https://example.org/jobs", "Cut Bank",
                                  "^Open Positions", "^Virtual Tours")
  expect_equal(r$Title, c("Health & Wellness Advisor", "Middle School Boys Basketball Coach", "Route Bus Driver",
                          "School Lunch Worker", "SUBSTITUTE Teachers/Paraprofessionals/Bus Drivers"))
  expect_true(all(r$Location == "Cut Bank"))
  expect_true(all(r$Link == "https://example.org/jobs"))
  expect_true(all(is.na(r$Posted_Date)))
  expect_true(all(c("Pay_Min", "Pay_Max", "Pay_Unit", "Pay_Text") %in% names(r)))
})

test_that("parse_googlesites_postings: Big Timber (titles inside sentences)", {
  r <- parse_googlesites_postings(gs_fixture("googlesites_bigtimbergs_2026-10-21.html"), "u", "Big Timber")
  expect_equal(r$Title, c("SPED Teacher", "Substitute Teachers", "SPED Case Manager"))
})

test_that("an unreadable page gives no rows, not an error", {
  expect_equal(nrow(parse_googlesites_postings(NA, "u", "x")), 0)
  expect_equal(nrow(parse_googlesites_postings("<html><body><p>Nothing here</p></body></html>", "u", "x")), 0)
})
