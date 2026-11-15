# The Edlio family routine (R/edlio.R) and the shared district page text. Fixture: an MT district page
# captured for the per-district scraper this routine replaced (claude-notes/2026-11-14_small-families/edlio/).

test_that("parse_edlio_postings: Livingston's posting documents give titles and per-posting links", {
  html <- paste(readLines(test_path("fixtures", "edlio_livingston_employment.html"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  url <- "https://www.livingstonschools.org/apps/jobs/"
  r <- parse_edlio_postings(html, url, "Livingston")
  expect_equal(nrow(r), 20)
  expect_true(all(r$Link != url))
  expect_true(all(grepl("^https://", r$Link)))
  expect_true(all(r$Location == "Livingston"))
  # All-caps school and section names are headers, tax forms are not titles
  expect_false(any(c("EAST SIDE ELEMENTARY", "MT W-4 2026", "I-9 2026") %in% r$Title))
  # A document's file name reads as words
  expect_true("Dish Server Cashiers" %in% r$Title)
})

test_that("district_page_text drops page chrome; bad input gives NA", {
  html <- '<html><body><header>District Menu Teachers</header><div><p>Openings:</p><p>Head Cook</p></div><footer>Coach Corner</footer></body></html>'
  expect_equal(parse_edlio_postings(html, "u", "x")$Title, "Head Cook")
  expect_true(is.na(district_page_text(NA)))
})
