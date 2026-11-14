# The Finalsite family routine (R/finalsite.R). Fixtures are MT district pages captured for the
# per-district scrapers this routine replaced (claude-notes/2026-11-14_finalsite-family/).

fs_fixture <- function(name) paste(readLines(test_path("fixtures", name), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

test_that("parse_finalsite_postings: Manhattan's resource documents give titles and per-posting links", {
  r <- parse_finalsite_postings(fs_fixture("finalsite_manhattan_2026-11-02.html"), "https://www.rollontigers.org/page/current-openings", "Manhattan")
  expect_true(all(c("Long-Term Substitute Teacher: Elementary", "Swing Shift Custodian", "HS Assistant Wrestling Coach") %in% r$Title))
  expect_equal(r$Link[r$Title == "Swing Shift Custodian"],
               "https://www.rollontigers.org/fs/resource-manager/view/602fdc35-1fe4-4718-9b10-1eedeb7ecc15")
  expect_true(all(r$Location == "Manhattan"))
  expect_equal(nrow(r), 12)
})

test_that("parse_finalsite_postings: Thompson Falls (link decorations off, the Job Descriptions library skipped)", {
  r <- parse_finalsite_postings(fs_fixture("finalsite_thompsonfalls_employment.html"), "https://www.thompsonfalls.net/employment", "Thompson Falls")
  expect_equal(r$Title, c("Para Professional", "Assistant High School Football Coach", "Food Service Worker",
                          "Food Service Worker - Dishwasher", "Assistant High School Soccer Coach"))
  expect_false(any(c("Clerk", "Drama Advisor", "Load More") %in% r$Title))
})

test_that("finalsite_page_text keeps element titles and reads resource titles; bad input gives NA", {
  html <- '<html><body><div id="fsHeader">Menu Teachers</div><div class="fsPageContent"><section><header><h2 class="fsElementTitle">Substitute Teachers</h2></header><a data-resource-title="Bus Driver" href="/x"><img alt=""></a></section></div></body></html>'
  t <- finalsite_page_text(html)
  expect_match(t, "Substitute Teachers", fixed = TRUE)
  expect_match(t, "Bus Driver", fixed = TRUE)
  expect_false(grepl("Menu", t, fixed = TRUE))
  expect_true(is.na(finalsite_page_text(NA)))
  r <- parse_finalsite_postings(html, "https://d.org/jobs", "Town")
  expect_equal(r$Link[r$Title == "Bus Driver"], "https://d.org/x")
})
