# The Joomla family routine (R/joomla.R). Fixtures: MT district pages captured for the per-district scrapers this
# routine replaced (claude-notes/2026-11-14_small-families/joomla/).

read_fixture <- function(f) paste(readLines(test_path("fixtures", f), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

test_that("parse_joomla_postings: Roy's prose postings and its plea heading", {
  r <- parse_joomla_postings(read_fixture("joomla_roy_employment.html"), "https://roy.k12.mt.us/", "Roy")
  # "BUS DRIVER NEEDED": the heading names the job; the lunch-program heading is not a job
  expect_equal(r$Title, c("Head Cook", "Full-time Paraprofessional", "BUS DRIVER"))
  expect_true(all(r$Location == "Roy"))
})

test_that("parse_joomla_postings: Joliet's table, each cell on its own line", {
  r <- parse_joomla_postings(read_fixture("joomla_joliet_jobs.html"), "https://www.jolietschools.org/", "Joliet")
  expect_equal(r$Title, c("Bus Route & Bus Route Sub Drivers", "Substitute Teachers", "Head High School Boys Basketball Coach",
                          "Asst High School Girls Basketball Coach", "Head High School Boys/Girls Golf Coach"))
  expect_false("Online" %in% r$Title)
})

test_that("a body with no HTML is an error, not zero postings", {
  # Roberts's real 200 body, 2026-11-04
  expect_error(parse_joomla_postings(read_fixture("joomla_roberts_maintenance.txt"), "u", "Roberts"),
               "no HTML: \"site under maintenance\"")
  expect_equal(nrow(parse_joomla_postings(NA, "u", "x")), 0)
})

test_that("district_page_text keeps a title's last letter before a cell end", {
  # Canyon Creek (Edlio): "Administrative Assistant </a></strong><br>" read as "Administrative Assistan"
  html <- '<html><body><table><tr><td><strong><a href="/j?id=1">Administrative Assistant </a></strong><br></td></tr></table></body></html>'
  expect_match(district_page_text(html), "Administrative Assistant\n")
  expect_equal(parse_edlio_postings(html, "https://x.org/", "x")$Title, "Administrative Assistant")
})
