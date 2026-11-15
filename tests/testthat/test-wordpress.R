# The WordPress family routine (R/wordpress.R). Fixtures are MT district pages captured for the
# per-district scrapers this routine replaced (claude-notes/2026-11-14_wordpress-family/).

wp_fixture <- function(name) paste(readLines(test_path("fixtures", name), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

test_that("parse_wordpress_postings: Turner (posted pay off the title, into the pay columns)", {
  r <- parse_wordpress_postings(wp_fixture("wordpress_turner_employment.html"), "https://example.org/jobs", "Turner")
  expect_equal(r$Title, c("Vo-Ag Teacher – Part-time", "Business Teacher – Part-time", "Secretary"))
  expect_equal(r$Pay_Min[r$Title == "Secretary"], 14)
  expect_equal(r$Pay_Unit[r$Title == "Secretary"], "hour")
  expect_true(all(r$Location == "Turner"))
})

test_that("parse_wordpress_postings: Augusta", {
  r <- parse_wordpress_postings(wp_fixture("wordpress_augusta_job_openings.html"), "u", "Augusta")
  expect_equal(r$Title, c("Special Education Full-Time Aide", "Become a Bus Driver", "Substitute Teachers"))
})

test_that("wordpress_page_text drops menus and sidebars; bad input gives NA", {
  html <- '<html><body><nav>Home Teachers</nav><div class="entry-content"><p>Openings:</p><p>Head Cook</p></div><aside class="widget">Coach Corner</aside></body></html>'
  expect_equal(trimws(wordpress_page_text(html)), "Openings:\n\nHead Cook")
  expect_equal(parse_wordpress_postings(html, "u", "x")$Title, "Head Cook")
  expect_true(is.na(wordpress_page_text(NA)))
})
