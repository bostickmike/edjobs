# CSS boards (R/css_board.R). Real pages captured 2026-08-07: a college's Elementor loop (8 items, the first a
# template with no title), a Webflow list (1 item), a Drupal body table of PDF links (relative hrefs).

read_fixture <- function(name) paste(readLines(test_path("fixtures", name), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
rule_of <- function(...) css_board_rule(css_board_rules(data.frame(Source = "Test board", ..., stringsAsFactors = FALSE)),
                                        "Test board")

test_that("items: title, its own link and an in-item location; a template item with no title is skipped", {
  rule <- rule_of(Item_CSS = "div.e-loop-item", Title_CSS = ".elementor-heading-title a", Link_CSS = "@title",
                  Location_CSS = ".elementor-post-info__terms-list-item")
  result <- parse_css_board(read_fixture("css_board_rocky_2026-08-07.html"), "https://rocky.edu/employment",
                            "Billings", rule)
  expect_equal(result$Title, c("Admissions Counselor", "Director of Flight Operations",
                               "Adjunct Faculty Position in Theater", "Head Bears Cheer Coach",
                               "Airframe & Powerplant Certified Mechanic with (I/ A) Inspection Authorization",
                               "Adjunct Instructor-Accounting", "Chief Flight Instructor"))
  expect_equal(result$Location, c("Admissions", "Aviation", "Academics", "Athletics", "Aviation", "Faculty", "Aviation"))
  expect_equal(result$Link[c(1, 7)], c("https://rocky.edu/employment/opportunities/admissions-counselor/",
                                       "https://rocky.edu/employment/opportunities/chief-flight-instructor/"))
  expect_true(all(is.na(result$Posted_Date)))
  expect_equal(names(result), c("Title", "Location", "Posted_Date", "Link"))
})

test_that("items: a link selector inside the item and a positional location field", {
  rule <- rule_of(Item_CSS = ".tile.w-dyn-item", Title_CSS = "h2.heading-h4", Link_CSS = "a.btn-secondary",
                  Location_CSS = ".row.can-grow > h3:nth-of-type(2)")
  result <- parse_css_board(read_fixture("css_board_fpcc_2026-08-07.html"), "https://www.fpcc.edu/about-fpcc/employment/",
                            "Main campus", rule)
  expect_equal(result$Title, "Agriculture Assistant")
  expect_equal(result$Location, "Poplar")
  expect_equal(result$Link, paste0("https://cdn.prod.website-files.com/68de85d9627927ed05698b64/",
                                   "69160b4cf0be390b6b317f44_Ag-Assistant-Job-Description.pdf"))
})

test_that("an item missing its location or link falls back to the registry place and the page", {
  html <- paste0('<div class="job"><h2>Cook</h2><span class="loc">Annex</span><a class="go" href="/cook.pdf">x</a></div>',
                 '<div class="job"><h2>Driver</h2></div>')
  rule <- rule_of(Item_CSS = "div.job", Title_CSS = "h2", Link_CSS = "a.go", Location_CSS = ".loc")
  result <- parse_css_board(html, "https://example.org/jobs/", "Main campus", rule)
  expect_equal(result$Title, c("Cook", "Driver"))
  expect_equal(result$Location, c("Annex", "Main campus"))
  expect_equal(result$Link, c("https://example.org/cook.pdf", "https://example.org/jobs/"))
})

test_that("no items: every title node is a row; @title links are made absolute against the page", {
  rule <- rule_of(Title_CSS = ".field--name-body table a[href$='.pdf']", Link_CSS = "@title")
  result <- parse_css_board(read_fixture("css_board_lbhc_2026-08-07.html"), "http://lbhc.edu/Job_opportunities",
                            "Crow Agency", rule)
  expect_equal(result$Title, c("Chief Finance Officer", "Math Instructor", "Ag Instructor", "Web IT specialist"))
  expect_equal(result$Location, rep("Crow Agency", 4))
  expect_equal(result$Link, paste0("http://lbhc.edu/sites/default/files/lbhc/humanresources/",
                                   c("2026_CFO_LBHC_final.pdf", "2026_Math_Instructor_Final.pdf", "Ag_Instructor.pdf",
                                     "Web_IT_Specialist.pdf")))
})

test_that("blank link: every row links to the page; titles are trimmed, blanks dropped, repeats kept", {
  html <- "<ul><li class='t'>  Para  </li><li class='t'> </li><li class='t'>Para</li></ul>"
  result <- parse_css_board(html, "https://example.org/jobs", "Town", rule_of(Title_CSS = "li.t"))
  expect_equal(result$Title, c("Para", "Para"))
  expect_equal(result$Link, rep("https://example.org/jobs", 2))
})

test_that("exclude, include and length rules filter titles, case-insensitively", {
  html <- paste0("<p class='t'>Teacher</p><p class='t'>Application form</p><p class='t'>Custodian</p>",
                 "<p class='t'>", strrep("A teacher who likes long sentences ", 4), "</p>")
  rule <- rule_of(Title_CSS = "p.t", Exclude_Regex = "application", Include_Regex = "teacher|custodian",
                  Max_Title_Chars = "60")
  expect_equal(parse_css_board(html, "https://example.org", "Town", rule)$Title, c("Teacher", "Custodian"))
})

test_that("an empty board gives zero rows with the columns; a missing board marker is an error", {
  rule <- rule_of(Title_CSS = "li.job", Board_CSS = "#openings")
  empty <- parse_css_board("<div id='openings'><p>No openings at this time</p></div>", "https://example.org", "Town", rule)
  expect_equal(nrow(empty), 0)
  expect_equal(names(empty), c("Title", "Location", "Posted_Date", "Link"))
  expect_error(parse_css_board("<div id='news'></div>", "https://example.org", "Town", rule), "board's marker")
})

test_that("css_board_rules() checks the file", {
  df <- function(...) data.frame(Source = "A", Title_CSS = "h2", ..., stringsAsFactors = FALSE)
  expect_equal(names(css_board_rules(df())), CSS_BOARD_RULE_COLUMNS)
  expect_error(css_board_rules(rbind(df(), df())), "listed twice")
  expect_error(css_board_rules(df(Location_CSS = ".loc")), "set Item_CSS")
  expect_error(css_board_rules(df(Link_CSS = "a")), "set Item_CSS")
  expect_silent(css_board_rules(df(Link_CSS = "@title")))
  expect_error(css_board_rules(df(Max_Tries = "six")), "whole number")
  expect_error(css_board_rules(df(Exclude_Regex = "(")), "not a valid regular expression")
  expect_error(css_board_rules(df(Selector = "x")), "unknown column")
  expect_error(css_board_rule(css_board_rules(df()), "B"), "no rule for 'B'")
})

test_that("fetch_css_board_postings() reads the page with as many tries as the rule says", {
  rule <- rule_of(Title_CSS = "li.t", Max_Tries = "6")
  tries <- NULL
  local_mocked_bindings(perform_with_retry = function(req, max_tries = 3, ...) {
    tries <<- max_tries
    httr2::response(200, headers = list("Content-Type" = "text/html"), body = charToRaw("<li class='t'>Cook</li>"))
  })
  expect_equal(fetch_css_board_postings("https://example.org/jobs", "Town", rule)$Title, "Cook")
  expect_equal(tries, 6L)
  fetch_css_board_postings("https://example.org/jobs", "Town", rule_of(Title_CSS = "li.t"))
  expect_equal(tries, 3L)
})

test_that("css_board_spec() resolves a registry row to the fetch call with its rule", {
  rules <- css_board_rules(data.frame(Source = "Test College", Title_CSS = "h2", stringsAsFactors = FALSE))
  sources <- list(scraper_registry(
    data.frame(Institution = "Test College", Platform = "CSSBoard", Feed_URL = "https://example.edu/jobs",
               Town = "Hometown", stringsAsFactors = FALSE),
    "Institution", "registry",
    specs = list(CSSBoard = css_board_spec(function() rules, "Feed_URL", "Town", "Institution"))))
  call <- resolve_scraper_call("Test College", sources)
  expect_equal(call$fn, "fetch_css_board_postings")
  expect_equal(call$args[[1]], "https://example.edu/jobs")
  expect_equal(call$args[[2]], "Hometown")
  expect_equal(call$args[[3]]$Title_CSS, "h2")
})
