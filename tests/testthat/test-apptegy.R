# The Apptegy family routine (R/apptegy.R). Fixtures are MT district pages
# captured for the per-district scrapers this routine replaced; expected titles
# are the routine's output as checked against those scrapers
# (claude-notes/2026-11-14_apptegy-family/parity.R).

read_fixture <- function(name) paste(readLines(test_path("fixtures", name), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

test_that("apptegy_split_line cuts details off a title, not title parts", {
  expect_equal(apptegy_split_line("1. Assistant Cook 2. Activities Bus Driver"), c("Assistant Cook", "Activities Bus Driver"))
  expect_equal(apptegy_split_line("Substitute Route Drivers (CDL): $23.00/hour"), "Substitute Route Drivers (CDL)")
  expect_equal(apptegy_split_line("Bus Drivers- Must have a CDL with a school bus passenger endorsement."), "Bus Drivers")
  # Title parts stay
  expect_equal(apptegy_split_line("Music - Instrumental"), "Music - Instrumental")
  expect_equal(apptegy_split_line("JH Football: Head Coach"), "JH Football: Head Coach")
  expect_equal(apptegy_split_line("Bus Driver - Substitutes"), "Bus Driver - Substitutes")
  expect_equal(apptegy_split_line("Assistant H.S. Boys Basketball Coach"), "Assistant H.S. Boys Basketball Coach")
  # A separator inside parentheses doesn't count; a trailing aside after a title part goes
  expect_equal(apptegy_split_line("Food Service Worker (Can be temporary position Oct. 2026-Dec 2026)- Job Description, Vacancy"),
               "Food Service Worker (Can be temporary position Oct. 2026-Dec 2026)")
  expect_equal(apptegy_split_line("Long Term Substitute - Kindergarten (Approximate start date is May 1st, 2027)"),
               "Long Term Substitute - Kindergarten")
  # "Title. Sentence"
  expect_equal(apptegy_split_line("Co-head HS Winter Cheerleading. This will be a shared"), "Co-head HS Winter Cheerleading")
})

test_that("apptegy_title_lines: headers, a skipped section, footer and boilerplate", {
  text <- paste(c("Employment", "Current Openings", "Head Cook", "Para-educator (Bona fide qualifications) needed",
                  "Please submit an application to the district office.", "Questions? Call 406-555-1234",
                  "CURRENTLY FILLED:", "Bus Driver", "Find Us", "Custodian"), collapse = "\n")
  expect_equal(apptegy_title_lines(text, skip_section = "CURRENTLY FILLED"), c("Head Cook", "Para-educator (Bona fide qualifications) needed"))
  # A start rule that matches nothing: no titles (the page changed)
  expect_equal(apptegy_title_lines(text, start = "^JOB OPENINGS$"), character(0))
  # A wrapped all-caps title continues
  expect_equal(apptegy_title_lines("Openings:\nSEEKING TWO (2) EVENING SHIFT SCHOOL CUSTODIANS\nAND ONE (1) MID SHIFT SCHOOL CUSTODIAN"),
               "TWO (2) EVENING SHIFT SCHOOL CUSTODIANS AND ONE (1) MID SHIFT SCHOOL CUSTODIAN")
  expect_equal(apptegy_title_lines(NA), character(0))
  expect_equal(apptegy_title_lines(""), character(0))
})

test_that("parse_apptegy_postings: Plevna (start/stop rules; role-less lines take the header's role)", {
  r <- parse_apptegy_postings(read_fixture("apptegy_plevna_rendered.txt"), "https://example.org/jobs", "Plevna", "text",
                              start = "^[0-9]{4}-[0-9]{4} School Year Positions Open:$", stop = "^Full Family Health Insurance Offered$")
  expect_equal(r$Title, c("7-12 Broadfield Science", "Math", "Basketball Coach - Jr. High/Elem. - Asst. Girls",
                          "Basketball Coach - Jr. High/Elem. Asst. Boys", "Track Coach - High School - Head",
                          "Track Coach - High School - Asst.", "Track Coach - Jr. High - Head", "Track Coach - Jr. High - Asst.",
                          "Bus Driver - Substitutes", "Classroom - Substitutes"))
  expect_true(all(r$Location == "Plevna"))
  expect_true(all(is.na(r$Posted_Date)))
  expect_true(all(r$Link == "https://example.org/jobs"))
  expect_true(all(c("Pay_Min", "Pay_Max", "Pay_Unit", "Pay_Text") %in% names(r)))
})

test_that("parse_apptegy_postings: St Regis keeps head and assistant coaches apart", {
  r <- parse_apptegy_postings(read_fixture("apptegy_stregis_rendered.txt"), "u", "St. Regis")
  expect_true(all(c("JH Football: Head Coach", "JH Football: Asst. Coach", "JH Volleyball: Asst. Coach") %in% r$Title))
  expect_equal(nrow(r), 14)
})

test_that("parse_apptegy_postings: Melstone's posted pay comes from each title's own line", {
  r <- parse_apptegy_postings(read_fixture("apptegy_melstone_rendered.txt"), "u", "Melstone")
  expect_equal(r$Pay_Min[r$Title == "Substitute Route Drivers (CDL)"], 23)
  expect_equal(r$Pay_Unit[r$Title == "Substitute Route Drivers (CDL)"], "hour")
  expect_equal(r$Pay_Min[r$Title == "Activity Drivers (non-CDL)"], 16)
  expect_true(is.na(r$Pay_Min[r$Title == "Head Cook"]))
  # Lines that differ only in a parenthetical stay two postings
  expect_true(all(c("Activity Drivers (CDL)", "Activity Drivers (non-CDL)") %in% r$Title))
})

test_that("parse_apptegy_postings: Deer Lodge page-builder cards", {
  html <- read_fixture("apptegy_deerlodge_pagedata.html")
  expect_false(is.null(decode_apptegy_pagebuilder_nodes(html)))
  r <- parse_apptegy_postings(html, "u", "Deer Lodge", "pagebuilder")
  expect_equal(r$Title, c("HS Special Ed Aide", "Elementary PE Teacher with a period of Intervention", "Substitutes Wanted",
                          "Special Education Teacher 5th/6th grades", "Bus Driver"))
  # Text mode on a page-builder page, or a page with no page-builder data: nothing, not an error
  expect_equal(nrow(parse_apptegy_postings("<html></html>", "u", "x", "pagebuilder")), 0)
  expect_null(decode_apptegy_pagebuilder_nodes(NA))
})

test_that("apptegy_pagebuilder_lines: a card saying no openings adds nothing", {
  nodes <- list(list(type = "CONTENT_NODE_CARD", content = list(
    heading = list(content = list(html = "<p>Bus Driver</p>")),
    text = list(content = list(html = "<p>No current openings</p>")))),
    list(type = "CONTENT_NODE_CARD", content = list(
      heading = list(content = list(html = "<p>Custodian</p>")),
      text = list(content = list(html = "<p>Apply at the office</p>")))))
  expect_equal(apptegy_pagebuilder_lines(nodes), "Custodian")
  expect_equal(apptegy_html_fragment_to_lines("<p>A &amp; B</p><li>C</li>"), c("A & B", "C"))
})

test_that("fetch_apptegy_postings reads innerText or the HTML through the session", {
  seen <- character(0)
  fake <- list(Page = list(navigate = function(url) NULL, loadEventFired = function(...) NULL),
               Runtime = list(evaluate = function(js) { seen <<- c(seen, js); list(result = list(value = "Openings:\nHead Cook")) }))
  local_mocked_bindings(Sys.sleep = function(...) NULL, .package = "base")
  r <- fetch_apptegy_postings(fake, "u", "Town")
  expect_equal(r$Title, "Head Cook")
  expect_equal(seen, "document.body.innerText")
  fetch_apptegy_postings(fake, "u", "Town", mode = "pagebuilder")
  expect_equal(seen[2], "document.documentElement.outerHTML")
})

test_that("auto-fix evidence: the routine reads innerText, and a family's per-district tests give that district's fixtures", {
  expect_true(scraper_reads_inner_text("fetch_apptegy_postings", character(0)))
  expect_false(scraper_reads_inner_text("fetch_cyberschool_postings", character(0)))
  expect_false(scraper_reads_inner_text("no_such_fn", character(0)))
  tests <- list(`test-apptegy-family.R` = c(
    'test_that("parse_apptegy_postings: Melstone Public Schools", {',
    '  x <- test_path("fixtures", "apptegy_melstone_rendered.txt")',
    '})',
    'test_that("parse_apptegy_postings: Plevna School District #55", {',
    '  x <- test_path("fixtures", "apptegy_plevna_rendered_2026-11-01.txt")',
    '  y <- test_path("fixtures", "apptegy_plevna_rendered.txt")',
    '})'))
  expect_equal(fixtures_used_by_tests("fetch_apptegy_postings", tests, "Plevna School District #55"),
               c("apptegy_plevna_rendered_2026-11-01.txt", "apptegy_plevna_rendered.txt"))
  expect_equal(fixtures_used_by_tests("fetch_apptegy_postings", tests, "Melstone Public Schools"), "apptegy_melstone_rendered.txt")
  # No block names the source: every block's fixtures, as before
  expect_length(fixtures_used_by_tests("fetch_apptegy_postings", tests, "Elsewhere"), 3)
  expect_length(fixtures_used_by_tests("fetch_apptegy_postings", tests), 3)
})
