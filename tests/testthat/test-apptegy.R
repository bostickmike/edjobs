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

test_that("titles inside a sentence, unless the sentence restates the title above it", {
  expect_equal(apptegy_title_lines("Openings:\nBig Timber Grade School is accepting applications for a SPED Teacher.\nBig Timber Grade School is also accepting applicants for Substitute Teachers."),
               c("SPED Teacher", "Substitute Teachers"))
  expect_equal(apptegy_title_lines("Assistant Cook\nAlberton School is seeking a dependable and enthusiastic Assistant Cook to join our team!"),
               "Assistant Cook")
  # Not a role, or about the reader
  expect_equal(apptegy_title_lines("Cut Bank Schools is looking for talented educators like YOU who want to make a difference."), character(0))
})

test_that("Google Sites-style lines: contacts, document links, pay asides, glued headers, labels", {
  expect_equal(apptegy_title_lines("Superintendent / Principal: Christy Bortz\nJH Football: Head Coach"), "JH Football: Head Coach")
  expect_equal(apptegy_title_lines("Openings:\nPart-time Bus Driver.pdf\nVeterans' Preference.pdf"), "Part-time Bus Driver")
  r <- parse_apptegy_postings("Openings:\nSubstitute Bus Drivers (Route sub=$18.10/hr. Activity trips=$18.10/hr.)", "u", "x")
  expect_equal(r$Title, "Substitute Bus Drivers")
  expect_equal(r$Pay_Min, 18.1)
  expect_equal(apptegy_title_lines("Certified Positions:No positions open at this time.Aide Positions:Paraprofessional (Full Time)"),
               "Paraprofessional (Full Time)")
  expect_equal(apptegy_title_lines("Openings:\nSPED Case Manager Opening\nCOACHES\nNo positions are currently available\nMS/HS Positions"),
               "SPED Case Manager")
})

test_that("job-description file names and camel-case file names keep the job (WY's Weston 1, MT's Fromberg)", {
  expect_equal(apptegy_title_lines("Open Positions\nSpeech Language Pathologist Job Description\nJob Description - Music Teacher K12\nAssistantBoysSoccerCoach"),
               c("Speech Language Pathologist", "Music Teacher K12", "Assistant Boys Soccer Coach"))
  expect_equal(apptegy_title_lines("Current Postings\nSubsMusicTeachingPosition.pdf"), "Subs Music Teaching Position")
  # A job-description heading alone, or one naming no role, is still not a title
  expect_equal(apptegy_title_lines("Openings:\nJob Description\nBoard Policy Job Description"), character(0))
})

test_that("a 'Substitute <job> Positions' line with nothing under it is a posting (WY's Sheridan 3)", {
  expect_equal(apptegy_title_lines("JH Assistant Basketball Coaches\nSubstitute Custodial Positions\nSheridan County School District #3 is a place where educators come to do deeply meaningful work in a setting that feels personal."),
               c("JH Assistant Basketball Coaches", "Substitute Custodial Positions"))
  # A header over a list stays a header; generic kinds stay headers
  expect_equal(apptegy_title_lines("Substitute Custodial Positions\nNight Custodian"), "Night Custodian")
  expect_equal(apptegy_title_lines("Bus Driver\nSubstitute Teaching Positions"), "Bus Driver")
})

test_that("a numbered list item on its own line loses its number (MT's Helena Flats)", {
  expect_equal(apptegy_title_lines("Openings:\n1. Maintenance/Custodial Supervisor\n2. Special Education Paraprofessional"),
               c("Maintenance/Custodial Supervisor", "Special Education Paraprofessional"))
  # Several numbered items on one line are still split
  expect_equal(apptegy_title_lines("Openings:\n1. Bus Driver 2. Custodian"), c("Bus Driver", "Custodian"))
})

test_that("Victor-style lists: a slash header, a count of positions, a lowercase substitute list", {
  expect_equal(apptegy_title_lines("Athletics/Coaching\nHead High School Track Coach\nClassified Staff Openings:\n3 Paraprofessional positions\nSubstitutes for the following positions:\nteachers\nkitchen\n2026-27"),
               c("Head High School Track Coach", "Paraprofessional", "Substitutes"))
})

# ---------------------------------------------------------------------------
# Passed application deadlines (Mike 11-14: "still open" = a candidate can apply)
# ---------------------------------------------------------------------------

test_that("a posting past its own application deadline is left out (Weston County SD 7, 2026-11-14)", {
  # Real rendered page: "School Maintenance Technician ... Applications will be
  # accepted until 5:00 PM on Thursday October 29, 2026." and "Special Education
  # Paraprofessional ... will close at 1:00 p.m. on Monday, November 30, 2026."
  page <- read_fixture("apptegy_weston7_rendered_2026-11-14.txt")
  skip <- "^(Subs Needed|WCSD #7 is currently accepting applications for:)$"
  kept <- function(d) parse_apptegy_postings(page, "u", "Upton", "text", NA, NA, skip, deadline_date = d)$Title
  expect_message(on_run <- kept(as.Date("2026-11-14")), "past their own application deadline.*School Maintenance Technician")
  expect_equal(on_run, c("Special Education Paraprofessional", "Substitute Teacher", "Substitute Paraprofessional",
                         "Substitute Custodian", "Substitute Bus Driver (CDL preferred but willing to train)",
                         "Substitute Lunchroom Cook"))
  # Before 10-29 both are open; after 11-30 the paraprofessional goes too.
  expect_true("School Maintenance Technician" %in% kept(as.Date("2026-10-20")))
  expect_false("Special Education Paraprofessional" %in% suppressMessages(kept(as.Date("2026-12-01"))))
  # NA turns the check off.
  expect_true("School Maintenance Technician" %in% kept(NA))
})

test_that("a deadline needs a cue and a full date with its year", {
  rows <- function(...) data.frame(Title = c("Cook", "Bus Driver"), Line = c("Cook", "Bus Driver"))
  passed <- function(block) apptegy_passed_deadline(rows(), paste(c("Cook", block, "Bus Driver"), collapse = "\n"),
                                                    as.Date("2026-11-14"))
  expect_equal(passed("Applications due 10/1/2026."), c(TRUE, FALSE))
  expect_equal(passed("Closing date: Oct. 3rd, 2026"), c(TRUE, FALSE))
  expect_equal(passed("Deadline 9/30/26"), c(TRUE, FALSE))
  expect_equal(passed("Open until filled."), c(FALSE, FALSE))
  expect_equal(passed("Applications due May 20."), c(FALSE, FALSE))          # no year
  expect_equal(passed("Renewed in April 2021 and on August 17, 2021."), c(FALSE, FALSE))  # no cue
  expect_equal(passed("Position closes December 4, 2026"), c(FALSE, FALSE))  # still ahead
  # Open until filled: a candidate can still apply, whatever date the text gives (2026-11-17 review, item 4).
  expect_equal(passed("Open until filled. Applications received by October 1, 2026 get first consideration."), c(FALSE, FALSE))
  expect_equal(passed(c("Applications due 10/1/2026", "or until the position is filled.")), c(FALSE, FALSE))
  expect_equal(passed("Review of applications begins October 1, 2026; open until filled"), c(FALSE, FALSE))
  expect_equal(passed("Priority deadline: October 1, 2026"), c(FALSE, FALSE))
  expect_equal(passed("First review due October 1, 2026"), c(FALSE, FALSE))
  expect_equal(passed("Applications will be accepted until 5:00 PM on Thursday October 29, 2026."), c(TRUE, FALSE))
  expect_equal(apptegy_passed_deadline(data.frame(Title = character(0), Line = character(0)), "x"), logical(0))
})
