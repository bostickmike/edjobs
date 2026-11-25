# Text boards (R/text_board.R). Real pages captured 2026-08-24 and 2026-11-25: postings that each repeat a line
# (Bigfork), postings named in a sentence (Clinton), and lists between fixed headings (Terry).

read_fixture <- function(name) paste(readLines(test_path("fixtures", name), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
rule_of <- function(...) text_board_rule(text_board_rules(data.frame(Source = "Test board", ..., stringsAsFactors = FALSE)),
                                         "Test board")
page <- function(...) paste0("<html><body>", paste0("<p>", c(...), "</p>", collapse = ""), "</body></html>")

test_that("a marker line gives the line before it, with the pay the posting's own lines state", {
  rule <- rule_of(Start_Regex = "^Certified Job Openings", Stop_Regex = "^EEO:",
                  Marker_Regex = "^BIGFORK SCHOOL DISTRICT NO\\. 38$", Pay_Regex = "^Salary ranging|^Rate of pay")
  result <- parse_text_board(read_fixture("text_board_bigfork_2026-11-25.html"), "https://x.org/jobs", "Bigfork", rule)
  expect_equal(result$Title, c(
    "Full-Time Temporary Custodian", "Food Service Associate 6 hours/day", "Full Time Instructional Paraprofessional",
    "Full Time High School Special Education Paraprofessional", "Substitute Bus Driver", "Substitute Food Service Worker",
    "Temporary Substitute Custodian", "Head Boys HS Baseball Coach",
    "2026-2027 High School Girls Basketball JV/Assistant Varsity Coach",
    "2026-2027 Middle School Girls Basketball Coaches (1 volunteer position and one paid position)"))
  expect_equal(unique(result$Location), "Bigfork")
  expect_equal(unique(result$Link), "https://x.org/jobs")
  expect_true(all(is.na(result$Posted_Date)))
  expect_equal(result$Pay_Min, c(16.97, NA, NA, NA, 20, NA, NA, NA, NA, NA))
  expect_equal(result$Pay_Max, c(20.04, NA, NA, NA, 20, NA, NA, NA, NA, NA))
  expect_equal(result$Pay_Unit[c(1, 5)], c("hour", "hour"))
})

test_that("a title pattern's capture group is the title, its first letter a capital", {
  rule <- rule_of(Title_Regex = "^Clinton School is looking for (?:an? )?(.+?)(?= for | to |[.]|$)", Pay_Regex = "Daily rate")
  result <- parse_text_board(read_fixture("text_board_clinton_2026-11-25.html"), "u", "Clinton", rule)
  expect_equal(result$Title, c("Substitute Teachers", "Paraprofessionals"))
  expect_equal(result$Pay_Min, c(95, NA))
  expect_equal(result$Pay_Unit, c("day", NA))
})

test_that("several rows for one source are sections, read in order", {
  rule <- text_board_rule(text_board_rules(data.frame(
    Source = c("T", "Other", "T"), Start_Regex = c("^CERTIFIED Employment Application$", "^x", "^We are taking applications on an ongoing basis"),
    Stop_Regex = c("^CLASSIFIED EMPLOYMENT INFORMATION", "", "^Applications materials may be submitted"),
    Exclude_Regex = c("", "", "^The following positions are currently available"))), "T")
  expect_equal(nrow(rule), 2)
  result <- parse_text_board(read_fixture("text_board_terry_2026-08-24.html"), "u", "Terry", rule)
  expect_equal(result$Title, c(
    "3 Elementary Teachers", "Part time Music Teacher for K \u2013 12", "Part time Art Teacher for K \u2013 12",
    "Physical Education Teacher", "Part time Family \u2013 Consumer Science Teacher for grades 7 \u2013 12",
    "Part time Business Teacher for grades 7 \u2013 12", "Title 1 Elementary Teacher", "Special Education Paraprofessional",
    "Expedition Route Driver", "Kitchen sub", "Sub Teacher/Aide", "Bus Driver", "Sub Bus Driver", "Sub custodian"))
})

test_that("a posting under a section's last heading is read", {
  rule <- rule_of(Start_Regex = "^Ongoing", Stop_Regex = "^Apply", Exclude_Regex = "^Available now")
  expect_equal(parse_text_board(page("Ongoing:", "Bus Driver", "Available now:", "Head Cook", "Apply here"), "u", "X", rule)$Title,
               c("Bus Driver", "Head Cook"))
})

test_that("strip, exclude, length and invisible characters", {
  rule <- rule_of(Start_Regex = "^Jobs$", Exclude_Regex = "Application$", Strip_Regex = " - .*$|^[* ]+", Max_Title_Chars = "20")
  result <- parse_text_board(page("Jobs", "\u200bCook\u00a0Helper - 6 hours", "* Bus Driver", "Classified Application",
                                  "This line is a paragraph about the district", "- all of it", "Bus Driver"), "u", "X", rule)
  expect_equal(result$Title, c("Cook Helper", "Bus Driver", "- all of it", "Bus Driver"))
})

test_that("pay is read only from a posting's own lines that Pay_Regex names", {
  html <- page("Jobs", "Cook", "Pay: $15 per hour", "Teacher", "Supplemental income of $7950/year in lieu of insurance")
  titles <- "^(Cook|Teacher)$"
  expect_true(all(is.na(parse_text_board(html, "u", "X", rule_of(Start_Regex = "^Jobs$", Title_Regex = titles))$Pay_Min)))
  result <- parse_text_board(html, "u", "X", rule_of(Start_Regex = "^Jobs$", Title_Regex = titles, Pay_Regex = "^Pay:"))
  expect_equal(result$Pay_Min, c(15, NA))
})

test_that("a missing start or stop is an error, not an empty board; an empty section is no postings", {
  rule <- rule_of(Start_Regex = "^Openings$", Stop_Regex = "^Apply")
  expect_error(parse_text_board(page("Openings", "Cook"), "https://x.org", "X", rule), "Stop_Regex '\\^Apply'.*https://x.org")
  expect_error(parse_text_board(page("Cook", "Apply"), "u", "X", rule), "Start_Regex")
  expect_error(parse_text_board("site under maintenance", "u", "X", rule), "no HTML")
  empty <- parse_text_board(page("Apply by mail", "Openings", "Apply here"), "u", "X", rule)
  expect_equal(nrow(empty), 0)
  expect_equal(names(empty), c("Title", "Location", "Posted_Date", "Link", "Pay_Min", "Pay_Max", "Pay_Unit", "Pay_Text"))
})

test_that("rules are checked", {
  d <- function(...) data.frame(Source = "A", ..., stringsAsFactors = FALSE)
  expect_error(text_board_rules(d(Stop_Regex = "x")), "set Start_Regex, Title_Regex or Marker_Regex for A")
  expect_error(text_board_rules(d(Title_Regex = "x", Marker_Regex = "y")), "not both, for A")
  expect_error(text_board_rules(d(Title_Regex = "(")), "Title_Regex for A is not a valid regular expression")
  expect_error(text_board_rules(d(Title_Regex = "x", Max_Title_Chars = "many")), "whole number for A")
  expect_error(text_board_rules(d(Title_Regex = "x", Title_CSS = "h2")), "unknown column\\(s\\): Title_CSS")
  expect_error(text_board_rules(data.frame(Title_Regex = "x")), "needs a Source column")
  expect_error(text_board_rule(d(Title_Regex = "x"), "B"), "no rule for 'B'")
  f <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(d(Title_Regex = "^(.+) wanted$"), f, row.names = FALSE)
  expect_equal(text_board_rule(f, "A")$Title_Regex, "^(.+) wanted$")
})

test_that("the dispatch spec passes the page, the place and the source's rule", {
  spec <- text_board_spec(function() data.frame(Source = "A", Title_Regex = "x"), "Job_Link", "City", "District")
  expect_equal(spec$fn, "fetch_text_board_postings")
  expect_equal(spec$parse_fn, "parse_text_board")
})
