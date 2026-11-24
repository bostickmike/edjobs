# Stale board ads (R/stale_board_postings.R). Titles and dates are real board rows from the 2026-11-23/11-20
# archives the rule was measured on (claude-notes/2026-11-24_stale-ats-ads/README.md, logs_variants.txt).

test_that("the cutoff is the start of the previous school year", {
  expect_equal(stale_cutoff_date(as.Date(c("2026-11-24", "2026-07-01", "2026-06-30", "2027-01-15"))),
               as.Date(c("2025-07-01", "2025-07-01", "2024-07-01", "2025-07-01")))
  expect_equal(stale_cutoff_date("2026-11-24", years_back = 0), as.Date("2026-07-01"))
  expect_equal(stale_cutoff_date("2026-11-24", school_year_start = "08-15"), as.Date("2025-08-15"))
  expect_equal(stale_cutoff_date("2026-08-01", school_year_start = "08-15"), as.Date("2024-08-15"))
  expect_true(is.na(stale_cutoff_date(NA)))
})

test_that("stale_board_rule() checks its settings", {
  expect_s3_class(stale_board_rule(), "edjobs_stale_rule")
  expect_error(stale_board_rule("7-1"), "MM-DD")
  expect_error(stale_board_rule("02-30"), "MM-DD")
  expect_error(stale_board_rule(years_back = -1), "whole number")
  expect_error(stale_board_rule(years_back = 1.5), "whole number")
  expect_error(stale_board_rule(exempt_patterns = "(unclosed"), "valid perl pattern")
  expect_error(stale_board_reason("x", "2020-01-01", NA, "2026-11-24", rule = list()), "stale_board_rule")
})

test_that("a posted or closing date before the cutoff makes a board ad stale", {
  run <- as.Date("2026-11-24")
  r <- stale_board_reason(
    c("High School Principal", "Elementary Classroom Teacher K-4", "Mathematics Teacher (Grades 7-12)",
      "Special Education Teacher Grades 7-12", "2026-27 Math Teacher", "Physical Education/Health Teacher - Middle School",
      "Speech Language Pathologist", "No dates"),
    c("2024-04-01", "2024-05-21", "2023-03-08", "2025-05-15", "2026-03-02", "2026-09-20", NA, NA),
    c("Until Filled", "06/30/2024", "Until filled", NA, NA, "10/15/2026", "06/30/2024", "TBD"),
    run)
  expect_equal(r, c(STALE_POSTED_REASON, STALE_POSTED_REASON, STALE_POSTED_REASON, STALE_POSTED_REASON, NA,
                    NA,                     # a closing date this school year that passed is not the rule (R1's false drops)
                    STALE_CLOSED_REASON,    # no posted date, a closing date in a past school year
                    NA))
  # The day before the cutoff is stale; the cutoff itself is not.
  expect_equal(stale_board_reason(c("a", "b"), c("2025-06-30", "2025-07-01"), NA, run), c(STALE_POSTED_REASON, NA))
  # m/d/Y and "Nov 9, 2026" dates read too; text that isn't a date is no date.
  expect_equal(stale_board_reason(c("a", "b", "c"), c("4/1/2024", "Apr 1, 2024", "Posted 3 weeks ago"), NA, run),
               c(STALE_POSTED_REASON, STALE_POSTED_REASON, NA))
  expect_identical(stale_board_reason(character(0), character(0), character(0), run), character(0))
})

test_that("standing pools and ongoing roles are never stale; instructional coaches are judged by their dates", {
  old <- "2023-06-07"
  kept <- c("District-Wide Substitute", "Substitute Teachers", "Bus Driver", "Relief Bus Driver", "Bus Monitor",
            "Bus Aide", "Route Bus Driver", "Paraprofessional (7-12)", "Paraeducator Sign Support [XP05]",
            "Academic Achievement Center Para Educator", "ELL Paraprofessional", "Instructional Aide",
            "Playground - Lunch Room Aide FY2425_94", "Custodian", "Maintenance Manager", "Seasonal House Cleaning Laborer",
            "Kitchen Staff - Cook", "Food Service Worker", "Girls Head Basketball Coach (Jr. High)", "Head Coach or Sponsor",
            "MS or Assistant Coach/Sponsor", "High School Assistant Football Coach", "GRHS Assistant Girls Soccer Coach",
            "TRHS Assistant Track Coach")
  expect_true(all(is.na(stale_board_reason(kept, old, NA, "2026-11-24"))))
  judged <- c("Instructional Coach/Testing/Curriculum Coordinator", "Academic Coach", "Literacy Coach",
              "Driver Education Teacher", "Athletics and Activities Director", "Speech Pathologist",
              "Occupational Therapist(s)", "Superintendent", "Campus Supervisor", "Paradise Valley Teacher")
  expect_equal(stale_board_reason(judged, old, NA, "2026-11-24"), rep(STALE_POSTED_REASON, length(judged)))
})

test_that("exemptions are the rule's data: a state can change them", {
  rule <- stale_board_rule(exempt_patterns = c(pool = STANDING_POOL_PATTERN))
  expect_equal(stale_board_reason(c("Bus Driver", "Substitute"), "2020-01-01", NA, "2026-11-24", rule),
               c(STALE_POSTED_REASON, NA))
  none <- stale_board_rule(exempt_patterns = character(0))
  expect_equal(stale_board_reason("Substitute", "2020-01-01", NA, "2026-11-24", none), STALE_POSTED_REASON)
})

test_that("flag_stale_board_postings() flags board rows only, keeps every row, and reads the state's rule", {
  withr::defer(set_state_config(NULL))
  set_state_config(NULL)
  df <- data.frame(title = c("High School Principal", "Guidance Counselor", "Administrative - Principal", "Bus Driver"),
                   date_posted = c("2024-04-01", "2024-01-14", "2024-08-24", "2022-01-07"),
                   Close_Date = c("Until Filled", NA, NA, "2/01/2022"), stringsAsFactors = FALSE)
  feed <- c(FALSE, FALSE, TRUE, FALSE)
  out <- flag_stale_board_postings(df, "2026-11-24", board = !feed)
  expect_equal(nrow(out), nrow(df))
  expect_equal(out$Stale_Reason, c(STALE_POSTED_REASON, STALE_POSTED_REASON, NA, NA))
  expect_equal(drop_stale_rows(out)$title, c("Administrative - Principal", "Bus Driver"))
  # A state's rule (here: cutoff at the current school year's start, no exemptions) from state_config().
  set_state_config(state_config("T", "00", "app", stale_rule = stale_board_rule(years_back = 0, exempt_patterns = character(0))))
  out2 <- flag_stale_board_postings(df, "2026-11-24", board = !feed)
  expect_equal(out2$Stale_Reason, c(STALE_POSTED_REASON, STALE_POSTED_REASON, NA, STALE_POSTED_REASON))
  expect_error(state_config("T", "00", "app", stale_rule = list()), "stale_board_rule")
  # An explicit rule wins over the state's.
  expect_equal(flag_stale_board_postings(df, "2026-11-24", rule = stale_board_rule(), board = !feed)$Stale_Reason,
               out$Stale_Reason)
  # Missing date columns are no dates; an empty frame gets the column.
  expect_true(all(is.na(flag_stale_board_postings(df["title"], "2026-11-24")$Stale_Reason)))
  e <- flag_stale_board_postings(df[0, ], "2026-11-24")
  expect_identical(e$Stale_Reason, character(0))
})

test_that("a reason already set is kept; feed rows that are the same ad are marked", {
  df <- data.frame(title = c("a", "b"), date_posted = c("2026-09-01", "2026-09-01"),
                   Stale_Reason = c(STALE_FEED_LINK_REASON, NA), stringsAsFactors = FALSE)
  expect_equal(flag_stale_board_postings(df, "2026-11-24")$Stale_Reason, c(STALE_FEED_LINK_REASON, NA))
  feed <- data.frame(title = c("Occupational Therapist(s)", "Teacher"), stringsAsFactors = FALSE)
  m <- mark_stale_feed_rows(feed, c(TRUE, FALSE))
  expect_equal(m$Stale_Reason, c(STALE_FEED_LINK_REASON, NA))
  expect_identical(mark_stale_feed_rows(feed[0, , drop = FALSE], logical(0))$Stale_Reason, character(0))
})

test_that("rows read back from an archive: NA and blank are not flagged; archives without the column are unchanged", {
  path <- withr::local_tempfile(fileext = ".csv")
  df <- data.frame(title = c("a", "b", "c"), Stale_Reason = c(STALE_POSTED_REASON, NA, ""), stringsAsFactors = FALSE)
  utils::write.csv(df, path, row.names = FALSE)
  back <- utils::read.csv(path, colClasses = "character")
  expect_equal(is_stale_flagged(back$Stale_Reason), c(TRUE, FALSE, FALSE))
  expect_equal(drop_stale_rows(back)$title, c("b", "c"))
  old <- data.frame(title = "a")
  expect_identical(drop_stale_rows(old), old)
  all_na <- utils::read.csv(text = "title,Stale_Reason\na,NA\n")   # read as logical NA
  expect_equal(nrow(drop_stale_rows(all_na)), 1)
})
