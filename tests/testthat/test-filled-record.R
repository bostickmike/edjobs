# Filled-postings record (R/filled_record.R, 2026-11-26).

rec <- function(...) filled_record(data.frame(..., stringsAsFactors = FALSE))

test_that("the record is checked: required fields, dates, a web address, Until after the record", {
  ok <- rec(District = "Custer Public Schools", Title = "PE Teaching Position", Evidence = "Board agenda item 6b: hire of a PE teacher",
            Source_URL = "https://example.org/agenda.pdf", Record_Date = "2026-07-22", Until = "2027-06-30")
  expect_equal(names(ok), FILLED_RECORD_COLUMNS)
  expect_identical(ok$Posted_Date, "")
  expect_error(rec(District = "A", Title = "T", Evidence = "", Source_URL = "https://x", Record_Date = "2026-07-22", Until = "2027-06-30"), "Evidence is blank")
  expect_error(rec(District = "A", Title = "T", Evidence = "e", Source_URL = "x", Record_Date = "2026-07-22", Until = "2027-06-30"), "web address")
  expect_error(rec(District = "A", Title = "T", Evidence = "e", Source_URL = "https://x", Record_Date = "07/22/2026", Until = "2027-06-30"), "YYYY-MM-DD")
  expect_error(rec(District = "A", Title = "T", Evidence = "e", Source_URL = "https://x", Record_Date = "2026-07-22", Until = "2026-07-01"), "Until is before")
  expect_error(rec(District = "A", Title = "T", Evidence = "e", Source_URL = "https://x", Record_Date = "2026-07-22", Until = "2027-06-30", Who = "x"), "unknown column")
})

test_that("rows match on district and title (case, spacing ignored), and on the posted date when the entry gives one", {
  r <- rec(District = c("Custer Public Schools", "Big Timber Grade School"), Title = c("PE Teaching Position", "History"),
           Posted_Date = c("", "2026-06-17"), Evidence = c("hire of a PE teacher", "hire of a JH history teacher"),
           Source_URL = "https://example.org/m.pdf", Record_Date = c("2026-07-22", "2026-08-12"), Until = "2027-06-30")
  df <- data.frame(District = c("Custer Public Schools", "Custer Public Schools", "Big Timber Grade School", "Big Timber Grade School"),
                   title = c("pe  teaching position", "Music Teaching Position", "History", "History"),
                   date_posted = c(NA, NA, "2026-06-17", "2026-10-01"), stringsAsFactors = FALSE)
  why <- filled_record_reason(df, r, as.Date("2026-11-30"))
  expect_equal(why[1], "filled per public record (2026-07-22): hire of a PE teacher")
  expect_true(is.na(why[2]))
  expect_match(why[3], "^filled per public record \\(2026-08-12\\)")
  expect_true(is.na(why[4]))                                   # a later ad of the same job counts
  expect_true(is_stale_flagged(why[1]) && !is_stale_flagged(why[2]))
})

test_that("an entry stops applying after its Until date, and an empty record or frame flags nothing", {
  r <- rec(District = "A", Title = "T", Evidence = "e", Source_URL = "https://x", Record_Date = "2026-07-22", Until = "2027-06-30")
  df <- data.frame(District = "A", title = "T", stringsAsFactors = FALSE)
  expect_false(is.na(filled_record_reason(df, r, as.Date("2027-06-30"))))
  expect_true(is.na(filled_record_reason(df, r, as.Date("2027-07-01"))))
  expect_equal(filled_record_reason(df[0, ], r, Sys.Date()), character(0))
  expect_true(is.na(filled_record_reason(df, r[0, ], Sys.Date())))
})
