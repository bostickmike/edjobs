# write_reference_file() / reference_table_problem() (scrape_helpers.R):
# a failed, partial or implausible refresh of a reference table must keep
# the committed file. Identical in the MT and WY repos.

# Real reference tables from the MT repo (Mt_Ed_Jobs/, 2026-11-10) as fixtures; each
# state repo still checks its own committed tables against these rules.
# (absolute: several tests change into a temp directory first)
fixture_dir <- normalizePath(test_path("fixtures"))
committed <- function(name) read.csv(file.path(fixture_dir, paste0("reference_", sub("\\.csv$", "", name), "_mt_2026-11-10.csv")),
                                     stringsAsFactors = FALSE)

test_that("write_reference_file keeps the committed QCEW file when a refresh comes back partial", {
  withr::local_dir(withr::local_tempdir())
  good <- committed("qcew_county_school_pay.csv")
  path <- file.path(getwd(), "qcew_county_school_pay.csv")
  write.csv(good, path, row.names = FALSE)
  partial <- good[seq_len(max(1, floor(nrow(good) * 0.5))), ]
  if (nrow(good) >= 2) {
    expect_false(suppressMessages(write_reference_file(partial, path)))
    expect_equal(nrow(read.csv(path)), nrow(good))
    expect_match(read.csv("scrape_log.csv")$error_message, "rows, down from")
  }
  expect_true(write_reference_file(good, path))
  expect_equal(utils::tail(read.csv("scrape_log.csv")$status, 1), "ok")
})

test_that("reference_table_problem catches out-of-range values and blank required columns", {
  rules <- REFERENCE_TABLE_RULES$ipeds_staff_salaries
  staff <- committed("ipeds_staff_salaries.csv")
  expect_null(reference_table_problem(staff, rules, staff))
  thousands <- staff; thousands$Avg_Salary <- thousands$Avg_Salary / 1000   # "$52.3" thousand
  expect_match(reference_table_problem(thousands, rules, staff), "Avg_Salary value")
  blank <- staff; blank$Avg_Salary <- NA
  expect_match(reference_table_problem(blank, rules, staff), "blank")
  expect_equal(reference_table_problem(staff[0, ], rules, staff), "no rows")
  expect_match(reference_table_problem(staff[, setdiff(names(staff), "Avg_Salary")], rules), "no Avg_Salary column")
})

test_that("a failed refresh is logged without touching the file", {
  withr::local_dir(withr::local_tempdir())
  path <- file.path(getwd(), "oews_role_wages.csv")
  write.csv(data.frame(Annual_Median = 57530, Year = "2025"), path, row.names = FALSE)
  suppressMessages(log_reference_result(path, "error", "HTTP 503"))
  log <- read.csv("scrape_log.csv")
  expect_equal(log$source, "Reference: BLS OEWS role wages")
  expect_equal(log$status, "error")
  expect_equal(log$n_rows, 1)
  expect_error(reference_rules_for("not_a_reference_table.csv"), "No REFERENCE_TABLE_RULES")
})

test_that("reference_table_status flags failed, missing and stale refreshes, and nothing when healthy", {
  dir <- withr::local_tempdir()
  write.csv(data.frame(County = "A", Avg_Annual_Pay = 50000, Year = "2025"),
            file.path(dir, "qcew_county_school_pay.csv"), row.names = FALSE)
  write.csv(data.frame(Annual_Median = 57530, Year = "2021"), file.path(dir, "oews_role_wages.csv"), row.names = FALSE)
  log <- data.frame(timestamp = c("2026-11-03 09:00:00", "2026-11-03 09:01:00", "2026-10-27 09:00:00"),
                    source = c("Reference: BLS QCEW county school pay", "Reference: BLS QCEW county school pay",
                               "Reference: BLS OEWS role wages"),
                    status = c("error", "ok", "ok"), n_rows = 1, error_message = c("HTTP 503", NA, NA))
  st <- reference_table_status(dir, log, today = as.Date("2026-11-03"))
  # QCEW: the later "ok" row wins. OEWS: last week's log doesn't count, and 2021 is stale.
  expect_equal(st$name, "BLS OEWS role wages")
  expect_match(st$problem, "no refresh logged this run")
  expect_match(st$problem, "newest figure is from 2021")

  log$status[2] <- "rejected"; log$error_message[2] <- "10 rows, down from 56"
  st <- reference_table_status(dir, log, today = as.Date("2026-11-03"))
  expect_match(st$problem[st$name == "BLS QCEW county school pay"], "refresh rejected \\(10 rows, down from 56\\)")

  write.csv(data.frame(Annual_Median = 57530, Year = "2025"), file.path(dir, "oews_role_wages.csv"), row.names = FALSE)
  log <- rbind(log, data.frame(timestamp = "2026-11-03 09:02:00", source = "Reference: BLS OEWS role wages",
                               status = "current", n_rows = 1, error_message = NA))
  log$status[2] <- "ok"
  expect_equal(nrow(reference_table_status(dir, log, today = as.Date("2026-11-03"))), 0)
})
