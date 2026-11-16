# K-12 salary history (moved from both states' test-salary-scrapers.R, 2026-11-16), with each state's columns.

MT_SALARY_COLUMNS <- c("Teacher_Count", "Teacher_Salary_10th_Pctile", "Teacher_Avg_Salary", "Teacher_Salary_90th_Pctile")
WY_SALARY_COLUMNS <- c("Teacher_Base_Salary", "Superintendent_Salary")

mt_salarymap2 <- function(year) data.frame(
  District = c("Billings Public Schools", "Missoula County Public Schools"),
  Salary_Year = year,
  Teacher_Count = c(852, 542),
  Teacher_Salary_10th_Pctile = c(45400, 42000),
  Teacher_Avg_Salary = c(66100, 62800),
  Teacher_Salary_90th_Pctile = c(82600, 83000),
  Extra_Column = "not archived",
  stringsAsFactors = FALSE
)
wy_salarymap2 <- function(year) data.frame(
  District = c("Albany County School District 1", "Big Horn School District 1"),
  Salary_Year = year,
  Teacher_Base_Salary = c(51500, 56750),
  Superintendent_Salary = c(174000, 152140),
  stringsAsFactors = FALSE
)

test_that("archive_k12_salary_snapshot creates the archive on first run and appends on a new year (MT columns)", {
  history_path <- withr::local_tempfile(fileext = ".csv")
  y1 <- mt_salarymap2("2022-23")

  expect_true(suppressMessages(archive_k12_salary_snapshot(y1, history_path, MT_SALARY_COLUMNS)))
  archived <- read.csv(history_path, stringsAsFactors = FALSE)
  expect_equal(nrow(archived), 2)
  expect_equal(names(archived), c("District", "Salary_Year", MT_SALARY_COLUMNS))
  expect_equal(unique(archived$Salary_Year), "2022-23")

  # Same year again (next week's run): no-op, no duplicate rows.
  expect_false(suppressMessages(archive_k12_salary_snapshot(y1, history_path, MT_SALARY_COLUMNS)))
  expect_equal(nrow(read.csv(history_path, stringsAsFactors = FALSE)), 2)

  # A new year: appended, the old year's rows kept.
  y2 <- mt_salarymap2("2023-24")
  y2$Teacher_Avg_Salary <- c(68000, 64500)
  expect_true(suppressMessages(archive_k12_salary_snapshot(y2, history_path, MT_SALARY_COLUMNS)))
  archived_final <- read.csv(history_path, stringsAsFactors = FALSE)
  expect_equal(nrow(archived_final), 4)
  expect_setequal(unique(archived_final$Salary_Year), c("2022-23", "2023-24"))
})

test_that("archive_k12_salary_snapshot creates the archive on first run and appends on a new year (WY columns)", {
  history_path <- withr::local_tempfile(fileext = ".csv")
  y1 <- wy_salarymap2("2025-2026")
  expect_true(suppressMessages(archive_k12_salary_snapshot(y1, history_path, WY_SALARY_COLUMNS)))
  expect_false(suppressMessages(archive_k12_salary_snapshot(y1, history_path, WY_SALARY_COLUMNS)))
  y2 <- wy_salarymap2("2026-2027")
  y2$Teacher_Base_Salary <- c(53000, 58500)
  expect_true(suppressMessages(archive_k12_salary_snapshot(y2, history_path, WY_SALARY_COLUMNS)))
  archived <- read.csv(history_path, stringsAsFactors = FALSE)
  expect_equal(names(archived), c("District", "Salary_Year", WY_SALARY_COLUMNS))
  expect_equal(nrow(archived), 4)
  expect_setequal(unique(archived$Salary_Year), c("2025-2026", "2026-2027"))
})

test_that("archive_k12_salary_snapshot skips archiving when Salary_Year isn't a single consistent value", {
  history_path <- withr::local_tempfile(fileext = ".csv")
  inconsistent <- wy_salarymap2(c("2025-2026", "2026-2027"))
  expect_false(suppressMessages(archive_k12_salary_snapshot(inconsistent, history_path, WY_SALARY_COLUMNS)))
  expect_false(file.exists(history_path))
})

test_that("archive_k12_salary_snapshot backfills a partial existing district/year set idempotently", {
  history_path <- withr::local_tempfile(fileext = ".csv")
  salarymap2 <- mt_salarymap2("2022-23")
  write.csv(salarymap2[1, c("District", "Salary_Year", MT_SALARY_COLUMNS)], history_path, row.names = FALSE)

  expect_true(suppressMessages(archive_k12_salary_snapshot(salarymap2, history_path, MT_SALARY_COLUMNS)))
  archived <- read.csv(history_path, stringsAsFactors = FALSE)
  expect_setequal(archived$District, salarymap2$District)
  expect_equal(nrow(archived), 2)

  expect_false(suppressMessages(archive_k12_salary_snapshot(salarymap2, history_path, MT_SALARY_COLUMNS)))
  expect_equal(nrow(read.csv(history_path, stringsAsFactors = FALSE)), 2)
})

test_that("missing_k12_salary_snapshot_rows keeps one row per new district/year pair", {
  existing <- data.frame(District = "A", Salary_Year = "2025-2026", stringsAsFactors = FALSE)
  snapshot <- data.frame(District = c("A", "B", "B"), Salary_Year = "2025-2026", Pay = c(1, 2, 3),
                         stringsAsFactors = FALSE)
  out <- missing_k12_salary_snapshot_rows(existing, snapshot)
  expect_equal(out$District, "B")
  expect_equal(out$Pay, 2)
})

test_that("standardize_date reads the posted-date formats the scrapers return", {
  expect_equal(standardize_date(c("Oct 23 2025 6:00 AM", "October 23 2025 6:00 AM (UTC)", "2025-10-23",
                                  "10/23/2025", "2025-10-23 06:00:00")),
               rep(as.Date("2025-10-23"), 5))
  expect_equal(standardize_date(factor("2025-10-23")), as.Date("2025-10-23"))
  expect_true(is.na(standardize_date("Open until filled")))
  expect_true(is.na(standardize_date(NA)))
})
