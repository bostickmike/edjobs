# The salary-source check (run_checks_salary.R; package review step 6, slice 7). Coverage is compared with the last
# committed file instead of typed-in counts. Old-vs-new output of the value and reference-table parts on both states'
# committed data is in claude-notes/2026-11-19_step6-parity/slice7/.

salary_files <- function(k12, he) {
  root <- withr::local_tempdir(.local_envir = parent.frame()); withr::local_dir(root, .local_envir = parent.frame())
  dir.create("app")
  write.csv(k12, file.path("app", "salarymap2.csv"), row.names = FALSE)
  write.csv(he, file.path("app", "salarymap.csv"), row.names = FALSE)
  file.path(root, "report.md")
}
k12_pay <- function() data.frame(District = c("A", "B", "C", "D"), Teacher_Base_Salary = c(50000, 52000, NA, 48000),
                                 Teacher_Base_Salary_Prior_Year = c(49000, 51000, NA, 47000))
he_pay <- function() data.frame(Name = c("U", "V"), Faculty_Avg_Salary = c(70000, 65000), Enrollment = c(900, 1200))
pay_checks <- function(extra = NULL) salary_checks(
  coverage = data.frame(name = c("K-12 base", "HE salary", "HE enrollment"),
                        file = c("salarymap2.csv", "salarymap.csv", "salarymap.csv"),
                        column = c("Teacher_Base_Salary", "Faculty_Avg_Salary", "Enrollment")),
  bounds = data.frame(name = "K-12 base", file = "salarymap2.csv", column = "Teacher_Base_Salary",
                      min_ok = 25000, max_ok = 150000),
  yoy = data.frame(file = "salarymap2.csv", current = "Teacher_Base_Salary", prior = "Teacher_Base_Salary_Prior_Year"),
  extra = extra)
use_config <- function(checks, envir = parent.frame()) {
  old <- current_state_config(); withr::defer(set_state_config(old), envir = envir)
  set_state_config(state_config("T", "00", "app", salary_checks = checks))
}
baseline <- function(k12, he) function(f) switch(f, salarymap2.csv = k12, salarymap.csv = he)

test_that("salary_checks and state_config validate their input", {
  expect_error(salary_checks(data.frame(name = "x", file = "salarymap2.csv")), "coverage needs columns")
  expect_error(salary_checks(data.frame(name = "x", file = "other.csv", column = "y")), "unknown file")
  expect_error(salary_checks(data.frame(name = "x", file = "salarymap.csv", column = "y"), extra = 1), "extra")
  expect_error(state_config("T", "00", "app", salary_checks = list()), "salary_checks")
  expect_null(state_config("T", "00", "app")$salary_checks)
  old <- current_state_config(); withr::defer(set_state_config(old)); set_state_config(NULL)
  expect_error(salary_drift_step(app_dir = "app"), "No salary checks configured")
})

test_that("unchanged or grown coverage is healthy, however many rows lack a value", {
  report <- salary_files(k12_pay(), he_pay())
  use_config(pay_checks())
  fewer <- k12_pay(); fewer$Teacher_Base_Salary[2] <- NA   # last run had less: growth is fine
  expect_output(drops <- salary_drift_step(report_path = report, previous = baseline(fewer, he_pay())), "look healthy")
  expect_equal(nrow(drops), 0)
  expect_false(file.exists(report))
})

test_that("a drop against last run is flagged with the districts that lost data, a removed row included", {
  now <- k12_pay()[-4, ]; now$Teacher_Base_Salary[1] <- NA   # A lost its value, D's row is gone, C never had one
  report <- salary_files(now, he_pay())
  writeLines("earlier section", report)
  use_config(pay_checks())
  expect_output(drops <- salary_drift_step(report_path = report, previous = baseline(k12_pay(), he_pay())),
                "Flagged items written")
  expect_equal(drops$name, "K-12 base")
  expect_equal(c(drops$previous, drops$actual), c(3, 1))
  expect_equal(drops$lost, "A; D")
  txt <- readLines(report)
  expect_equal(txt[1], "earlier section")
  expect_true(any(grepl("**K-12 base**: 3 had a value last run, 1 now", txt, fixed = TRUE)))
  expect_true(any(grepl("Lost: A; D", txt, fixed = TRUE)))
})

test_that("a file with no committed copy is said so, not silently passed", {
  report <- salary_files(k12_pay(), he_pay())
  use_config(pay_checks())
  expect_output(drops <- salary_drift_step(report_path = report, previous = baseline(k12_pay(), NULL)))
  expect_equal(attr(drops, "missing_baseline"), "salarymap.csv")
  expect_true(any(grepl("No committed copy of `salarymap.csv`", readLines(report), fixed = TRUE)))
})

test_that("values out of range, year-over-year outliers and the state's extra lines are reported", {
  k12 <- data.frame(District = LETTERS[1:6], Teacher_Base_Salary = c(50000, 51000, 52000, 53000, 90000, 999),
                    Teacher_Base_Salary_Prior_Year = c(49000, 50000, 51000, 52000, 50000, 48000))
  report <- salary_files(k12, he_pay())
  use_config(pay_checks(extra = function() c("Split watch: something new", "")))
  expect_output(salary_drift_step(report_path = report, previous = baseline(k12, he_pay())))
  txt <- readLines(report)
  expect_true(any(grepl("**K-12 base** (F): $999 is outside the expected $25,000-$150,000 range", txt, fixed = TRUE)))
  expect_true(any(grepl("**E**: $50,000 -> $90,000 (+80.0%)", txt, fixed = TRUE)))
  expect_true(any(grepl("Split watch: something new", txt, fixed = TRUE)))
})

test_that("the committed-file baseline reads git's HEAD copy, or the folder EDJOBS_SALARY_BASELINE names", {
  skip_if(Sys.which("git") == "")
  root <- withr::local_tempdir(); withr::local_dir(root)
  dir.create("app")
  write.csv(k12_pay(), "app/salarymap2.csv", row.names = FALSE)
  system2("git", c("init", "-q")); system2("git", c("add", "."))
  system2("git", c("-c", "user.name=t", "-c", "user.email=t@t", "commit", "-q", "-m", "x"))
  write.csv(k12_pay()[1, ], "app/salarymap2.csv", row.names = FALSE)   # the run's refreshed file
  withr::local_envvar(EDJOBS_SALARY_BASELINE = "")
  prev <- committed_salary_file("app")
  expect_equal(nrow(prev("salarymap2.csv")), 4)
  expect_null(prev("salarymap.csv"))

  dir.create("snap"); write.csv(k12_pay()[1:2, ], "snap/salarymap2.csv", row.names = FALSE)
  withr::local_envvar(EDJOBS_SALARY_BASELINE = file.path(root, "snap"))
  expect_equal(nrow(prev("salarymap2.csv")), 2)
  expect_null(prev("salarymap.csv"))
})
