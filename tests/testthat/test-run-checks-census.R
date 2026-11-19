# The Census check and the auto-fix PR live check (run_checks_census.R; package review step 6, slice 4). Old-vs-new
# output on both states' committed data, and on a perturbed copy, is in
# claude-notes/2026-11-19_step6-parity/slice4/parity4.sh.

census_files <- function(k12, he) {
  root <- withr::local_tempdir(.local_envir = parent.frame()); withr::local_dir(root, .local_envir = parent.frame())
  dir.create("app")
  write.csv(k12, file.path("app", "salarymap2.csv"), row.names = FALSE)
  write.csv(he, file.path("app", "salarymap.csv"), row.names = FALSE)
  file.path(root, "report.md")
}

k12_rows <- function() data.frame(
  District = c("A", "B", "C", "BOCES"), County = c("X", "X", "Y", "Y"),
  Median_Household_Income = 60000, Median_Gross_Rent = 900, Mining_Employment_Share = 0.1,
  Population_Change_Pct = 0.02, Child_Poverty_Rate = c(0.1, 0.2, 0.15, NA))
he_rows <- function() data.frame(Name = c("U", "V"), County = c("X", "Z"), Median_Household_Income = 60000,
                                 Median_Gross_Rent = 900, Mining_Employment_Share = 0.1, Population_Change_Pct = 0.02)

test_that("state_config validates saipe_gaps", {
  expect_error(state_config("T", "00", "app", saipe_gaps = 1), "saipe_gaps")
  expect_equal(state_config("T", "00", "app")$saipe_gaps, character(0))
})

test_that("the Census check expects every county in the files, and SAIPE for every district but the state's gaps", {
  old <- current_state_config(); withr::defer(set_state_config(old))
  report <- census_files(k12_rows(), he_rows())
  set_state_config(state_config("T", "00", "app", saipe_gaps = "BOCES"))
  expect_output(flags <- check_census_step(report_path = report), "look healthy")
  expect_null(flags)
  expect_false(file.exists(report))

  set_state_config(state_config("T", "00", "app"))  # the BOCES gap not declared
  expect_output(flags <- check_census_step(report_path = report), "Flagged items written")
  expect_equal(flags$name, "K-12 district child poverty (Census SAIPE)")
  expect_equal(c(flags$expected, flags$actual), c(4, 3))
  expect_true(any(grepl("No SAIPE rate: BOCES", readLines(report))))
})

test_that("a county losing its ACS figures and an implausible value are flagged; flags append to the report", {
  old <- current_state_config(); withr::defer(set_state_config(old))
  k12 <- k12_rows(); k12$Median_Household_Income[k12$County == "Y"] <- NA; k12$Median_Gross_Rent[1] <- 9999
  report <- census_files(k12, he_rows())
  writeLines("earlier section", report)
  set_state_config(state_config("T", "00", "app", saipe_gaps = "BOCES"))
  expect_output(flags <- check_census_step(report_path = report))
  expect_equal(flags$name, "K-12 county context (Census ACS)")
  expect_equal(c(flags$expected, flags$actual), c(2, 1))
  txt <- readLines(report)
  expect_equal(txt[1], "earlier section")
  expect_true(any(grepl("expected >= 2, got 1", txt, fixed = TRUE)))
  expect_true(any(grepl("**K-12 county median rent (Census ACS)** (A): 9,999", txt, fixed = TRUE)))
})

test_that("the live check: not applicable without a linked auto-fix issue", {
  out <- withr::local_tempfile()
  gh <- function(args) if (args[1] == "pr") list(closingIssuesReferences = list(number = integer(0)), body = "no link")
  expect_output(ok <- live_check_autofix_step("o/r", "1", out_path = out, gh = gh), "not applicable")
  expect_true(ok)
  expect_false(file.exists(out))
})

test_that("the live check runs the linked issue's scraper and reports; a source with no call says so", {
  old <- current_state_config(); withr::defer(set_state_config(old))
  set_state_config(state_config("T", "00", "app", scraper_sources = list(scraper_calls(
    list("Alpha Schools" = list(fn = "stub_live_fetch", args = list("https://alpha.example"))), where = "a test"))))
  env <- new.env()
  env$stub_live_fetch <- function(url) data.frame(Title = c("Math Teacher", "Bus Driver"))
  gh_for <- function(source) function(args) {
    if (args[1] == "pr") return(list(closingIssuesReferences = list(number = integer(0)), body = "Fixes the scraper (see #7)"))
    list(body = sprintf("<!-- autofix-source: %s -->\nbody", source))
  }
  out <- withr::local_tempfile()
  expect_output(ok <- live_check_autofix_step("o/r", "1", out_path = out, gh = gh_for("Alpha Schools"), envir = env),
                "2 posting")
  expect_true(ok)
  expect_true(any(grepl("Math Teacher", readLines(out))))

  env$stub_live_fetch <- function(url) data.frame(Title = character(0))
  expect_output(ok <- live_check_autofix_step("o/r", "1", out_path = out, gh = gh_for("Alpha Schools"), envir = env))
  expect_false(ok)

  expect_true(live_check_autofix_step("o/r", "1", out_path = out, gh = gh_for("Omega Schools"), envir = env))
  expect_true(any(grepl("No single-source live check", readLines(out))))
})
