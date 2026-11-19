# The weekly run's checks (run_checks.R; package review step 6, slice 1). Each replaces a state script; the expected
# text below is what those scripts printed, so the run logs read the same.

local_app <- function(k12_old, k12_new, he_old, he_new, env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  app <- file.path(root, "app"); dir.create(app)
  write.csv(data.frame(x = seq_len(k12_old)), file.path(root, "baseline_combinedclean.csv"), row.names = FALSE)
  write.csv(data.frame(x = seq_len(k12_new)), file.path(app, "combinedclean.csv"), row.names = FALSE)
  writexl::write_xlsx(data.frame(x = seq_len(he_old)), file.path(root, "baseline_hedata.xlsx"))
  writexl::write_xlsx(data.frame(x = seq_len(he_new)), file.path(app, "hedata.xlsx"))
  list(root = root, app = app)
}

test_that("row counts: a normal week passes and prints both counts", {
  d <- local_app(100, 90, 20, 21)
  expect_output(n <- check_run_row_counts(d$app, baseline_dir = d$root),
                "K-12 rows: 100 -> 90\nHE rows:   20 -> 21\nSanity check passed.", fixed = TRUE)
  expect_equal(unname(n), c(100, 90, 20, 21))
})

test_that("row counts: losing more than half of either side stops the run", {
  d <- local_app(100, 49, 20, 20)
  expect_error(capture.output(check_run_row_counts(d$app, baseline_dir = d$root)),
               "^K-12 row count dropped from 100 to 49 \\(more than 50%\\)")
  d <- local_app(100, 100, 20, 9)
  expect_error(capture.output(check_run_row_counts(d$app, baseline_dir = d$root)),
               "^HE row count dropped from 20 to 9 \\(more than 50%\\).*Refusing to commit\\.$")
  d <- local_app(100, 50, 20, 10)   # exactly half: still passes, as the script did
  expect_output(check_run_row_counts(d$app, baseline_dir = d$root), "Sanity check passed.", fixed = TRUE)
})

test_that("the app folder comes from the state config when not given", {
  d <- local_app(10, 10, 5, 5)
  old <- current_state_config()
  set_state_config(state_config("Test", "00", d$app))
  withr::defer(set_state_config(old))
  expect_output(check_run_row_counts(baseline_dir = d$root), "Sanity check passed.", fixed = TRUE)
})

test_that("schemas: pass, a missing column and a missing file", {
  d <- local_app(1, 1, 1, 1)
  write.csv(data.frame(District = "A", Title = "T"), file.path(d$app, "a.csv"), row.names = FALSE)
  writexl::write_xlsx(data.frame(Institution = "U"), file.path(d$app, "b.xlsx"))
  expect_output(check_app_schemas(list(a.csv = c("District", "Title")), list(b.xlsx = "Institution"), app_dir = d$app),
                "Schema check passed -- every shipped dataset has all the columns app.R expects.", fixed = TRUE)
  out <- capture.output(err <- tryCatch(
    check_app_schemas(list(a.csv = c("District", "Pay_Min"), gone.csv = "x"), list(b.xlsx = "Institution"), app_dir = d$app),
    error = conditionMessage))
  expect_match(err, "^Schema check failed: 2 missing column\\(s\\) across 2 file\\(s\\)")
  expect_true(any(grepl("Pay_Min", out)) && any(grepl("<entire file missing>", out, fixed = TRUE)))
  expect_equal(out[1], "Schema check FAILED:")
})

test_that("expectations step: no file prints a line and does nothing else", {
  dir <- withr::local_tempdir(); withr::local_dir(dir)
  expect_output(r <- run_expectations_step(out_csv = file.path(dir, "out.csv"), report_path = file.path(dir, "report.md")),
                "No run_expectations.csv, nothing to check.", fixed = TRUE)
  expect_null(r)
  expect_false(file.exists(file.path(dir, "out.csv")))
})

test_that("expectations step: all-pass writes the table but no report; a failure starts the report", {
  dir <- withr::local_tempdir(); withr::local_dir(dir)
  withr::local_envvar(GITHUB_STEP_SUMMARY = "")
  write.csv(data.frame(District = c("A", "B")), "data.csv", row.names = FALSE)
  row <- function(min) data.frame(id = "rows", applies_from = "2026-11-01", expires = "2026-12-31", file = "data.csv",
                                  filter = "", measure = "", min = min, max = "", note = "")
  write.csv(row("2"), "run_expectations.csv", row.names = FALSE)
  out <- capture.output(res <- run_expectations_step(run_date = as.Date("2026-11-13"), out_csv = "out.csv", report_path = "report.md"))
  expect_equal(res$Status, "pass")
  expect_match(out[1], "^pass +rows +2  \\(expected")
  expect_true(file.exists("out.csv")); expect_false(file.exists("report.md"))

  write.csv(row("3"), "run_expectations.csv", row.names = FALSE)
  capture.output(res <- run_expectations_step(run_date = as.Date("2026-11-13"), out_csv = "out.csv", report_path = "report.md"))
  expect_equal(res$Status, "fail")
  expect_true(file.exists("report.md"))
  first <- readLines("report.md")
  # An existing report (another check's) gets this section appended after a rule.
  capture.output(run_expectations_step(run_date = as.Date("2026-11-13"), out_csv = "out.csv", report_path = "report.md"))
  expect_equal(readLines("report.md"), c(first, "", "---", "", first))
})
