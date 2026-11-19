# The auto-fix issue step (run_checks_autofix.R; package review step 6, slice 5). Old-vs-new parity on five
# scenarios in each state, through a fake gh that logs every call: claude-notes/2026-11-19_step6-parity/slice5/.
# Here: a shell stub for gh, for the cap, the recovery close and the "still flagged" comment.

local_fake_gh <- function(issues_json, env = parent.frame()) {
  skip_on_os("windows")
  dir <- withr::local_tempdir(.local_envir = env)
  log <- file.path(dir, "gh.log")
  writeLines(issues_json, file.path(dir, "issues.json"))
  writeLines(c("#!/bin/sh", sprintf("echo \"$*\" >> '%s'", log),
               sprintf("case \"$1 $2\" in \"issue list\") cat '%s' ;; \"issue create\") echo https://github.com/o/r/issues/100 ;; esac",
                       file.path(dir, "issues.json"))),
             file.path(dir, "gh"))
  Sys.chmod(file.path(dir, "gh"), "755")
  withr::local_envvar(PATH = paste(dir, Sys.getenv("PATH"), sep = .Platform$path.sep), GITHUB_REPOSITORY = "o/r",
                      AUTOFIX_ENGINE = "none", COPILOT_TOKEN = "", .local_envir = env)
  log
}

test_that("new issues are capped, a still-flagged source gets a comment, a recovered one is closed", {
  old <- current_state_config(); withr::defer(set_state_config(old))
  set_state_config(state_config("T", "00", "app"))
  log <- local_fake_gh(jsonlite::toJSON(data.frame(
    number = c(7L, 8L), createdAt = "2026-11-10T00:00:00Z",
    body = c("<!-- autofix-source: Old A -->", "<!-- autofix-source: Recovered B -->"))))
  results <- withr::local_tempfile(fileext = ".csv")
  write.csv(data.frame(name = c("Old A", "N1", "N2", "N3"), type = "K-12", mean_count = 5, count = 0,
                       url = NA, verdict = "likely_broken", error_message = NA, llm_note = NA, llm_titles = "",
                       page_text = NA), results, row.names = FALSE)
  out <- capture.output(file_autofix_issues_step(results, max_new_issues = 2, today = as.Date("2026-11-19")))
  expect_true(any(grepl("Commented on #7 (Old A)", out, fixed = TRUE)))
  expect_equal(sum(grepl("^Created #100 for N[12]$", out)), 2)
  expect_true(any(grepl("Cap of 2 new auto-fix issues reached; skipping N3", out, fixed = TRUE)))
  expect_true(any(grepl("Closed #8 (Recovered B: not_flagged)", out, fixed = TRUE)))
  calls <- readLines(log)
  expect_true(any(grepl("^issue close 8 --comment Closing: Recovered B is no longer flagged", calls)))
  expect_false(any(grepl("assignee|workflow run", calls)))   # engine none: filed, not handed off
})

test_that("no results file closes every open issue", {
  old <- current_state_config(); withr::defer(set_state_config(old))
  set_state_config(state_config("T", "00", "app"))
  log <- local_fake_gh(jsonlite::toJSON(data.frame(number = 9L, createdAt = "2026-11-10T00:00:00Z",
                                                   body = "<!-- autofix-source: C -->")))
  out <- capture.output(file_autofix_issues_step(file.path(tempdir(), "no-such-results.csv")))
  expect_true(any(grepl("Closed #9 (C: not_flagged)", out, fixed = TRUE)))
})

test_that("state_config validates canonical_source_name and drift_intro", {
  expect_error(state_config("T", "00", "app", canonical_source_name = "x"), "canonical_source_name")
  expect_error(state_config("T", "00", "app", drift_intro = c("a", "b")), "drift_intro")
})

test_that("corroboration: scrape errors, missing URLs and rendered pages get their verdicts; names go through the state's canonicalizer", {
  old <- current_state_config(); withr::defer(set_state_config(old))
  root <- withr::local_tempdir(); withr::local_dir(root); dir.create("app")
  set_state_config(state_config("T", "00", "app",
    feed = list(short_name = "FEED", label = "Statewide feed", url = "https://feed.example", discrepancy_file = "d.csv"),
    scraper_sources = list(scraper_calls(list(
      "Alpha Schools Raw" = list(fn = "f", args = list(), url = "https://alpha.example"),
      "Beta Schools" = list(fn = "f", args = list(), url = "https://beta.example")), where = "a test")),
    canonical_source_name = function(x) sub(" Raw$", "", x), drift_intro = "Custom intro."))
  write.csv(data.frame(District = "Alpha Schools", title = "Math Teacher", Posting_Source = "board"),
            file.path("app", "combinedclean.csv"), row.names = FALSE)
  write.csv(data.frame(name = c("Alpha Schools", "Beta Schools", "Gamma Schools"), mean_count = c(6, 4, 3),
                       n_weeks = 4, count = 0, type = "K-12", scrape_error = c(NA, "HTTP 500", NA),
                       safety_net_vacancies = 0), "flagged.csv", row.names = FALSE)
  rendered <- character(0)
  render <- function(url) { rendered <<- c(rendered, url); "" }
  withr::local_envvar(GEMINI_API_KEY = "")
  out <- capture.output(res <- corroborate_drift_step("flagged.csv", "results.csv", "report.md", render = render,
                                                      today = as.Date("2026-11-19")))
  expect_equal(rendered, "https://alpha.example")   # canonicalized name found its URL; the errored one isn't rendered
  expect_equal(res$verdict, c("looks_genuinely_empty", "confirmed_broken", "no_url_available"))
  expect_equal(read.csv("results.csv")$name, res$name)
  report <- readLines("report.md")
  expect_true(any(grepl("Custom intro.", report, fixed = TRUE)))
})

test_that("corroboration: nothing flagged does nothing", {
  root <- withr::local_tempdir(); withr::local_dir(root)
  write.csv(data.frame(name = character(0)), "flagged.csv", row.names = FALSE)
  expect_output(res <- corroborate_drift_step("flagged.csv", "results.csv", "report.md"), "Nothing flagged")
  expect_null(res)
  expect_false(file.exists("results.csv"))
})
