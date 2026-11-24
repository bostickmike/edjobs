# Live dashboard check (R/live_check.R). Fixtures are the live MT URL's two answers on 2026-11-23: Connect Cloud's
# "Loading..." page (the app asleep) and the app page once awake (trimmed; its footer line as served).
read_page <- function(name) paste(readLines(test_path("fixtures", name), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
loading <- read_page("connect_cloud_loading_page.html")
app_page <- read_page("live_dashboard_mt_2026-11-23.html")

test_that("the loading page is told apart from the app", {
  expect_true(is_connect_loading_page(loading))
  expect_false(is_connect_loading_page(app_page))
  expect_true(is.na(live_refreshed_date(loading)))
})

test_that("the footer's Refreshed on date is read, a zero-padded day too", {
  expect_equal(live_refreshed_date(app_page), as.Date("2026-11-23"))
  # app.R formats with %d: early-month days are zero-padded.
  expect_equal(live_refreshed_date("<div>Refreshed on: November 03, 2026 · Created by</div>"), as.Date("2026-11-03"))
  expect_true(is.na(live_refreshed_date("<html><title>Error</title></html>")))
  expect_true(is.na(live_refreshed_date("Refreshed on: Novembre 03, 2026")))
})

test_that("the expected date is the latest Archive_Date in allsum.csv and allsum_he.csv", {
  dir <- withr::local_tempdir()
  write.csv(data.frame(Broad_Category = "Art", Archive_Date = c("2026-11-17", "2026-11-20"), District = "Total", sum = 1),
            file.path(dir, "allsum.csv"), row.names = FALSE)
  write.csv(data.frame(Category = "Nursing", Archive_Date = c("2026-11-23", "2026-11-17"), Institution = "Total",
                       Job_Type = "Faculty", sum = 1), file.path(dir, "allsum_he.csv"), row.names = FALSE)
  expect_equal(expected_refreshed_date(dir), as.Date("2026-11-23"))
})

# A fake fetch that answers with each page in turn (the last one again once they run out).
pages_in_turn <- function(...) {
  pages <- list(...); i <- 0
  function(url) { i <<- min(i + 1, length(pages)); pages[[i]] }
}
no_sleep <- function(s) invisible(NULL)

test_that("a live page showing main's date passes on the first read", {
  r <- check_live_dashboard("u", as.Date("2026-11-23"), fetch = pages_in_turn(list(status = 200L, body = app_page)),
                            sleep = no_sleep)
  expect_true(r$ok)
  expect_identical(r$status, "ok")
  expect_identical(r$attempts, 1L)
})

test_that("a waking app is read again until it serves the dashboard", {
  r <- check_live_dashboard("u", as.Date("2026-11-23"), wait_minutes = 1,
                            fetch = pages_in_turn(list(status = 200L, body = loading), list(status = 200L, body = app_page)),
                            sleep = no_sleep)
  expect_true(r$ok)
  expect_identical(r$attempts, 2L)
})

test_that("each failure is named once the wait is over", {
  expired <- function(page, expected = as.Date("2026-11-23")) {
    check_live_dashboard("u", expected, wait_minutes = 0, fetch = function(url) page, sleep = no_sleep)
  }
  stale <- expired(list(status = 200L, body = app_page), as.Date("2026-11-26"))
  expect_identical(stale$status, "stale")
  expect_equal(stale$live_date, as.Date("2026-11-23"))
  expect_match(stale$detail, "Shows 2026-11-23, main's data is 2026-11-26")
  expect_identical(expired(list(status = 200L, body = loading))$status, "no_app")
  expect_identical(expired(list(status = 200L, body = "<html>An error has occurred</html>"))$status, "no_app")
  expect_identical(expired(list(status = 502L, body = ""))$status, "unreachable")
  down <- expired(list(status = NA_integer_, body = "", error = "Could not resolve host"))
  expect_identical(down$status, "unreachable")
  expect_match(down$detail, "Could not resolve host")
})

test_that("a stale page is read again within the wait (a redeploy in progress)", {
  r <- check_live_dashboard("u", as.Date("2026-11-26"), wait_minutes = 0.0005, poll_seconds = 0,
                            fetch = function(url) list(status = 200L, body = app_page), sleep = function(s) Sys.sleep(0.01))
  expect_false(r$ok)
  expect_gt(r$attempts, 1L)
})

test_that("the step writes a summary line, and a report only when the check fails", {
  dir <- withr::local_tempdir()
  write.csv(data.frame(Archive_Date = "2026-11-23"), file.path(dir, "allsum.csv"), row.names = FALSE)
  write.csv(data.frame(Archive_Date = "2026-11-20"), file.path(dir, "allsum_he.csv"), row.names = FALSE)
  report <- file.path(dir, "report.md"); summary <- file.path(dir, "summary.md")
  local_mocked_bindings(fetch_live_page = function(url, timeout = 180) list(status = 200L, body = app_page))
  r <- suppressMessages(live_check_step("https://x", dir, wait_minutes = 0, report_path = report, summary_path = summary))
  expect_true(r$ok)
  expect_false(file.exists(report))
  expect_match(readLines(summary), "Live dashboard check: \\*\\*ok\\*\\*")

  write.csv(data.frame(Archive_Date = "2026-11-26"), file.path(dir, "allsum.csv"), row.names = FALSE)
  r <- suppressMessages(live_check_step("https://x", dir, wait_minutes = 0, report_path = report, summary_path = summary,
                                        run_url = "https://run"))
  expect_false(r$ok)
  txt <- paste(readLines(report), collapse = "\n")
  expect_match(txt, "older \\(or other\\) data than main")
  expect_match(txt, "Expected \"Refreshed on\": 2026-11-26")
  expect_match(txt, "Run: https://run")
})
