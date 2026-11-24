# Live dashboard check (2026-11-23). Connect Cloud redeploys a state's app on every push to main, after the weekly
# run's checks have passed, but nothing checked the app it then served: an install that fails on Connect Cloud (the
# edjobs tag, a package) would show only when someone opened the page. These read the live page's "Refreshed on"
# date (home_tab_panel()'s footer, in the HTML the server sends before any websocket) and compare it with main's data.
# A sleeping app first answers with Connect Cloud's "Loading..." page, then serves the app once awake
# (tests/testthat/fixtures/connect_cloud_loading_page.html, live_dashboard_mt_2026-11-23.html).

#' Whether a page is Connect Cloud's "Loading..." page
#'
#' Connect Cloud answers a sleeping app's URL with this page while the app starts, then reloads.
#' @param html The page's HTML, one string.
#' @return TRUE or FALSE.
#' @export
is_connect_loading_page <- function(html) {
  grepl("<title>Posit Connect Cloud</title>", html, fixed = TRUE) && grepl('id="loading"', html, fixed = TRUE)
}

#' The "Refreshed on" date a live dashboard shows
#'
#' Reads the Home tab footer (`home_tab_panel()`): "Refreshed on: November 23, 2026". Month names are read in
#' English whatever the locale.
#' @param html The page's HTML, one string.
#' @return A Date, NA when the page has no such line (the loading page, an error page).
#' @export
live_refreshed_date <- function(html) {
  m <- regmatches(html, regexec("Refreshed on: ([A-Z][a-z]+) ([0-9]{1,2}), ([0-9]{4})", html))[[1]]
  if (length(m) == 0) return(as.Date(NA))
  month <- match(m[2], month.name)
  if (is.na(month)) return(as.Date(NA))
  as.Date(sprintf("%s-%02d-%02d", m[4], month, as.integer(m[3])), optional = TRUE)
}

#' The "Refreshed on" date a dashboard's committed data should show
#'
#' The latest `Archive_Date` in `allsum.csv` and `allsum_he.csv`, as app.R computes `last_refreshed_date` (its
#' category filters drop no whole day in practice).
#' @param app_dir The app folder (`Mt_Ed_Jobs`).
#' @return A Date.
#' @export
expected_refreshed_date <- function(app_dir) {
  dates <- unlist(lapply(c("allsum.csv", "allsum_he.csv"), function(f) {
    utils::read.csv(file.path(app_dir, f), colClasses = "character")$Archive_Date
  }))
  d <- as.Date(dates, optional = TRUE)
  if (all(is.na(d))) stop("No Archive_Date in ", app_dir, "/allsum.csv or allsum_he.csv")
  max(d, na.rm = TRUE)
}

#' Fetch a live dashboard page
#'
#' A sleeping app's first answer can take a minute or more (Connect Cloud holds the connection while it starts).
#' @param url The dashboard URL.
#' @param timeout Seconds to wait for an answer.
#' @return `list(status = <HTTP status>, body = <HTML>)`, or `list(status = NA, body = "", error = <message>)` when
#'   nothing answered.
#' @export
fetch_live_page <- function(url, timeout = 180) {
  tryCatch({
    resp <- httr2::request(url) %>%
      httr2::req_user_agent("edjobs live dashboard check") %>%
      httr2::req_timeout(timeout) %>%
      httr2::req_error(is_error = function(resp) FALSE) %>%
      httr2::req_perform()
    list(status = httr2::resp_status(resp), body = httr2::resp_body_string(resp))
  }, error = function(e) list(status = NA_integer_, body = "", error = conditionMessage(e)))
}

#' Check a live dashboard shows its committed data
#'
#' Fetches `url` until it serves the app with "Refreshed on" equal to `expected`, or `wait_minutes` pass. A redeploy
#' after a push takes a while, so a page that still shows the previous date, or is still loading, is read again.
#' @param url The dashboard URL.
#' @param expected The Date it should show ([expected_refreshed_date()]).
#' @param wait_minutes How long to keep reading before giving up.
#' @param poll_seconds Seconds between reads.
#' @param fetch,sleep Injected for tests ([fetch_live_page()], [Sys.sleep()]).
#' @return A list: `ok`; `status`, one of `"ok"`, `"stale"` (the app shows another date), `"no_app"` (pages came
#'   back but none was the app: still loading, or an error page), `"unreachable"` (no answer, or an HTTP error);
#'   `live_date`; `expected`; `attempts`; `detail` (the last read, in words).
#' @export
check_live_dashboard <- function(url, expected, wait_minutes = 40, poll_seconds = 60,
                                 fetch = fetch_live_page, sleep = Sys.sleep) {
  deadline <- Sys.time() + wait_minutes * 60
  attempts <- 0L
  repeat {
    attempts <- attempts + 1L
    page <- fetch(url)
    live <- as.Date(NA)
    if (is.na(page$status)) {
      status <- "unreachable"
      detail <- paste("No answer:", page$error %||% "unknown error")
    } else if (page$status >= 400) {
      status <- "unreachable"
      detail <- sprintf("HTTP %d", page$status)
    } else if (is_connect_loading_page(page$body)) {
      status <- "no_app"
      detail <- "Connect Cloud's \"Loading...\" page (the app was still starting)"
    } else {
      live <- live_refreshed_date(page$body)
      if (is.na(live)) {
        status <- "no_app"
        detail <- sprintf("HTTP %d, but the page has no \"Refreshed on\" line (not the dashboard)", page$status)
      } else if (live == expected) {
        status <- "ok"
        detail <- sprintf("Shows %s", format(live))
      } else {
        status <- "stale"
        detail <- sprintf("Shows %s, main's data is %s", format(live), format(expected))
      }
    }
    if (status == "ok" || Sys.time() >= deadline) break
    sleep(poll_seconds)
  }
  list(ok = status == "ok", status = status, live_date = live, expected = expected, attempts = attempts,
       detail = detail)
}

#' The issue text for a failed live check
#'
#' @param result [check_live_dashboard()]'s result.
#' @param url The dashboard URL.
#' @param wait_minutes The wait the check used.
#' @param run_url The workflow run's URL, or "".
#' @return Markdown, one string.
#' @export
live_check_report <- function(result, url, wait_minutes, run_url = "") {
  what <- switch(result$status,
    stale = "The live dashboard shows older (or other) data than main.",
    no_app = "The live dashboard URL answered, but not with the dashboard.",
    unreachable = "The live dashboard URL did not answer.",
    "The live dashboard check did not pass.")
  c(
    sprintf("**%s**", what),
    "",
    sprintf("- URL: %s", url),
    sprintf("- Expected \"Refreshed on\": %s (the latest `Archive_Date` in main's `allsum.csv` and `allsum_he.csv`)",
            format(result$expected)),
    sprintf("- Last read: %s", result$detail),
    sprintf("- Read %d time(s) over about %d minutes", result$attempts, wait_minutes),
    if (nzchar(run_url)) sprintf("- Run: %s", run_url),
    "",
    "Where to look: the app's deploy logs on connect.posit.cloud. When main's data is new but the page isn't, the",
    "redeploy may have failed (for example a package install: the edjobs tag in `manifest.json`, a CRAN package).",
    "This issue closes itself once a check passes."
  ) %>% paste(collapse = "\n")
}

#' Run the live check from a workflow step
#'
#' Writes a Markdown line to `summary_path`; when the check fails, writes [live_check_report()] to `report_path`
#' (the workflow files it as an issue).
#' @param url,app_dir,wait_minutes,poll_seconds See [check_live_dashboard()] and [expected_refreshed_date()].
#' @param report_path File the failure report is written to.
#' @param summary_path File to append a Markdown summary to (`$GITHUB_STEP_SUMMARY`), or NULL.
#' @param run_url The workflow run's URL, or "".
#' @return The result of [check_live_dashboard()], invisibly.
#' @export
live_check_step <- function(url, app_dir, wait_minutes = 40, poll_seconds = 60, report_path = "live_check_report.md",
                            summary_path = NULL, run_url = "") {
  expected <- expected_refreshed_date(app_dir)
  result <- check_live_dashboard(url, expected, wait_minutes = wait_minutes, poll_seconds = poll_seconds)
  line <- sprintf("Live dashboard check: **%s**. %s (%d read(s)). %s", result$status, result$detail,
                  result$attempts, url)
  message(line)
  if (!is.null(summary_path) && nzchar(summary_path)) cat(line, "\n", file = summary_path, sep = "", append = TRUE)
  if (!result$ok) writeLines(live_check_report(result, url, wait_minutes, run_url), report_path)
  invisible(result)
}
