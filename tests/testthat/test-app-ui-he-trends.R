# Higher Ed Trends tab UI (step 7 slice 2b): he_trends_panel().
skip_if_not_installed("shiny"); skip_if_not_installed("bslib"); skip_if_not_installed("shinyWidgets")
skip_if_not_installed("plotly"); skip_if_not_installed("shinycssloaders")

test_that("he_trends_panel builds both sub-tabs from the summaries, with the slider's window passed in", {
  d <- as.Date("2026-01-05") + 7 * 0:60
  hesum <- data.frame(Institution = rep(c("Total", "Col A"), length.out = 61), Category = "Math", Archive_Date = d)
  now <- data.frame(Institution = c("Total", "Col B"))
  h <- as.character(he_trends_panel(now, hesum))
  expect_match(h, "data-value=\"he_trends\"", fixed = TRUE)
  expect_match(h, "data-value=\"he_trends_historical\"", fixed = TRUE)
  expect_match(h, "Col B", fixed = TRUE)   # Current Trends choices come from henowsum
  expect_match(h, "id=\"he_longitudinal_plot\"", fixed = TRUE)
  # Default window: the last 365 days (WY); a state can pass its own (MT bounds it to WINDOW_WEEKS).
  from <- function(x) as.numeric(sub(".*data-from=\"([0-9]+)\".*", "\\1", x))
  expect_equal(from(h), as.numeric(as.POSIXct(max(d) - 365, tz = "UTC")) * 1000)
  h2 <- as.character(he_trends_panel(now, hesum, c(max(d) - 70, max(d))))
  expect_equal(from(h2), as.numeric(as.POSIXct(max(d) - 70, tz = "UTC")) * 1000)
})
