# The "Last 12 Weeks" sparklines: the latest update in each ISO week (latest_snapshot_per_week()), then the last 12.
weekly_points <- function(df, n = 12) utils::tail(latest_snapshot_per_week(df[order(as.Date(df$Archive_Date)), , drop = FALSE]), n)

test_that("sparklines keep each week's latest update, then the last 12 weeks", {
  # MT updates Monday and Thursday: one point per week, the Thursday.
  d <- data.frame(Archive_Date = as.Date(c("2026-11-02", "2026-11-05", "2026-11-09", "2026-11-12", "2026-11-16",
                                           "2026-11-19", "2026-11-23")), n = 1:7)
  out <- weekly_points(d)
  expect_equal(as.character(out$Archive_Date), c("2026-11-05", "2026-11-12", "2026-11-19", "2026-11-23"))
  expect_equal(nrow(weekly_points(d, 2)), 2)
  # weekly history unchanged; text dates and unsorted input; ISO weeks cross the new year
  w <- data.frame(Archive_Date = c("2026-12-28", "2026-12-21", "2027-01-04"), n = c(2, 1, 3))
  expect_equal(weekly_points(w)$n, c(1, 2, 3))
  # 24 twice-weekly updates span 12 weeks, so 12 points
  tw <- data.frame(Archive_Date = sort(c(as.Date("2026-09-07") + 7 * 0:11, as.Date("2026-09-10") + 7 * 0:11)), n = 1:24)
  expect_equal(nrow(weekly_points(tw)), 12)
})
