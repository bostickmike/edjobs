test_that("latest_run_per_week keeps each week's latest update, then the last n weeks", {
  # MT runs Monday and Thursday: one point per week, the Thursday.
  d <- data.frame(Archive_Date = as.Date(c("2026-11-02", "2026-11-05", "2026-11-09", "2026-11-12", "2026-11-16",
                                           "2026-11-19", "2026-11-23")), n = 1:7)
  out <- latest_run_per_week(d, 12)
  expect_equal(as.character(out$Archive_Date), c("2026-11-05", "2026-11-12", "2026-11-19", "2026-11-23"))
  expect_equal(out$n, c(2L, 4L, 6L, 7L))
  expect_equal(nrow(latest_run_per_week(d, 2)), 2)
  # weekly history: unchanged; text dates and unsorted input work; ISO weeks cross the new year
  w <- data.frame(Archive_Date = c("2026-12-28", "2026-12-21", "2027-01-04"), n = c(2, 1, 3))
  expect_equal(latest_run_per_week(w)$n, c(1, 2, 3))
  expect_equal(nrow(latest_run_per_week(d[0, ])), 0)
  # 24 twice-weekly updates span 12 weeks, so 12 points
  tw <- data.frame(Archive_Date = sort(c(as.Date("2026-09-07") + 7 * 0:11, as.Date("2026-09-10") + 7 * 0:11)), n = 1:24)
  expect_equal(nrow(latest_run_per_week(tw, 12)), 12)
})
