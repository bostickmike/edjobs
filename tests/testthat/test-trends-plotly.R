# The Trends tabs' multi-year plots (0.93.0): legend under the axis, anchored left, titled "Category" above its items.
test_that("trends_plotly puts a titled, left-anchored legend below the plot, 500px tall", {
  df <- data.frame(d = as.Date("2026-11-01") + 0:1, n = 1:2, Broad_Category = "Library Media")
  p <- ggplot2::ggplot(df, ggplot2::aes(d, n, color = Broad_Category)) + ggplot2::geom_line() +
    ggplot2::theme(legend.position = "bottom")
  w <- plotly::plotly_build(trends_plotly(p))
  lg <- w$x$layout$legend
  expect_identical(lg$orientation, "h")
  expect_identical(lg$xanchor, "left")
  expect_equal(lg$x, 0)
  expect_lt(lg$y, 0)
  expect_identical(lg$title$text, "Category")
  expect_identical(lg$title$side, "top")
  expect_equal(w$x$layout$height, 500)
})
