# K-12 Trends tab UI (step 7 slice 2b): k12_trends_panel().
skip_if_not_installed("shiny"); skip_if_not_installed("bslib"); skip_if_not_installed("shinyWidgets")
skip_if_not_installed("plotly"); skip_if_not_installed("shinycssloaders")

test_that("k12_trends_panel shows the slider label and counting note only when asked", {
  d <- as.Date("2026-01-05") + 7 * 0:60
  k12sum <- data.frame(District = "Total", Broad_Category = rep(c("Math", "Science"), length.out = 61), Archive_Date = d)
  now <- data.frame(District = c("Total", "Dist Z"))
  plain <- as.character(k12_trends_panel(now, k12sum))
  expect_match(plain, "data-value=\"k12_trends_historical\"", fixed = TRUE)
  expect_match(plain, "Dist Z", fixed = TRUE)
  expect_no_match(plain, "k12_slider_label", fixed = TRUE)
  expect_no_match(plain, "alert-info", fixed = TRUE)
  # Both popovers say what the counts leave out (Mike 2026-11-24), so a reader knows what the charts show.
  expect_equal(lengths(regmatches(plain, gregexpr("Counts full teaching positions only: part-time teacher, substitute and coaching postings are not included.", plain, fixed = TRUE))), 2)
  full <- as.character(k12_trends_panel(now, k12sum, c(max(d) - 70, max(d)), slider_label = TRUE,
                                        counting_note = htmltools::tagList(htmltools::tags$strong("How: "), "once.")))
  expect_match(full, "id=\"k12_slider_label\"", fixed = TRUE)
  expect_match(full, "role=\"note\">\\s*<strong>How: </strong>\\s*once.\\s*</div>")
  # The plot's container is as tall as the plot the server draws (500px), so nothing sits on its axis; the note is a
  # plain footnote (placement checked by eye in the running app, 2026-11-24).
  expect_match(full, "id=\"k12_longitudinal_plot\"[^>]*height:\\s*500px")
  expect_no_match(full, "alert-info", fixed = TRUE)
})
