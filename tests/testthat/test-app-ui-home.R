# Home tab UI (step 7 slice 2b): home_tab_panel().
skip_if_not_installed("shiny"); skip_if_not_installed("bslib")

test_that("home_tab_panel puts in the state's name, source, footer and own card", {
  h <- as.character(home_tab_panel("Testland", "the Testland job board.", "2026-11-21", "Created by Someone"))
  expect_match(h, "data-value=\"intro\"", fixed = TRUE)
  expect_match(h, "Education Jobs in Testland", fixed = TRUE)
  expect_match(h, "the Testland job board.", fixed = TRUE)
  expect_match(h, "Refreshed on: 2026-11-21 · Created by Someone", fixed = TRUE)
  expect_match(h, "id=\"home_not_marked\"", fixed = TRUE)
  expect_match(h, not_marked_note("he"), fixed = TRUE)
  expect_no_match(h, "extra-card", fixed = TRUE)
  h2 <- as.character(home_tab_panel("Testland", "s", "d", "c", extra = htmltools::div(class = "extra-card", "x")))
  expect_match(h2, "class=\"extra-card\"", fixed = TRUE)
  expect_lt(regexpr("extra-card", h2), regexpr("Refreshed on:", h2))
})
