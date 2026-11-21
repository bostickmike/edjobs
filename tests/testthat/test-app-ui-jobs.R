# Jobs tabs UI (step 7 slice 2b): jobs_tab_panel().
skip_if_not_installed("shiny"); skip_if_not_installed("bslib"); skip_if_not_installed("shinyWidgets"); skip_if_not_installed("DT")

test_that("jobs_tab_panel builds each level's ids, and the Appointment filter only for Higher Ed", {
  k <- as.character(jobs_tab_panel("k12", c("Note one. ", "Note two.")))
  h <- as.character(jobs_tab_panel("he", "HE note."))
  expect_match(k, "data-value=\"k12_table\"", fixed = TRUE)
  expect_match(k, "id=\"k12_quick_filter\"", fixed = TRUE)
  expect_match(k, "id=\"k12_jobs\"", fixed = TRUE)
  expect_match(k, "Note one.\\s+Note two.")
  expect_no_match(k, "he_table_appointment", fixed = TRUE)
  expect_match(h, "id=\"he_filter_status\"", fixed = TRUE)
  expect_match(h, "id=\"he_table_appointment\"", fixed = TRUE)
  expect_error(jobs_tab_panel("x", "n"))
})
