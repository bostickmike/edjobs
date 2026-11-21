# Higher Ed Colleges tab UI (step 7 slice 2b): he_colleges_panel().
skip_if_not_installed("shiny"); skip_if_not_installed("bslib"); skip_if_not_installed("DT")
skip_if_not_installed("plotly"); skip_if_not_installed("shinycssloaders")

test_that("he_colleges_panel puts the state's notes and compare defaults in", {
  h <- as.character(he_colleges_panel(c("Col A", "Col B", "Col C"), c("Col B", "Col C"),
                                      salaries_note = "Salary note here.", county_note = "County note here."))
  expect_match(h, "data-value=\"he_summary\"", fixed = TRUE)
  expect_match(h, "data-value=\"he_summary_comp\"", fixed = TRUE)
  expect_match(h, "<strong>Salaries: </strong>\\s*Salary note here.")
  expect_match(h, "<strong>County: </strong>\\s*County note here.")
  expect_match(h, "<option value=\"Col B\" selected>Col B</option>", fixed = TRUE)
  expect_no_match(h, "<option value=\"Col A\" selected>", fixed = TRUE)
  expect_match(h, "id=\"he_compare_jobs_table\"", fixed = TRUE)
})
