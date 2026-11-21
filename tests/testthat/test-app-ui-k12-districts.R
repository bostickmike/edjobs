# K-12 Districts tab UI (step 7 slice 2b): k12_districts_panel().
skip_if_not_installed("shiny"); skip_if_not_installed("bslib"); skip_if_not_installed("DT")
skip_if_not_installed("plotly"); skip_if_not_installed("shinycssloaders")

test_that("k12_districts_panel puts the state's text and salary card in place", {
  h <- as.character(k12_districts_panel(
    c("Dist A", "Dist B"), "Dist B", openings_note = "Open note.",
    directory_notes = htmltools::tagList(htmltools::p("Pay note."), htmltools::p("County note.")),
    directory_source = "Dir source.", compare_blurb = "Pick some.",
    salary_card = dash_card("Salary Card Title", plotly::plotlyOutput("k12_compare_salary_plot")),
    openings_source = "Open source."))
  expect_match(h, "data-value=\"k12_summary\"", fixed = TRUE)
  expect_match(h, "<strong>Openings: </strong>\\s*Open note.")
  expect_lt(regexpr("Teacher vacancy rate", h), regexpr("Pay note.", h))
  for (x in c("County note.", "Dir source.", "Pick some.", "Salary Card Title", "Open source.", "id=\"k12_compare_salary_plot\""))
    expect_match(h, x, fixed = TRUE)
  expect_match(h, "<option value=\"Dist B\" selected>Dist B</option>", fixed = TRUE)
})
