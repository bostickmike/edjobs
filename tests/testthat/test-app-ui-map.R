# Map tab UI (step 7 slice 2b): map_tab_panel().
skip_if_not_installed("shiny"); skip_if_not_installed("bslib"); skip_if_not_installed("leaflet"); skip_if_not_installed("shinycssloaders")

test_that("map_tab_panel puts the state's text around the shared filters and map", {
  bands <- list(low = list(unit = "hour", label = "Under $20"), high = list(unit = "year", label = "$60k+"))
  html <- as.character(map_tab_panel("Testland", about = list(htmltools::p("About one.")), partial_note = "Partial means X.",
                                     map_note = "Under the map. ", county_choices = c("All Counties" = "all", "A County" = "A"),
                                     pay_bands = bands, he_place_note = "Colleges at campus."))
  expect_match(html, "Testland Education Job Openings Map", fixed = TRUE)
  expect_true(regexpr("About one.", html) < regexpr("vacancy rates use different staffing sources", html))
  expect_true(regexpr("vacancy rates use different staffing sources", html) < regexpr("Partial means X.", html))
  expect_match(html, "Hourly: Under $20", fixed = TRUE)
  expect_match(html, "Annual: $60k+", fixed = TRUE)
  expect_match(html, "Under the map.\\s+Click a marker to see its jobs.\\s+<br/>\\s+Colleges at campus.")
  expect_match(html, "id=\"combined_map\"", fixed = TRUE)
  expect_no_match(as.character(map_tab_panel("T", list(), "P", "N. ", c(All = "all"), bands)), "<br/>", fixed = TRUE)
})
