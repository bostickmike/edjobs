# Step 7 slice 1 (2026-11-21): the shared dashboard server and UI helpers. Each state's app parity run (shinytest2,
# every tab) is the real check; these cover the contract.
test_that("dashboard_server_shared() names every data object it reads", {
  f <- edjobs::dashboard_server_shared
  g <- codetools::findGlobals(f, merge = FALSE)
  ns <- asNamespace("edjobs")
  known <- c(ls(ns, all.names = TRUE), ls(parent.env(ns), all.names = TRUE), ls(baseenv(), all.names = TRUE))
  # every free function is base or edjobs's own (package calls are pkg::fn)
  expect_equal(setdiff(g$functions, known), character(0))
  # every free variable is a column name declared in globals.R, not an app object read by accident
  expect_equal(setdiff(g$variables, c(known, utils::globalVariables(package = "edjobs"))), character(0))
  expect_true(all(dashboard_server_data_names() %in% all.names(body(f))))
})

test_that("dashboard_server_shared() stops on missing data objects", {
  skip_if_not_installed("shiny")
  expect_error(dashboard_server_shared(NULL, NULL, NULL, data = list(k12sum = data.frame()), state_name = "Montana",
                                       feedback_repo = "x/y"), "lacks ccdata")
})

test_that("UI helpers build the same tags app.R did", {
  skip_if_not_installed("shiny"); skip_if_not_installed("bslib")
  expect_match(as.character(spells_footnote()), "not time to fill", fixed = TRUE)
  expect_match(as.character(download_tile("download", "Jobs", "All jobs", "dl_jobs", "Download")), 'id="dl_jobs"', fixed = TRUE)
  expect_match(as.character(dash_card("Title", "body")), "card-header", fixed = TRUE)
  expect_match(as.character(help_popover("About", "text", source = "OPI")), "Source: ", fixed = TRUE)
  expect_match(as.character(info_popover("Method", "text")), "circle-info", fixed = TRUE)
  expect_match(as.character(kpi_value_box("12", "Openings", color = "green")), "bg-success", fixed = TRUE)
})

test_that("the Listing Duration panels call only base, edjobs or pkg::fn()", {
  ns <- asNamespace("edjobs")
  known <- c(ls(ns, all.names = TRUE), ls(parent.env(ns), all.names = TRUE), ls(baseenv(), all.names = TRUE))
  for (f in list(k12_spells_panel, he_spells_panel))
    expect_equal(setdiff(codetools::findGlobals(f, merge = FALSE)$functions, known), character(0))
})

test_that("the Listing Duration panels build with their input ids", {
  skip_if_not_installed("shiny"); skip_if_not_installed("bslib"); skip_if_not_installed("shinycssloaders")
  k <- as.character(htmltools::tagList(bslib::page_navbar(k12_spells_panel(), he_spells_panel())))
  for (id in c("k12_spell_status", "k12_spell_district", "he_spell_status", "he_spell_institution"))
    expect_match(k, paste0('id="', id, '"'), fixed = TRUE)
})
