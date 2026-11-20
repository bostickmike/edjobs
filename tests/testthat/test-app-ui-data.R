# Data tab UI (step 7 slice 2b): data_tab_panel(), its tile and provenance-row constructors, and the tile counts.
skip_if_not_installed("shiny"); skip_if_not_installed("bslib"); skip_if_not_installed("DT"); skip_if_not_installed("shinycssloaders")

test_that("data_tile fills shared fields from the defaults and lets the state override them", {
  x <- data_tile("download_weekly_totals", "Weekly totals.", label = "Weekly Totals (CSV)")
  expect_identical(x$icon, "chart-line")
  expect_identical(x$label, "Weekly Totals (CSV)")
  expect_identical(x$dictionary_file, "k12_district_weekly_totals.csv")
  expect_identical(data_tile("download_he_spells")$heading, "Higher Ed Listing Duration")
  expect_error(data_tile("download_active_k12"), "needs text")
  expect_error(data_tile("download_new_file", "Text."), "needs icon, heading, label")
})

test_that("data_provenance_row takes a shared row and needs all four cells otherwise", {
  r <- data_provenance_row("ccd_staffing", layer = "Staffing")
  expect_identical(r$source, "NCES Common Core of Data (CCD) via Urban Institute")
  expect_identical(r$badge, "info")
  expect_error(data_provenance_row(layer = "X"), "needs source, cadence, description")
})

test_that("data_tab_panel draws the tiles in order, the popover columns, the table and the repo links", {
  dict <- data.frame(File_Name = "combinedclean.csv", Column_Name = "Title", Definition = "Job title.")
  html <- as.character(data_tab_panel(
    tiles = list(data_tile("download_active_k12", "Open jobs.", sources = "boards."), data_tile("download_dictionary")),
    dictionary = dict,
    provenance_rows = list(data_provenance_row(layer = "Jobs", source = "Boards", cadence = "Weekly", description = "D.", badge = "success"),
                           data_provenance_row("county")),
    citation = "Me (2026). Dash. ", repo_url = "https://github.com/x/y"))
  expect_true(regexpr("download_active_k12", html) < regexpr("download_dictionary", html))
  expect_match(html, "<code>Title</code>", fixed = TRUE)
  expect_match(html, "badge text-bg-success", fixed = TRUE)
  expect_match(html, "County Economic Context", fixed = TRUE)
  expect_match(html, "table table-striped table-bordered table-hover", fixed = TRUE)
  expect_match(html, "href=\"https://github.com/x/y/issues\"", fixed = TRUE)
  expect_match(html, "data_dictionary_table", fixed = TRUE)
})

test_that("rows_phrase rounds down like the hand-written counts and download_rows counts what the download gives", {
  expect_identical(rows_phrase(8401), " (8,400+ records)")
  expect_identical(rows_phrase(13513), " (13,000+ records)")
  expect_identical(rows_phrase(57, "rows"), " (57 rows)")
  expect_identical(rows_phrase(NA), "")
  f <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(data.frame(District = "A", Archive_Date = c("2026-11-16", "2026-11-19"), n = 1:2), f, row.names = FALSE)
  expect_identical(download_rows(f), 2L)
  expect_identical(download_rows(f, latest_week = TRUE), 1L)
  expect_identical(download_rows(file.path(tempdir(), "no-such-file.csv")), NA_integer_)
})
