# district_spec() and the Districts tab builders (step 7 slice 3 part 3). The two states' outputs were checked
# against their old app.R code on the real data (claude-notes/2026-11-21_step7-slice3b/parity.log); these pin the
# spec's rules on a small frame.

fake_map <- function() {
  data.frame(
    Name = c("North SD", "South SD", "Big College"), Type = c("K-12 District", "K-12 District", "Higher Ed Institution"),
    County = c("Elk", "Elk", "Pine"), CurrentCount = c(3L, 5L, 2L), Data_Coverage = c("Full", "Partial", NA),
    WeeklyNew = c(1L, 0L, 0L), Vacancy_Rate = c(0.05, NA, 0.02), Enrollment = c(900, 300, 4000),
    Students_Per_Teacher = c(14.26, 11, 18), Pay = c(52000, NA, NA), Pay_Prior = c(51000, NA, NA),
    Pay_Low = c(40000, 41000, NA), Pay_High = c(70000, NA, NA), Pay_Year = c("2025-2026", NA, NA),
    Pay_Source = c("Survey A", NA, NA), Salary_Year = c("2025-2026", NA, "2024-25"), Salary_Source = c("Survey A", NA, "IPEDS"),
    Median_Household_Income = c(60000, 60000, 55000), Median_Gross_Rent = c(800, 800, 700),
    Median_Household_Income_MOE = NA_real_, Median_Gross_Rent_MOE = NA_real_, Mining_Employment_Share = c(0.02, 0.02, 0.1),
    Mining_Employment_Share_MOE = NA_real_, Population_Change_Pct = 0.01, Child_Poverty_Rate = c(0.12, NA, NA),
    Child_Poverty_Low = NA_real_, Child_Poverty_High = NA_real_, Child_Poverty_Basis = NA_character_,
    Free_Reduced_Lunch_Share = c(0.4, NA, NA), FRL_Year = c("2023-24", NA, NA), ACS_Year = "2019-2023", SAIPE_Year = "2023",
    stringsAsFactors = FALSE
  )
}

fake_spec <- function(...) {
  district_spec(
    columns = list(
      district_column("Pay (%s)", value = "Pay", year_col = "Pay_Year", year_fallback = "this year", card = "Pay:"),
      district_column("Pay (%s)", value = "Pay_Prior", year_col = "Pay_Year", year_fallback = "last year", prior_year = TRUE),
      district_column("Pay Range", range = c("Pay_Low", "Pay_High"), card = "Range:"),
      district_column("Budget", value = "Pay", card = "Budget:", card_section = "budget")
    ),
    popup_lines = list(district_popup_line("Pay", "Pay", also = "Pay_Prior", also_label = " prior year", year_col = "Pay_Year"),
                       district_popup_line("Low", "Pay_Low", suffix = " (A)")),
    compare_bars = c("Pay bar" = "Pay"), compare_y_title = "Dollars",
    source_notes = list(district_source_note("Pay data:", "Pay_Source", year_col = "Pay_Year", url = "https://example.org",
                                             url_text = "example.org", after = "Not a base salary.")),
    saipe_scope_note = ". District-level.", saipe_standin_note = "Stand-in sentence.", ...
  )
}

test_that("constructors refuse incomplete column specs", {
  expect_error(district_column("A", value = "x", range = c("a", "b")), "not both")
  expect_error(district_column("A"), "not both")
  expect_error(district_column("A", range = "a"), "two columns")
  expect_error(district_column("A", value = "x", year_col = "y", year_fallback = "z"), "%s")
  expect_error(district_column("A (%s)", value = "x", year_col = "y"), "year_fallback")
  expect_error(district_column("A", value = "x", prior_year = TRUE), "year_col")
  expect_error(district_popup_line("A", "x", range = c("a", "b"), also = "c"), "not both")
  expect_error(district_source_note("A", "s", url = "https://x"), "together")
  expect_error(district_spec(list(), list(), c(a = "x"), "", list(), "", ""), "columns")
  expect_error(fake_spec(compare_bars = NULL), "compare_bars")
})

test_that("the spec's columns must exist in the map data", {
  expect_silent(check_district_spec(fake_spec(), fake_map()))
  expect_error(check_district_spec(fake_spec(), fake_map()[, setdiff(names(fake_map()), "Pay_Prior")]), "Pay_Prior")
})

test_that("prior_school_year_label() shifts a four-digit range back a year", {
  expect_equal(prior_school_year_label("2025-2026"), "2024-2025")
  expect_true(is.na(prior_school_year_label("2025-26")))
})

test_that("district_summary_df() puts the spec's columns between the district and county figures", {
  df <- district_summary_df(fake_map(), fake_spec())
  expect_equal(df$District, c("South SD", "North SD"))   # K-12 only, most openings first
  expect_equal(names(df)[8:12], c("Students per Teacher", "Pay (2025-2026)", "Pay (2024-2025)", "Pay Range", "Budget"))
  expect_equal(names(df)[13], "County Median Income")
  expect_equal(df[["Pay Range"]], c(NA, "$40,000 - $70,000"))
  expect_equal(attr(df, "currency_cols"), c("Pay (2025-2026)", "Pay (2024-2025)", "Budget", "County Median Income",
                                            "County Median Rent (per month)"))
  # Year fallback when no K-12 row has the year; repeated headers stop.
  m <- fake_map(); m$Pay_Year <- NA
  expect_equal(names(district_summary_df(m, fake_spec()))[9:10], c("Pay (this year)", "Pay (last year)"))
  same <- fake_spec(); same$columns[[2]]$prior_year <- FALSE
  expect_error(district_summary_df(fake_map(), same), "repeat")
})

test_that("district_summary_footnote() writes the source notes and the state's poverty sentences", {
  text <- function(x) gsub("\\s+", " ", as.character(x))
  html <- text(district_summary_footnote(fake_map(), fake_spec()))
  expect_match(html, "Pay data: Survey A (2025-2026 school year) — <a href=\"https://example.org\" target=\"_blank\">example.org</a> Not a base salary.", fixed = TRUE)
  expect_match(html, "census.gov/saipe</a> . District-level.", fixed = TRUE)
  expect_match(html, "Stand-in sentence.", fixed = TRUE)
  m <- fake_map(); m$Salary_Source <- NA
  expect_null(district_summary_footnote(m, fake_spec()))
  # A required year that is missing drops the note; require_year = FALSE keeps it without the year.
  m <- fake_map(); m$Pay_Year <- NA
  expect_no_match(text(district_summary_footnote(m, fake_spec())), "Pay data:")
  s <- fake_spec(); s$source_notes[[1]]$require_year <- FALSE
  expect_match(text(district_summary_footnote(m, s)), "Pay data: Survey A —", fixed = TRUE)
})

test_that("compare cards place pay rows above Enrollment and budget rows below Students / Teacher", {
  row <- fake_map()[1, ]
  labels <- vapply(district_card_rows(row, fake_spec()), `[[`, "", "label")
  expect_equal(labels, c("Pay:", "Range:", "Enrollment:", "Students / Teacher:", "Budget:", "County Median Rent:",
                         "Children in Poverty (Census est.):", "Free/Reduced Lunch:"))
  values <- district_card_rows(fake_map()[2, ], fake_spec())
  expect_equal(values[[1]]$value, "N/A")
  expect_equal(values[[2]]$value, "N/A")   # range with one end missing
  expect_true(values[[2]]$small)
  html <- as.character(district_compare_cards(c("North SD", "South SD"), fake_map(), fake_spec()))
  expect_match(html, "col-sm-6")
  expect_match(as.character(district_compare_cards(NULL, fake_map(), fake_spec())), "Please select")
})

test_that("popup pay lines show only where the amount is, with the optional parts", {
  html <- district_popup_pay_html(fake_map(), fake_spec())
  expect_equal(html[1], "<div>Pay: $52,000 ($51,000 prior year) &middot; 2025-2026</div><div>Low: $40,000 (A)</div>")
  expect_equal(html[2], "<div>Low: $41,000 (A)</div>")
  expect_equal(html[3], "")
})

test_that("the shared-vacancy remark needs the column and the spec's note", {
  m <- fake_map(); m$Campus_Of <- NA; m$Campus_Note <- NA; m$SearchActive <- FALSE; m$DisplayCount <- m$CurrentCount
  m$SampleTitles <- ""; m$Enrollment_Change_Pct <- NA; m$Pell_Recipient_Share <- NA; m$Faculty_Avg_Salary <- NA
  m$School_Pay_County <- NA; m$Link <- "https://x"; m$SearchKeyword <- NA; m$SearchBand <- ""; m$SearchQuery <- ""
  m$MatchingSample <- ""
  expect_no_match(district_map_popups(m, fake_spec(shared_vacancy_note = "joint figure"))[1], "joint figure")
  m$Vacancy_Rate_Shared <- c(TRUE, FALSE, FALSE)
  p <- district_map_popups(m, fake_spec(shared_vacancy_note = "joint figure"))
  expect_match(p[1], "vacancy rate: 5.0% <span style='color:#666;'>(joint figure)</span>", fixed = TRUE)
  expect_no_match(district_map_popups(m, fake_spec())[1], "joint figure")
  expect_length(district_map_labels(m), 3)
})
