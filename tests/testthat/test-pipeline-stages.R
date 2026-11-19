# Pipeline stages (review step 5): each must write exactly what the Rmd chunk it replaces wrote.

stage_fixture <- function() {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  app <- file.path(dir, "app"); dir.create(app)
  profile <- data.frame(District = c("A SD", "B SD", "C SD"), NCES_LEAID = c("5600001", "5600002", NA),
                        County = c("Alpha County", "Alpha County", "Beta County"),
                        Median_Household_Income = c(1, 1, 2), ACS_Year = 2020L, Other = c("x", "y", "z"),
                        stringsAsFactors = FALSE)
  utils::write.csv(profile, file.path(app, "salarymap2.csv"), row.names = FALSE)
  context <- data.frame(County = c("Alpha County", "Beta County"), Median_Household_Income = c(60000, 52000),
                        Median_Gross_Rent = c(900, 800), Population = c(10000, 2000), ACS_Year = 2024L,
                        Mining_Employment_Share = c(0.1, 0.02), Population_Change_Pct = c(0.01, -0.03),
                        Median_Household_Income_MOE = c(2000, 3000), stringsAsFactors = FALSE)
  list(dir = dir, app = app, context = context,
       config = state_config("Toyland", "99", app_dir = app))
}

# The chunk as both states' Rmds wrote it before the stage existed.
old_chunk <- function(path, context) {
  x <- utils::read.csv(path, fileEncoding = "UTF-8") %>%
    dplyr::select(-dplyr::any_of(c("Median_Household_Income", "Median_Gross_Rent", "Population",
                                   "ACS_Year", "Mining_Employment_Share", "Population_Change_Pct",
                                   "Median_Household_Income_MOE", "Median_Gross_Rent_MOE", "Mining_Employment_Share_MOE"))) %>%
    dplyr::left_join(context, by = "County")
  utils::write.csv(x, path, row.names = FALSE)
}

test_that("refresh_county_context writes byte for byte what the old chunk wrote, and logs the fetch", {
  f <- stage_fixture()
  withr::local_dir(f$dir)
  expected <- file.path(f$dir, "expected.csv")
  file.copy(file.path(f$app, "salarymap2.csv"), expected)
  old_chunk(expected, f$context)
  out <- refresh_county_context("salarymap2.csv", "Census ACS county context (TY)", config = f$config,
                                fetch = function() f$context)
  expect_identical(readLines(file.path(f$app, "salarymap2.csv")), readLines(expected))
  expect_equal(nrow(out), 2)
  log <- utils::read.csv("scrape_log.csv")
  expect_equal(log$source[nrow(log)], "Census ACS county context (TY)")
  expect_equal(read_source_retrieved(f$app)$Source, "Census ACS")
  # A second run gives the same file (no .x/.y columns).
  refresh_county_context("salarymap2.csv", "Census ACS county context (TY)", config = f$config, fetch = function() f$context)
  expect_identical(readLines(file.path(f$app, "salarymap2.csv")), readLines(expected))
})

test_that("a failed county-context fetch leaves the file untouched", {
  f <- stage_fixture()
  withr::local_dir(f$dir)
  before <- readLines(file.path(f$app, "salarymap2.csv"))
  expect_message(out <- refresh_county_context("salarymap2.csv", "Census ACS county context (TY)", config = f$config,
                                               fetch = function() stop("no key")), "untouched")
  expect_equal(nrow(out), 0)
  expect_identical(readLines(file.path(f$app, "salarymap2.csv")), before)
})

test_that("the county-context fetch gets the state's county_suffix", {
  f <- stage_fixture()
  withr::local_dir(f$dir)
  cfg <- state_config("Toyland", "99", app_dir = f$app, county_suffix = ", Toyland")
  seen <- NULL
  local_mocked_bindings(fetch_census_county_context = function(...) { seen <<- list(...); f$context })
  refresh_county_context("salarymap2.csv", "x", config = cfg)
  expect_identical(seen$county_suffix, ", Toyland")
  expect_error(state_config("T", "99", "a", county_suffix = c("a", "b")), "county_suffix")
})

test_that("refresh_source_record_links writes what the old chunk wrote and skips a missing file", {
  f <- stage_fixture()
  expected <- file.path(f$dir, "expected.csv")
  file.copy(file.path(f$app, "salarymap2.csv"), expected)
  utils::write.csv(add_source_record_links(utils::read.csv(expected, fileEncoding = "UTF-8")), expected, row.names = FALSE)
  done <- refresh_source_record_links(config = f$config)
  expect_equal(basename(done), "salarymap2.csv")
  expect_identical(readLines(file.path(f$app, "salarymap2.csv")), readLines(expected))
  expect_true(any(grepl("CCD_Link", readLines(expected)[1])))
})

test_that("the stages need a state config", {
  expect_error(refresh_county_context("x.csv", "x", config = NULL), "no state config")
  expect_error(refresh_source_record_links(config = NULL), "no state config")
})

test_that("refresh_file_columns writes what the old per-fetch chunk wrote, keyed by Name, and keeps others' columns", {
  f <- stage_fixture()
  withr::local_dir(f$dir)
  prof <- data.frame(Name = c("U1", "U2"), IPEDS_UNITID = c(240727L, 240505L), Enrollment = c(1, 2),
                     Enrollment_Year = 2020L, Pell_Year = 2019L, stringsAsFactors = FALSE)
  utils::write.csv(prof, file.path(f$app, "salarymap.csv"), row.names = FALSE)
  enr <- data.frame(Name = c("U1", "U2"), Enrollment = c(9500.5, 3100), Enrollment_Year = 2023L)
  expected <- file.path(f$dir, "expected.csv")
  file.copy(file.path(f$app, "salarymap.csv"), expected)
  x <- utils::read.csv(expected, fileEncoding = "UTF-8") %>%
    dplyr::select(-dplyr::any_of(c("Enrollment", "Enrollment_Year"))) %>%
    dplyr::left_join(enr, by = "Name")
  utils::write.csv(x, expected, row.names = FALSE)
  out <- refresh_file_columns("salarymap.csv", "IPEDS fall enrollment FTE (TY)", fetch = function() enr,
                              expected_cols = c("Name", "Enrollment", "Enrollment_Year"), by = "Name",
                              retrieved_source = "Urban IPEDS", config = f$config)
  expect_identical(readLines(file.path(f$app, "salarymap.csv")), readLines(expected))
  expect_equal(out, enr)
  expect_true("Urban IPEDS" %in% read_source_retrieved(f$app)$Source)
  # A failed fetch: the default message names the file and its columns; the file stays.
  before <- readLines(file.path(f$app, "salarymap.csv"))
  expect_message(refresh_file_columns("salarymap.csv", "IPEDS fall enrollment FTE (TY)", fetch = function() stop("524"),
                                      expected_cols = c("Name", "Enrollment", "Enrollment_Year"), by = "Name",
                                      config = f$config),
                 "salarymap.csv's Enrollment, Enrollment_Year untouched")
  expect_identical(readLines(file.path(f$app, "salarymap.csv")), before)
  expect_error(refresh_file_columns("salarymap.csv", "x", fetch = function() enr, expected_cols = "Enrollment",
                                    by = "Name", config = f$config))
})

test_that("refresh_file_columns' then and relationship reproduce the SAIPE and lunch blocks byte for byte", {
  f <- stage_fixture()
  withr::local_dir(f$dir)
  prof <- data.frame(District = c("A SD", "B SD", "C SD"), Child_Poverty_Rate = c(0.1, 0.2, NA),
                     Child_Poverty_Low = 0, Child_Poverty_High = 1, Free_Reduced_Lunch_Share = 0.5, stringsAsFactors = FALSE)
  utils::write.csv(prof, file.path(f$app, "salarymap2.csv"), row.names = FALSE)
  saipe <- data.frame(District = c("A SD", "B SD"), Child_Poverty_Rate = c(0.153, 0.287), SAIPE_Year = 2024L,
                      SAIPE_Population = c(15000, 900))
  expected <- file.path(f$dir, "expected.csv")
  file.copy(file.path(f$app, "salarymap2.csv"), expected)
  x <- utils::read.csv(expected, fileEncoding = "UTF-8") %>%
    dplyr::select(-dplyr::any_of(c("Child_Poverty_Rate", "SAIPE_Year", "SAIPE_Population", "Child_Poverty_Low", "Child_Poverty_High"))) %>%
    dplyr::left_join(saipe, by = "District") %>%
    add_saipe_range()
  utils::write.csv(x, expected, row.names = FALSE)
  refresh_file_columns("salarymap2.csv", "SAIPE (TY)", fetch = function() saipe, by = "District",
                       expected_cols = c("District", "Child_Poverty_Rate", "SAIPE_Year", "SAIPE_Population"),
                       replace = c("Child_Poverty_Rate", "SAIPE_Year", "SAIPE_Population", "Child_Poverty_Low", "Child_Poverty_High"),
                       then = add_saipe_range, config = f$config)
  expect_identical(readLines(file.path(f$app, "salarymap2.csv")), readLines(expected))
  # many-to-one: two districts share one lunch row; a duplicated key in the fetch is an error.
  frl <- data.frame(District = c("A SD", "B SD", "C SD"), Free_Reduced_Lunch_Share = c(0.4, 0.6, 0.3))
  refresh_file_columns("salarymap2.csv", "FRL (TY)", fetch = function() frl, by = "District",
                       expected_cols = c("District", "Free_Reduced_Lunch_Share"), relationship = "many-to-one",
                       config = f$config)
  expect_equal(utils::read.csv(file.path(f$app, "salarymap2.csv"))$Free_Reduced_Lunch_Share, c(0.4, 0.6, 0.3))
  expect_error(refresh_file_columns("salarymap2.csv", "FRL (TY)", fetch = function() rbind(frl, frl), by = "District",
                                    expected_cols = c("District", "Free_Reduced_Lunch_Share"), relationship = "many-to-one",
                                    config = f$config))
})
