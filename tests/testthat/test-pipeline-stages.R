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
