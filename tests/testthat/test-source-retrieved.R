# Source retrieval dates (source_retrieved.R; edjobs 0.29.0): a date is recorded only when a fetch succeeded and
# its data was kept, so the footer's date always belongs to the data on the page.
fixture_dir <- normalizePath(test_path("fixtures"))   # absolute: tests below change into a temp directory
committed <- function(name) read.csv(file.path(fixture_dir, paste0("reference_", sub("\\.csv$", "", name), "_mt_2026-11-10.csv")),
                                     stringsAsFactors = FALSE)

test_that("record_source_retrieved keeps one row per source and replaces its date", {
  d <- withr::local_tempdir()
  expect_equal(nrow(read_source_retrieved(d)), 0)
  record_source_retrieved("BLS QCEW", d, as.Date("2026-11-03"))
  record_source_retrieved("Census ACS", d, as.Date("2026-11-10"))
  record_source_retrieved("BLS QCEW", d, as.Date("2026-11-10"))
  x <- read_source_retrieved(d)
  expect_equal(x$Source, c("BLS QCEW", "Census ACS"))
  expect_equal(x$Retrieved, c("2026-11-10", "2026-11-10"))
})

test_that("source_retrieved_label gives one date, a range, or nothing", {
  x <- data.frame(Source = c("Urban CCD", "Urban IPEDS", "BLS OEWS"), Retrieved = c("2026-11-10", "2026-11-03", "2026-10-31"))
  expect_equal(source_retrieved_label(x, "BLS OEWS"), "retrieved 2026-10-31")
  expect_equal(source_retrieved_label(x, c("Urban CCD", "Urban IPEDS")), "retrieved 2026-11-03 to 2026-11-10")
  expect_equal(source_retrieved_label(x, "Census ACS"), "")
})

test_that("safe_scrape records the date only for a non-empty result", {
  withr::local_dir(withr::local_tempdir())
  safe_scrape("ok", function() data.frame(a = 1), "a", retrieved_source = "Census ACS", app_dir = ".")
  safe_scrape("empty", function() data.frame(a = numeric(0)), "a", retrieved_source = "Census SAIPE", app_dir = ".")
  suppressMessages(safe_scrape("fails", function() stop("down"), "a", retrieved_source = "Urban CCD", app_dir = "."))
  safe_scrape("no key", function() data.frame(a = 1), "a")
  expect_equal(read_source_retrieved(".")$Source, "Census ACS")
})

test_that("write_reference_file records the date only when the refresh is kept", {
  withr::local_dir(withr::local_tempdir())
  good <- committed("qcew_county_school_pay.csv")
  path <- file.path(getwd(), "qcew_county_school_pay.csv")
  write.csv(good, path, row.names = FALSE)
  expect_false(suppressMessages(write_reference_file(good[1, ], path, retrieved_source = "BLS QCEW")))
  expect_equal(nrow(read_source_retrieved(getwd())), 0)
  expect_true(write_reference_file(good, path, retrieved_source = "BLS QCEW"))
  expect_equal(read_source_retrieved(getwd())$Retrieved, format(Sys.Date()))
})

test_that("refresh_oews_wages keeps the earlier date when the year on file is current", {
  withr::local_dir(withr::local_tempdir())
  oews <- data.frame(Area_Code = "3000000", Area_Name = "Montana", SOC = "252021", Occupation = "Elementary teachers",
                     Annual_Median = 55000, Hourly_Median = NA, Year = "2024")   # the "on file" check reads only Year
  write.csv(oews, "oews_role_wages.csv", row.names = FALSE)
  record_source_retrieved("BLS OEWS", ".", as.Date("2026-10-31"))
  local_mocked_bindings(fetch_oews_latest_year = function(...) as.character(oews$Year[1]))
  suppressMessages(refresh_oews_wages("MT", "oews_role_wages.csv", api_key = "", retrieved_source = "BLS OEWS"))
  expect_equal(read_source_retrieved(".")$Retrieved, "2026-10-31")
})
