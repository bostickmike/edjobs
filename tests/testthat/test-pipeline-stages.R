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

# --- scrape_registry_platforms() (slice 4) -----------------------------------------------------------------------

toy_registry <- function() {
  data.frame(District = c("Alpha SD", "Beta SD", "Gamma SD", "Delta SD", "Echo SD"),
             Platform = c("AlphaHeuristic", "Fam", "FamText", "Fam", "Skip"),
             Job_Link = c("https://alpha.example/jobs", "https://beta.example/jobs", "https://gamma.example/jobs",
                          "https://delta.example/jobs", "https://echo.example/jobs"),
             City = c("Alpha", "Beta", "Gamma", NA, "Echo"),
             Posting_Source = c("Alpha board (direct board)", "", "Gamma board (direct board)", NA, ""),
             stringsAsFactors = FALSE)
}

toy_source <- function() {
  scraper_registry(toy_registry(), "District", "toy registry",
    specs = list(Fam = scraper_spec("fetch_fam_postings", list("Job_Link", registry_text("City"))),
                 FamText = scraper_spec("fetch_fam_postings", list("Job_Link", registry_text("City"), function(row) "text"))),
    fallback = function(platform, row) {
      if (grepl("Heuristic$", platform)) scraper_spec(paste0("fetch_", tolower(sub("Heuristic$", "", platform)), "_postings"), "Job_Link")
    })
}

test_that("scrape_registry_platforms runs each row's spec, labels it, and keeps groups apart", {
  dir <- withr::local_tempdir(); withr::local_dir(dir)
  calls <- list()
  fetch_alpha_postings <- function(url) {
    calls[[length(calls) + 1]] <<- list("alpha", url)
    data.frame(Title = "Teacher", Location = "Alpha", Posted_Date = "11/03/2026", Link = paste0(url, "/1"))
  }
  fetch_fam_postings <- function(url, city, mode = "html") {
    calls[[length(calls) + 1]] <<- list("fam", url, city, mode)
    if (grepl("delta", url)) stop("site down")
    data.frame(Title = c("Aide", "Cook"), Location = city, Posted_Date = c("2026-11-01", NA),
               Link = paste0(url, c("/a", "/b")), Pay_Min = c(15, NA), Pay_Max = NA_real_,
               Pay_Unit = c("hour", NA), Pay_Text = c("$15/hour", NA))
  }
  out <- scrape_registry_platforms(toy_registry(), list(alpha = "AlphaHeuristic", fam = c("Fam", "FamText")),
                                   source = toy_source())
  expect_named(out, c("alpha", "fam"))
  expect_identical(calls, list(list("alpha", "https://alpha.example/jobs"),
                               list("fam", "https://beta.example/jobs", "Beta", "html"),
                               list("fam", "https://gamma.example/jobs", "Gamma", "text"),
                               list("fam", "https://delta.example/jobs", NA_character_, "html")))
  expect_identical(out$alpha, data.frame(title = "Teacher", date_posted = as.Date("2026-11-03"), position = NA_character_,
                                         location = "Alpha", url = "https://alpha.example/jobs/1", District = "Alpha SD",
                                         Posting_Source = "Alpha board (direct board)"))
  # Registry order within a group; a blank label falls back to the district's; Delta's failure is logged, not fatal.
  expect_identical(out$fam$District, c("Beta SD", "Beta SD", "Gamma SD", "Gamma SD"))
  expect_identical(out$fam$Posting_Source, rep(c("Beta SD site (direct board)", "Gamma board (direct board)"), each = 2))
  expect_identical(out$fam$Pay_Text, c("$15/hour", NA, "$15/hour", NA))
  expect_identical(out$fam$date_posted, as.Date(c("2026-11-01", NA, "2026-11-01", NA)))
  log <- utils::read.csv("scrape_log.csv")
  expect_identical(log$source, c("Alpha SD", "Beta SD", "Gamma SD", "Delta SD"))
  expect_identical(log$status, c("ok", "ok", "ok", "error"))
})

test_that("scrape_registry_platforms returns the empty K-12 frame for a group with no rows, and stops on no spec", {
  dir <- withr::local_tempdir(); withr::local_dir(dir)
  out <- scrape_registry_platforms(toy_registry(), "NobodyHeuristic", source = toy_source())
  expect_length(out, 1)
  expect_identical(names(out[[1]]), c("title", "date_posted", "position", "location", "url", "District", "Posting_Source"))
  expect_s3_class(out[[1]]$date_posted, "Date")
  expect_equal(nrow(out[[1]]), 0)
  expect_error(scrape_registry_platforms(toy_registry(), "Skip", source = toy_source()), "no scraper spec for platform 'Skip'")
})

test_that("a group's dates are standardized together, as the per-platform blocks did", {
  # parse_date_time() picks formats from the whole vector: the stage must not merge groups before parsing.
  dir <- withr::local_tempdir(); withr::local_dir(dir)
  reg <- data.frame(District = c("A", "B"), Platform = c("AHeuristic", "BHeuristic"), Job_Link = c("a", "b"))
  fetch_a_postings <- function(url) data.frame(Title = "x", Location = "", Posted_Date = "03/04/2026", Link = url)
  fetch_b_postings <- function(url) data.frame(Title = "y", Location = "", Posted_Date = c("13/04/2026", "14/04/2026", "03/04/2026"), Link = url)
  src <- scraper_registry(reg, "District", "r", fallback = function(platform, row)
    scraper_spec(paste0("fetch_", tolower(sub("Heuristic$", "", platform)), "_postings"), "Job_Link"))
  out <- scrape_registry_platforms(reg, c("AHeuristic", "BHeuristic"), source = src)
  expect_identical(out[[1]]$date_posted, standardize_date("03/04/2026"))
  expect_identical(out[[2]]$date_posted, standardize_date(c("13/04/2026", "14/04/2026", "03/04/2026")))
})

test_that("registry_posting_source uses the column, else the district's default", {
  expect_identical(registry_posting_source(data.frame(District = "X SD", Posting_Source = "X board")), "X board")
  expect_identical(registry_posting_source(data.frame(District = "X SD", Posting_Source = "")), "X SD site (direct board)")
  expect_identical(registry_posting_source(data.frame(District = "X SD")), "X SD site (direct board)")
})

test_that("scrape_registry_platforms uses the caller's safe_scrape, so a pipeline dry run's stub reaches it", {
  dir <- withr::local_tempdir(); withr::local_dir(dir)
  called <- character(0)
  fetch_alpha_postings <- function(url) stop("the live scraper must not run")
  safe_scrape <- function(source_name, scrape_fn, expected_cols, log_path = NULL) {
    called <<- c(called, source_name)
    data.frame(Title = "Stub", Location = "", Posted_Date = "2026-11-01", Link = "u")
  }
  out <- scrape_registry_platforms(toy_registry(), "AlphaHeuristic", source = toy_source())
  expect_identical(called, "Alpha SD")
  expect_identical(out[[1]]$title, "Stub")
  expect_false(file.exists("scrape_log.csv"))
})

# --- scrape_registry_platforms(shape = "raw"): higher ed (slice 5) ----------------------------------------------

he_toy_registry <- function() {
  data.frame(Institution = c("Alpha College", "Beta U"), Platform = c("JobBoard", "Shared"),
             Feed_URL = c("https://alpha.example/feed", "https://beta.example/feed"),
             Job_Link = c("https://alpha.example", "https://beta.example"), stringsAsFactors = FALSE)
}

he_toy_source <- function() {
  scraper_registry(he_toy_registry(), "Institution", "he toy registry",
    specs = list(JobBoard = scraper_spec("fetch_board_postings", "Feed_URL"),
                 Shared = scraper_spec("fetch_shared_postings", "Feed_URL")))
}

test_that("raw shape keeps the scraper's columns, adds the name column, and runs the after hook", {
  dir <- withr::local_tempdir(); withr::local_dir(dir)
  fetch_board_postings <- function(url) data.frame(Title = "Prof", Location = "X", Posted_Date = "2026-11-01", Link = url, Close_Date = "2026-12-01")
  fetch_shared_postings <- function(url) data.frame(Title = c("A", "B"), Location = "Y", Posted_Date = NA, Link = url, Unit = c("m", "n"))
  out <- scrape_registry_platforms(he_toy_registry(), list(board = "JobBoard", shared = "Shared"), source = he_toy_source(),
                                   shape = "raw",
                                   after = list(Shared = function(df, row) { df$Institution <- paste(df$Unit, row$Institution); df }))
  expect_identical(names(out$board), c("Title", "Location", "Posted_Date", "Link", "Close_Date", "Institution"))
  expect_identical(out$board$Institution, "Alpha College")
  expect_identical(out$shared$Institution, c("m Beta U", "n Beta U"))
  expect_false("Posting_Source" %in% names(out$board))
  log <- utils::read.csv("scrape_log.csv")
  expect_identical(log$source, c("Alpha College", "Beta U"))
})

test_that("raw shape keeps an id column the scraper set itself (JazzHR's campus split)", {
  dir <- withr::local_tempdir(); withr::local_dir(dir)
  fetch_board_postings <- function(url) data.frame(Title = "P", Location = "", Posted_Date = NA, Link = url, Institution = "Highlands College")
  out <- scrape_registry_platforms(he_toy_registry()[1, ], "JobBoard", source = he_toy_source(), shape = "raw")
  expect_identical(out[[1]]$Institution, "Highlands College")
})

test_that("k12 shape always takes the registry's name, as the old blocks did", {
  dir <- withr::local_tempdir(); withr::local_dir(dir)
  fetch_alpha_postings <- function(url) data.frame(Title = "T", Location = "", Posted_Date = NA, Link = url, District = "Somewhere else")
  out <- scrape_registry_platforms(toy_registry(), "AlphaHeuristic", source = toy_source())
  expect_identical(out[[1]]$District, "Alpha SD")
})

test_that("a source can be named by its where, and expected_cols can differ by platform", {
  dir <- withr::local_tempdir(); withr::local_dir(dir)
  local_mocked_bindings(state_scraper_sources = function() list(toy_source(), he_toy_source()))
  fetch_board_postings <- function(url) data.frame(Title = character(0))
  fetch_shared_postings <- function(url) data.frame(Title = character(0))
  out <- scrape_registry_platforms(he_toy_registry(), list(board = "JobBoard", shared = "Shared"),
                                   source = "he toy registry", shape = "raw",
                                   expected_cols = list(JobBoard = c("Title", "Institution")))
  expect_identical(names(out$board), c("Title", "Institution"))
  expect_identical(names(out$shared), c("Title", "Location", "Posted_Date", "Link"))
  expect_error(scrape_registry_platforms(he_toy_registry(), "JobBoard", source = "nope"), "no registry 'nope'")
})
