# Slice E stages: each must write what the Rmd block it replaces wrote; the old blocks are kept here.

e_fixture <- function() {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  app <- file.path(dir, "app"); dir.create(app)
  colleges <- data.frame(Name = c("U1", "U2", "U3"), IPEDS_UNITID = c(240727L, 240505L, NA), County = "Alpha County",
                         Faculty_Avg_Salary = c(1, 2, NA), Salary_Year = 2020L, Salary_Source = "old",
                         Faculty_Avg_Salary_Y1Ago = 5, Faculty_Avg_Salary_Y2Ago = 6, Enrollment = c(10, 20, 30),
                         stringsAsFactors = FALSE)
  utils::write.csv(colleges, file.path(app, "salarymap.csv"), row.names = FALSE)
  districts <- data.frame(District = c("A SD", "B SD", "C SD"), NCES_LEAID = c(3000001L, 3000002L, NA),
                          Teachers_Total_FTE = 1, Enrollment = 2, CCD_Year = 2020L, CCD_Source = "old",
                          Data_Coverage = "Full", Other = c("x", "y", "z"), stringsAsFactors = FALSE)
  utils::write.csv(districts, file.path(app, "salarymap2.csv"), row.names = FALSE)
  list(dir = dir, app = app, config = state_config("Montana", "30", app_dir = app))
}
salary_rows <- function() data.frame(Name = c("U1", "U2", "U3"), Faculty_Avg_Salary = c(88123.4567, 70111.5, NA),
                                     Faculty_Avg_Salary_Professor = c(100000, NA, NA), Faculty_Count = c(50, 20, NA),
                                     Salary_Year = "2024", stringsAsFactors = FALSE)
trend_rows <- function() data.frame(Name = rep(c("U1", "U2", "U3"), 3), Year = rep(c(2024, 2023, 2022), each = 3),
                                    Faculty_Avg_Salary = c(88123.4567, 70111.5, NA, 85000.25, 69000, NA, 82000, 68000, NA))

# Montana's block (Wyoming's differed only in Salary_Note, kept when the map has it, and in the column order).
old_he_salary <- function(path, he_salary, salary_trend) {
  salarymap <- utils::read.csv(path, fileEncoding = "UTF-8") %>%
    dplyr::select(-dplyr::any_of(c("Faculty_Avg_Salary", "Faculty_Avg_Salary_Professor", "Faculty_Count", "Salary_Year",
                                   "Salary_Note", "Salary_Source"))) %>%
    dplyr::left_join(he_salary, by = "Name") %>%
    dplyr::mutate(Salary_Source = ifelse(is.na(Faculty_Avg_Salary), NA_character_,
                                         "IPEDS via Urban Institute Education Data Portal (educationdata.urban.org)"))
  latest_year <- max(salary_trend$Year)
  wide <- salary_trend %>% dplyr::mutate(YearsAgo = latest_year - Year) %>% dplyr::filter(YearsAgo > 0) %>%
    dplyr::select(Name, YearsAgo, Faculty_Avg_Salary) %>%
    tidyr::pivot_wider(names_from = YearsAgo, values_from = Faculty_Avg_Salary, names_glue = "Faculty_Avg_Salary_Y{YearsAgo}Ago")
  salarymap <- salarymap %>% dplyr::select(-dplyr::any_of(c("Faculty_Avg_Salary_Y1Ago", "Faculty_Avg_Salary_Y2Ago"))) %>%
    dplyr::left_join(wide, by = "Name")
  utils::write.csv(salarymap, path, row.names = FALSE)
}

# The old block wrote Salary_Year quoted (text from the fetch); the enrollment refresh, next in both Rmds, reads and
# rewrites the file, which drops the quotes. So compare with the old block's file after one such rewrite.
rewrite_csv <- function(path) utils::write.csv(utils::read.csv(path, fileEncoding = "UTF-8"), path, row.names = FALSE)

test_that("refresh_ipeds_he_salaries writes what the old block's file is after the next refresh rewrites it", {
  f <- e_fixture(); withr::local_dir(f$dir)
  for (note in c(FALSE, TRUE)) {
    map <- data.frame(unitid = 1:3, Name = c("U1", "U2", "U3"))
    sal <- salary_rows()
    if (note) { map$Salary_Note <- c("shared", NA, NA); sal$Salary_Note <- map$Salary_Note }
    expected <- file.path(f$dir, "expected.csv")
    file.copy(file.path(f$app, "salarymap.csv"), expected, overwrite = TRUE)
    old_he_salary(expected, sal, trend_rows())
    rewrite_csv(expected)
    out <- refresh_ipeds_he_salaries(map, config = f$config, fetch = function(m) sal, fetch_trend = function(m) trend_rows())
    expect_identical(readLines(file.path(f$app, "salarymap.csv")), readLines(expected))
    expect_equal(out, sal)
  }
  log <- utils::read.csv(file.path(f$dir, "scrape_log.csv"))
  expect_identical(unique(log$source), c("IPEDS instructional staff salaries (MT)",
                                         "IPEDS instructional staff salary 3-year trend (MT)"))
  expect_true("Urban IPEDS" %in% read_source_retrieved(f$app)$Source)
})

test_that("a failed salary fetch leaves the file and skips the trend; a failed trend keeps last run's trend", {
  f <- e_fixture(); withr::local_dir(f$dir)
  map <- data.frame(unitid = 1:3, Name = c("U1", "U2", "U3"))
  before <- readLines(file.path(f$app, "salarymap.csv"))
  asked <- FALSE
  expect_message(refresh_ipeds_he_salaries(map, config = f$config, fetch = function(m) stop("524"),
                                           fetch_trend = function(m) { asked <<- TRUE; trend_rows() }), "untouched")
  expect_false(asked)
  expect_identical(readLines(file.path(f$app, "salarymap.csv")), before)
  refresh_ipeds_he_salaries(map, config = f$config, fetch = function(m) salary_rows(), fetch_trend = function(m) stop("524"))
  d <- utils::read.csv(file.path(f$app, "salarymap.csv"))
  expect_equal(d$Faculty_Avg_Salary[1:2], c(88123.4567, 70111.5))
  expect_equal(d$Faculty_Avg_Salary_Y1Ago, c(5, 5, 5))
  expect_identical(d$Salary_Source[3], NA_character_)
})

test_that("the fetches get the config's FIPS, not the session's", {
  f <- e_fixture(); withr::local_dir(f$dir)
  seen <- character(0)
  local_mocked_bindings(
    fetch_ipeds_he_salaries = function(unitid_map, fips) { seen <<- c(seen, fips); salary_rows() },
    fetch_ipeds_he_salary_trend = function(unitid_map, fips, n_years) { seen <<- c(seen, fips); trend_rows() },
    fetch_ccd_teacher_fte = function(district_ids, fips) {
      seen <<- c(seen, fips)
      data.frame(District = "A SD", Teachers_Total_FTE = 1, Enrollment = 1, CCD_Year = "2024")
    })
  refresh_ipeds_he_salaries(data.frame(unitid = 1, Name = "U1"), config = f$config)
  refresh_ccd_teacher_fte(list("A SD" = "3000001"), config = f$config)
  expect_identical(seen, c("30", "30", "30"))
})

test_that("refresh_ccd_teacher_fte writes what the old block wrote, and keeps the file on a failure", {
  f <- e_fixture(); withr::local_dir(f$dir)
  fte <- data.frame(District = c("A SD", "B SD"), Teachers_Total_FTE = c(12.25, 3), Enrollment = c(150, 40),
                    CCD_Year = "2024", stringsAsFactors = FALSE)
  expected <- utils::read.csv(file.path(f$app, "salarymap2.csv"), fileEncoding = "UTF-8") %>%
    dplyr::select(-dplyr::any_of(c("Teachers_Total_FTE", "Enrollment", "CCD_Year", "CCD_Source"))) %>%
    dplyr::left_join(fte, by = "District") %>%
    dplyr::mutate(CCD_Source = "NCES Common Core of Data via Urban Institute Education Data Portal (educationdata.urban.org)")
  utils::write.csv(expected, file.path(f$dir, "expected.csv"), row.names = FALSE)
  refresh_ccd_teacher_fte(list(), config = f$config, fetch = function(ids) fte)
  expect_identical(readLines(file.path(f$app, "salarymap2.csv")), readLines(file.path(f$dir, "expected.csv")))
  before <- readLines(file.path(f$app, "salarymap2.csv"))
  expect_message(refresh_ccd_teacher_fte(list(), config = f$config, fetch = function(ids) stop("x")), "untouched")
  expect_identical(readLines(file.path(f$app, "salarymap2.csv")), before)
})

test_that("refresh_data_coverage writes what the old block wrote", {
  f <- e_fixture(); withr::local_dir(f$dir)
  tiers <- data.frame(District = "B SD", Data_Coverage = "Partial", stringsAsFactors = FALSE)
  expected <- utils::read.csv(file.path(f$app, "salarymap2.csv"), fileEncoding = "UTF-8") %>%
    dplyr::mutate(Other = toupper(Other)) %>%
    dplyr::select(-dplyr::any_of("Data_Coverage")) %>% dplyr::left_join(tiers, by = "District") %>%
    dplyr::mutate(Data_Coverage = dplyr::coalesce(Data_Coverage, "Full"))
  utils::write.csv(expected, file.path(f$dir, "expected.csv"), row.names = FALSE)
  refresh_data_coverage(tiers, config = f$config, before = function(d) dplyr::mutate(d, Other = toupper(Other)))
  expect_identical(readLines(file.path(f$app, "salarymap2.csv")), readLines(file.path(f$dir, "expected.csv")))
  expect_error(refresh_data_coverage(tiers["District"], config = f$config))
})

oews_rows <- function() data.frame(Area_Code = "0000030", SOC = "25-2021", Year = "2025", Median = 50000)

test_that("refresh_pay_benchmarks passes the state, keeps the files on errors, and reads the codes as text", {
  f <- e_fixture(); withr::local_dir(f$dir)
  utils::write.csv(oews_rows(), file.path(f$app, "oews_role_wages.csv"), row.names = FALSE)
  got <- list()
  out <- refresh_pay_benchmarks(f$config,
                                refresh_oews = function(state, path, retrieved_source) got$oews <<- list(state, basename(path), retrieved_source),
                                fetch_qcew = function(fips) { got$qcew <<- fips; stop("503") })
  expect_identical(got$oews, list("MT", "oews_role_wages.csv", "BLS OEWS"))
  expect_identical(got$qcew, "30")
  expect_identical(out$Area_Code, "0000030")
  expect_type(out$Year, "character")
  expect_false(file.exists(file.path(f$app, "qcew_county_school_pay.csv")))
  log <- utils::read.csv(file.path(f$dir, "scrape_log.csv"))
  expect_identical(log$status, "error")
  expect_match(as.character(log$error_message), "503")
  # A failed OEWS check is logged and the file on file is still returned; the QCEW refresh still runs.
  got <- list()
  out <- refresh_pay_benchmarks(f$config, refresh_oews = function(...) stop("down"), fetch_qcew = function(fips) { got$q <<- fips; stop("x") })
  expect_identical(out$SOC, "25-2021"); expect_identical(got$q, "30")
  # A state base R doesn't list goes by its FIPS code.
  toy <- state_config("Toyland", "99", app_dir = f$app)
  refresh_pay_benchmarks(toy, refresh_oews = function(state, path, retrieved_source) got$toy <<- state, fetch_qcew = function(fips) stop("x"))
  expect_identical(got$toy, "99")
})

test_that("refresh_ipeds_staff_pay asks for the file's colleges and keeps the file on an error", {
  f <- e_fixture(); withr::local_dir(f$dir)
  got <- NULL
  refresh_ipeds_staff_pay(config = f$config, fetch = function(fips, unitids) { got <<- list(fips, as.integer(unitids)); stop("524") })
  expect_identical(got, list(30L, c(240727L, 240505L)))
  expect_false(file.exists(file.path(f$app, "ipeds_staff_salaries.csv")))
  expect_identical(utils::read.csv(file.path(f$dir, "scrape_log.csv"))$status, "error")
})

test_that("the slice E stages need a state config", {
  expect_error(refresh_pay_benchmarks(NULL), "no state config")
  expect_error(refresh_ipeds_staff_pay(config = NULL), "no state config")
  expect_error(refresh_ipeds_he_salaries(data.frame(), config = NULL), "no state config")
  expect_error(refresh_ccd_teacher_fte(list(), config = NULL), "no state config")
  expect_error(refresh_data_coverage(data.frame(District = "a", Data_Coverage = "b"), config = NULL), "no state config")
})
