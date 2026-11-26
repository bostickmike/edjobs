# K-12 places from the districts' CCD towns (R/k12_location.R). Fixtures: six districts of CCD's 2023 directories
# (Urban Institute API, read 2026-11-26), chosen for the cases: a district whose town a statewide feed misnames, one
# whose feed town isn't a town, a "Ft." / "Fort" spelling, an office in another town than every school, one-town and
# several-town districts.

towns_fixture <- function() {
  ccd_district_towns(utils::read.csv(test_path("fixtures", "ccd_district_towns_schools_2023.csv")),
                     utils::read.csv(test_path("fixtures", "ccd_district_towns_districts_2023.csv")), 2023)
}
ids <- c(Laramie1 = "5601980", Carbon2 = "5601700", Fremont21 = "5602820", Lincoln1 = "5604030",
         Weston1 = "5604830", Platte1 = "5605090")

test_that("a district's towns: its office and every school's, from CCD", {
  t <- towns_fixture()
  expect_equal(names(t), c("NCES_LEAID", "Office_Town", "School_Towns", "CCD_Year"))
  expect_equal(nrow(t), 6)
  row <- function(id) t[t$NCES_LEAID == id, ]
  expect_equal(row("5601980")[c("Office_Town", "School_Towns")],
               data.frame(Office_Town = "Cheyenne", School_Towns = "Cheyenne; Granite Canyon; Horse Creek"), ignore_attr = TRUE)
  expect_equal(row("5604030")$Office_Town, "Diamondville")
  expect_equal(row("5604030")$School_Towns, "Kemmerer")
  expect_equal(unique(t$CCD_Year), "2023")
  expect_null(reference_table_problem(t, reference_rules_for("district_towns.csv")))
})

test_that("a feed town that is none of the district's becomes its office town; one that is stays", {
  df <- data.frame(District = c("Laramie1", "Carbon2", "Carbon2", "Fremont21", "Lincoln1", "Platte1", "STRIDE"),
                   location = c("Laramie", "CCSD#2", "Saratoga", "Fort Washakie", "Kemmerer", "Glendo", "Cheyenne"),
                   stringsAsFactors = FALSE)
  out <- fill_k12_locations(df, towns_fixture(), ids, feed_rows = rep(TRUE, 7))
  expect_equal(out$location, c("Cheyenne", "Saratoga", "Saratoga", "Fort Washakie", "Kemmerer", "Glendo", "Cheyenne"))
  expect_equal(attr(out, "location_changes"), c(filled = 0L, corrected = 2L))
})

test_that("a place a board gives is kept, even when it is a building and not a town", {
  df <- data.frame(District = c("Laramie1", "Weston1"), location = c("Hebard Elementary", "Central Office"))
  out <- fill_k12_locations(df, towns_fixture(), ids)
  expect_equal(out$location, c("Hebard Elementary", "Central Office"))
  expect_equal(attr(out, "location_changes"), c(filled = 0L, corrected = 0L))
})

test_that("a row with no place takes the district's town only when all its schools are in one", {
  df <- data.frame(District = c("Weston1", "Lincoln1", "Fremont21", "Platte1", "Carbon2", "STRIDE", "Weston1"),
                   location = c(NA, "", NA, NA, NA, NA, "  "), stringsAsFactors = FALSE)
  out <- fill_k12_locations(df, towns_fixture(), ids, feed_rows = c(TRUE, rep(FALSE, 6)))
  # Lincoln 1's office is in Diamondville, every school in Kemmerer: the school town.
  expect_equal(out$location, c("Newcastle", "Kemmerer", "Ft. Washakie", NA, NA, NA, "Newcastle"))
  expect_equal(attr(out, "location_changes"), c(filled = 4L, corrected = 0L))
})

test_that("no towns table, or no rows, leaves the rows as they are", {
  df <- data.frame(District = "Weston1", location = NA_character_)
  expect_equal(fill_k12_locations(df, NULL, ids)$location, NA_character_)
  expect_equal(nrow(fill_k12_locations(df[0, ], towns_fixture(), ids)), 0)
  expect_error(fill_k12_locations(df, towns_fixture(), ids, feed_rows = c(TRUE, FALSE)))
})

test_that("the refresh stage writes the table and keeps the committed one when the fetch fails", {
  fixture <- towns_fixture()
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  config <- state_config(state = "Test", fips = "99", app_dir = "app")
  dir.create("app")
  t <- refresh_district_towns(config, fetch = function() fixture)
  expect_equal(nrow(t), 6)
  expect_true(file.exists("app/district_towns.csv"))
  kept <- refresh_district_towns(config, fetch = function() stop("CCD down"))
  expect_equal(kept, t)
  expect_true(any(grepl("district towns", utils::read.csv("scrape_log.csv")$source)))
})
