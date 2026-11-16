# District staffing (R/ccd.R) and SAIPE child poverty (R/saipe.R), matched by
# NCES id. Fixtures: real CCD 2024 district directory rows (MT: towns used by
# the crosswalk tests; WY: the whole state) and real SAIPE 2024 responses with
# the count fields (MT: unified, elementary, secondary; WY: unified,
# elementary; captured 2026-11-15).

saipe_fixture <- function(st, geo) {
  f <- test_path("fixtures", sprintf("saipe_counts_%s_%s_2024.json", st, geo))
  if (!file.exists(f)) return(data.frame())   # WY has no secondary districts (the API answers 204)
  parse_saipe_json(paste(readLines(f, warn = FALSE), collapse = "\n"))
}
mt_saipe <- function() {
  out <- saipe_rows_by_leaid(stats::setNames(lapply(SAIPE_GEOGRAPHIES, function(g) saipe_fixture("mt", g)), SAIPE_GEOGRAPHIES), "30")
  out$SAIPE_Year <- 2024L
  out
}

# ---- CCD ---------------------------------------------------------------------

test_that("parse_ccd_teacher_fte sums an elementary + high school pair by LEAID (Billings)", {
  df <- read.csv(test_path("fixtures", "ccd_mt_directory_2024_crosswalk_towns.csv"), colClasses = c(leaid = "character"))
  r <- parse_ccd_teacher_fte(df, list("Billings Public Schools" = c("3003870", "3003900"),
                                      "Swan River School District" = "3025500"))
  expect_equal(r$Teachers_Total_FTE, c(696 + 328, 13))
  expect_equal(r$Enrollment, c(11169 + 5444, 158))
})

test_that("parse_ccd_teacher_fte leaves out non-district agencies and gives NA for an id with no record", {
  df <- read.csv(test_path("fixtures", "ccd_mt_directory_2024_crosswalk_towns.csv"), colClasses = c(leaid = "character"))
  # 3000020 is Big Country Coop (agency_type 4), 3000019 a county supervisory record with no schools.
  r <- parse_ccd_teacher_fte(df, list(Coop = "3000020", County = "3000019", Missing = "3099999"))
  expect_true(all(is.na(r$Teachers_Total_FTE)))
  expect_true(all(is.na(r$Enrollment)))
  expect_equal(r$District, c("Coop", "County", "Missing"))
})

test_that("parse_ccd_teacher_fte gives WY's 48 districts the figures the old name match gave", {
  df <- read.csv(test_path("fixtures", "ccd_wy_directory_2024.csv"), colClasses = c(leaid = "character"))
  real <- df[df$agency_type == 1 & !is.na(df$number_of_schools) & df$number_of_schools >= 1, ]
  expect_equal(nrow(real), 48)
  ids <- stats::setNames(as.list(real$leaid), real$lea_name)
  r <- parse_ccd_teacher_fte(df, ids)
  albany <- r[r$District == "Albany County School District #1", ]
  expect_equal(albany$Teachers_Total_FTE, 316)
  expect_equal(albany$Enrollment, 3752)
  expect_equal(r$Teachers_Total_FTE[r$District == "Laramie County School District #1"], 1067)
})

test_that("parse_ccd_teacher_fte returns an empty, correctly-shaped frame when given no rows", {
  r <- parse_ccd_teacher_fte(data.frame(), list(A = "1"))
  expect_equal(nrow(r), 0)
  expect_equal(names(r), c("District", "Teachers_Total_FTE", "Enrollment"))
})

test_that("fetch_ccd_teacher_fte takes the latest directory year for the state and adds CCD_Year", {
  urls <- character(0)
  local_mocked_bindings(fetch_json_paginated = function(url, ...) {
    urls <<- c(urls, url)
    if (grepl("/2024/", url)) read.csv(test_path("fixtures", "ccd_mt_directory_2024_crosswalk_towns.csv"), colClasses = c(leaid = "character")) else data.frame()
  })
  r <- fetch_ccd_teacher_fte(list(Billings = c("3003870", "3003900")), fips = "30")
  expect_equal(r$CCD_Year, "2024")
  expect_true(all(grepl("school-districts/ccd/directory/[0-9]{4}/\\?fips=30$", urls)))
})

# ---- SAIPE -------------------------------------------------------------------

test_that("saipe_rows_by_leaid keys every geography by LEAID and keeps the counts", {
  s <- mt_saipe()
  expect_equal(nrow(s), 64 + 235 + 98)
  b <- s[s$LEAID %in% c("3003870", "3003900"), ]
  expect_equal(b$SD_NAME, c("Billings Elementary School District", "Billings High School District"))
  expect_equal(b$Child_Poverty_Rate, c(0.089, 0.061))
  expect_equal(b$SAIPE_Poor_5_17, c(1216, 426))
  expect_equal(b$SAIPE_Children_5_17, c(13706, 7029))
})

test_that("a combined district's rate pools the counts, not an equal average of the rates (Billings, Butte)", {
  r <- parse_census_saipe_child_poverty(mt_saipe(), list(Billings = c("3003870", "3003900"), Butte = c("3005280", "3005310")))
  expect_equal(r$Child_Poverty_Rate[1], (1216 + 426) / (13706 + 7029))
  expect_false(isTRUE(all.equal(r$Child_Poverty_Rate[1], (0.089 + 0.061) / 2)))
  expect_equal(r$Child_Poverty_Rate[2], (440 + 166) / (3381 + 1786))
  expect_equal(r$SAIPE_Year, c(2024L, 2024L))
})

test_that("a single SAIPE district keeps its published rate; a missing one is NA", {
  r <- parse_census_saipe_child_poverty(mt_saipe(), list("East Helena" = "3000655", Lewistown_HS = "3010530", None = "3099999"))
  expect_equal(r$Child_Poverty_Rate, c(0.044, 0.104, NA))
  expect_equal(r$SAIPE_Year, c(2024L, 2024L, NA))   # no year without a rate
})

test_that("pool_child_poverty falls back to the mean of rates when no counts are known", {
  expect_equal(pool_child_poverty(data.frame(Child_Poverty_Rate = c(0.2, 0.1))), 0.15)
  expect_equal(pool_child_poverty(data.frame(Child_Poverty_Rate = c(0.2, NA), SAIPE_Poor_5_17 = c(20, NA),
                                             SAIPE_Children_5_17 = c(100, NA))), 0.2)
  expect_true(is.na(pool_child_poverty(data.frame(Child_Poverty_Rate = NA_real_))))
})

test_that("surrounding_district_poverty pools a split town's districts when the counts are there", {
  s <- mt_saipe()
  p <- surrounding_district_poverty(1, 1, s, c("3005280" = "Butte Elem", "3005310" = "Butte H S"),
                                    geocode = function(lat, lon) c("3005280", "3005310"))
  expect_equal(p$rate, (440 + 166) / (3381 + 1786))
  expect_equal(p$basis, "Surrounding district: Butte Elem / Butte H S")
  expect_equal(p$year, 2024L)
})

test_that("fetch_saipe_by_leaid reads all three geographies and treats WY's empty secondary (204) as no rows", {
  geos <- character(0)
  local_mocked_bindings(perform_with_retry = function(req, ...) {
    g <- sub(".*school%20district%20%28([a-z]+)%29.*", "\\1", req$url)
    geos <<- c(geos, g)
    f <- test_path("fixtures", sprintf("saipe_counts_wy_%s_2024.json", g))
    if (!file.exists(f)) return(httr2::response(204))
    httr2::response(200, body = readBin(f, "raw", file.size(f)))
  })
  s <- fetch_saipe_by_leaid(year = 2024, api_key = "k", fips = "56")
  expect_equal(geos, SAIPE_GEOGRAPHIES)
  expect_equal(nrow(s), 47 + 1)
  expect_true("Fremont County School District 38" %in% s$SD_NAME)   # SAIPE's one WY elementary district
  expect_true(all(substr(s$LEAID, 1, 2) == "56") && all(nchar(s$LEAID) == 7))
})

test_that("parse_saipe_json returns an empty data frame for an empty or header-only response", {
  expect_equal(nrow(parse_saipe_json("")), 0)
  expect_equal(nrow(parse_saipe_json('[["SD_NAME","SAEPOVRAT5_17RV_PT"]]')), 0)
})

# ---- SAIPE error range -----------------------------------------------------------

test_that("SAIPE_Population comes from SAEPOVALL_PT and a pair takes the larger district's (Billings, Butte)", {
  s <- mt_saipe()
  expect_equal(s$SAIPE_Population[s$LEAID == "3000655"], 10809)
  r <- parse_census_saipe_child_poverty(s, list(Billings = c("3003870", "3003900"), Butte = c("3005280", "3005310"),
                                                None = "3099999"))
  expect_equal(r$SAIPE_Population, c(139403, 36095, NA))
})

test_that("saipe_relative_error follows Census's bands, lower bound inclusive", {
  expect_equal(saipe_relative_error(c(0, 2499, 2500, 4999, 5000, 10000, 20000, 64999, 65000, 139403)),
               c(0.67, 0.67, 0.42, 0.42, 0.35, 0.28, 0.23, 0.23, 0.15, 0.15))
  expect_true(all(is.na(saipe_relative_error(c(NA, -1)))))
})

test_that("add_saipe_range gives rate x (1 -/+ 1.645 x CV), floored at 0 and capped at 1", {
  df <- add_saipe_range(data.frame(Child_Poverty_Rate = c(0.15, 0.15, 0.8, NA, 0.2),
                                   SAIPE_Population = c(1000, 70000, 1000, 1000, NA)))
  expect_equal(df$Child_Poverty_Low, c(0, 0.15 * (1 - 1.645 * 0.15), 0, NA, NA))
  expect_equal(df$Child_Poverty_High, c(0.15 * (1 + 1.645 * 0.67), 0.15 * (1 + 1.645 * 0.15), 1, NA, NA))
})

test_that("surrounding_district_poverty also returns the population behind its rate", {
  p <- surrounding_district_poverty(1, 1, mt_saipe(), geocode = function(lat, lon) c("3005280", "3005310"))
  expect_equal(p$population, 36095)
  none <- surrounding_district_poverty(1, 1, mt_saipe(), geocode = function(lat, lon) "3099999")
  expect_true(is.na(none$population))
})
