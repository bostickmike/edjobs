places <- data.frame(
  Town = c("Riverton", "Jackson", "Lander", "Fort Washakie", "Laramie", "Cheyenne", "Casper", "Roberts",
           "Mountain View", "Mountain View", "Libby", "Kalispell"),
  County = c("Fremont County", "Teton County", "Fremont County", "Fremont County", "Albany County",
             "Laramie County", "Natrona County", "Carbon County", "Uinta County", "Crook County", "Lincoln County",
             "Flathead County"),
  Latitude = c(43.03, 43.47, 42.83, 43.0, 41.31, 41.14, 42.85, 45.36, 41.27, 44.2, 48.39, 48.2),
  Longitude = c(-108.38, -110.78, -108.73, -108.88, -105.61, -104.8, -106.3, -109.17, -110.34, -104.3, -115.56, -114.3),
  stringsAsFactors = FALSE)

test_that("location_town reads only a written place", {
  loc <- c("Riverton", "Jackson, WY", "Laramie, WY (Laramie Campus)", "US-WY-Casper", "Wind River - Ft. Washakie",
           "1115 N Roberts St", "United States", "WY, United States", "VP of Academic Services", NA, "",
           "Mountain View, WY", "CHEYENNE, WY 82007", "Rock Springs & Evanston")
  expect_equal(location_town(loc, places, "WY"),
               c("Riverton", "Jackson", "Laramie", "Casper", "Fort Washakie", NA, NA, NA, NA, NA, NA, NA, "Cheyenne", NA))
})

test_that("location_out_of_state finds another state's abbreviation", {
  expect_equal(location_out_of_state(c("Denver, CO, United States", "US-HI-Honolulu", "St. Clair County, IL",
                                       "Laramie, WY", "COLORADO STATE", NA), "WY"),
               c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
})

test_that("place_he_postings keeps the main campus and unplaceable postings at the main point", {
  reg <- data.frame(Institution = c("Central Wyoming College", "University of Wyoming", "No Town College"),
                    Town = c("Riverton", "Laramie", ""), stringsAsFactors = FALSE)
  p <- data.frame(Institution = c("Central Wyoming College", "Central Wyoming College", "University of Wyoming",
                                  "University of Wyoming", "University of Wyoming", "No Town College", "Unknown"),
                  Location = c("Riverton", "Jackson", "Casper, WY", "Denver, CO, United States", "United States",
                               "Jackson", "Lander"),
                  stringsAsFactors = FALSE)
  out <- place_he_postings(p, reg, places, "WY")
  expect_equal(out$Place_Basis, c("main", "other_town", "other_town", "out_of_state", "no_town", "main", "main"))
  expect_equal(out$Campus_Town, c(NA, "Jackson", "Casper", NA, NA, NA, NA))
  expect_equal(out$Campus_County[2:3], c("Teton County", "Natrona County"))
  expect_equal(out$Campus_Latitude[2], 43.47)
  expect_true(all(is.na(out$Campus_Longitude[-(2:3)])))
})

test_that("read_state_places requires the place columns", {
  f <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(places, f, row.names = FALSE)
  expect_equal(nrow(read_state_places(f)), nrow(places))
  utils::write.csv(places[, c("Town", "County")], f, row.names = FALSE)
  expect_error(read_state_places(f))
})
