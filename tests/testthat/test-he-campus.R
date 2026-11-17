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

test_that("HE_PLACE_COLUMNS names exactly the columns place_he_postings adds", {
  reg <- data.frame(Institution = "A", Town = "Riverton")
  p <- data.frame(Institution = "A", Location = "Jackson")
  expect_setequal(setdiff(names(place_he_postings(p, reg, places, "WY")), names(p)), HE_PLACE_COLUMNS)
})

test_that("title_place_names reads only a marker", {
  expect_equal(title_place_names(c("Livestock and Farm Assistant Manager *Havre, MT*", "Administrative Associate III (Conrad, MT)",
                                   "Research Associate – Precision Agriculture **Havre, MT**", "Instructor (Rock Springs Campus)",
                                   "Director of Workforce Development and the Green River Center", NA)),
               list("Havre", "Conrad", "Havre", "Rock Springs", character(0), character(0)))
})

pa_page <- function(name) paste(readLines(test_path("fixtures", paste0("peopleadmin_posting_", name, "_2026-11-16.html")),
                                          warn = FALSE, encoding = "UTF-8"), collapse = "\n")

test_that("peopleadmin_page_places reads explicit phrases, not boilerplate or checkboxes", {
  expect_equal(peopleadmin_page_places(pa_page("msu_53598")), "Joliet")         # Carbon County agent; seat is Red Lodge
  expect_equal(peopleadmin_page_places(pa_page("msu_53614")), "Plentywood")     # "The Extension office is located in"
  expect_equal(peopleadmin_page_places(pa_page("msu_53666")), "Miles City")
  expect_equal(peopleadmin_page_places(pa_page("sheridan_4737")), "Wright")     # not "Gillette College is located in Gillette"
  expect_equal(peopleadmin_page_places(pa_page("western_6074")), "Green River")
  expect_equal(peopleadmin_page_places(pa_page("eastern_1989")), "Douglas")
  w <- strsplit(peopleadmin_page_places(pa_page("western_5258")), "; ")[[1]]
  expect_setequal(w, c("Green River", "main campus"))
  expect_false(grepl("Billings|Missoula|Kalispell", coalesce_chr(peopleadmin_page_places(pa_page("msu_52040")))))  # checkboxes
  expect_true(is.na(peopleadmin_page_places(NA_character_)))
})

test_that("place_he_postings uses title, department table and page places; several towns stay at the main campus", {
  pl <- rbind(places, data.frame(Town = c("Bozeman", "Havre", "Sidney", "Joliet", "Rock Springs", "Green River"),
                                 County = c("Gallatin County", "Hill County", "Richland County", "Carbon County",
                                            "Sweetwater County", "Sweetwater County"),
                                 Latitude = 1:6, Longitude = -(1:6), stringsAsFactors = FALSE))
  reg <- data.frame(Institution = c("Montana State University", "Western Wyoming Community College"),
                    Town = c("Bozeman", "Rock Springs"), stringsAsFactors = FALSE)
  dep <- data.frame(Institution = "Montana State University", Location = c("AES EARC", "AES NARC"),
                    Town = c("Sidney", "Havre"), stringsAsFactors = FALSE)
  p <- data.frame(Institution = c(rep("Montana State University", 5), rep("Western Wyoming Community College", 3)),
                  Location = c("AES EARC", "AES NARC", "Extension Agents/Field", "Physics", "Extension Agents/Field",
                               rep("Western Wyoming Community College", 3)),
                  Title = c("Research Assistant II", "Agronomist *Havre, MT*", "Extension Agent - Carbon County",
                            "Lecturer", "Extension Agent - Two Counties", "Instructor", "Adjunct", "Aide (Rock Springs Campus)"),
                  Page_Places = c(NA, NA, "Joliet", NA, "Joliet; Sidney", "Green River", "Green River; main campus", NA),
                  stringsAsFactors = FALSE)
  out <- place_he_postings(p, reg, pl, "MT", campus_departments = dep)
  expect_equal(out$Campus_Town, c("Sidney", "Havre", "Joliet", NA, NA, "Green River", NA, NA))
  expect_equal(out$Place_Basis, c("other_town", "other_town", "other_town", "no_town", "no_town", "other_town", "no_town", "main"))
  expect_false("Page_Places" %in% names(out))
  expect_setequal(setdiff(names(out), names(p)), HE_PLACE_COLUMNS)
})

test_that("a town marked with another state's name isn't placed in this state (2026-11-17 review)", {
  title <- c("Extension Agent (Big Horn, MT)", "Extension Agent (Big Horn, WY)", "Instructor (Rock Springs Campus)")
  expect_equal(title_place_names(title, "WY"), list(character(0), "Big Horn", "Rock Springs"))
  expect_equal(title_place_names(title, "MT"), list("Big Horn", character(0), "Rock Springs"))
  expect_equal(title_place_names("Agent (Boise, Idaho)", "ID"), list("Boise"))
  expect_equal(title_place_names("Agent (Boise, Idaho)", "WY"), list(character(0)))
  # With no state given and no state config, any US state's name or abbreviation counts.
  old <- current_state_config(); set_state_config(NULL); on.exit(set_state_config(old))
  expect_equal(title_place_names(title[1:2]), list("Big Horn", "Big Horn"))
})
