# read_k12_registry() / read_he_registry(): the fixtures are Montana's and Wyoming's real registries
# (registry_{mt,wy}_{k12,he}.csv, copied from each state's main on 2026-11-28: MT 070e17d, WY 1a65168).

fx <- function(f) test_path("fixtures", f)

test_that("Montana's K-12 registry: the values a plain read.csv() gives, with LEAIDs as 7-digit text", {
  r <- read_k12_registry(fx("registry_mt_k12.csv"))
  plain <- utils::read.csv(fx("registry_mt_k12.csv"), stringsAsFactors = FALSE)
  expect_equal(nrow(r), nrow(plain))
  expect_true(all(K12_REGISTRY_COLUMNS %in% names(r)))
  expect_type(r$NCES_LEAID, "character")
  expect_true(all(nchar(r$NCES_LEAID) == 7))
  expect_equal(as.integer(r$NCES_LEAID), plain$NCES_LEAID)
  expect_equal(r$Latitude, plain$Latitude)
  expect_equal(r$Longitude, plain$Longitude)
  expect_equal(r$Posting_Source, plain$Posting_Source)                       # a state's own column comes through
  text_cols <- setdiff(names(plain), c("NCES_LEAID", "Latitude", "Longitude"))
  expect_equal(r[text_cols], plain[text_cols])                               # blanks stay "", as before
})

test_that("Wyoming's K-12 registry: what WY's own reader gave, except an all-blank City stays text", {
  r <- read_k12_registry(fx("registry_wy_k12.csv"))
  wy <- utils::read.csv(fx("registry_wy_k12.csv"), stringsAsFactors = FALSE,
                        colClasses = c(NCES_LEAID = "character", Platform_Key = "character"))
  expect_equal(r[setdiff(names(wy), "City")], wy[setdiff(names(wy), "City")])
  expect_true(is.logical(wy$City) && all(is.na(wy$City)))                    # the plain read: logical NA
  expect_identical(unique(r$City), "")
  expect_identical(r$NCES_LEAID[r$District == "Laramie Montessori Charter School"], "")   # outside CCD: no id
})

test_that("a LEAID read as a number gets its leading zero back", {
  f <- tempfile(fileext = ".csv")
  r <- utils::read.csv(fx("registry_wy_k12.csv"), colClasses = "character")[1:2, ]
  r$NCES_LEAID <- c("800001", "0800002")                                    # FIPS 08: one written without its zero
  utils::write.csv(r, f, row.names = FALSE)
  expect_equal(read_k12_registry(f)$NCES_LEAID, c("0800001", "0800002"))
})

test_that("a registry lacking schema columns, or with a coordinate that isn't a number, stops", {
  f <- tempfile(fileext = ".csv")
  r <- utils::read.csv(fx("registry_wy_k12.csv"), colClasses = "character")
  utils::write.csv(r[setdiff(names(r), c("Platform_Key", "List_Skip"))], f, row.names = FALSE)
  expect_error(read_k12_registry(f), "lacks column\\(s\\): Platform_Key, List_Skip")
  r$Latitude[3] <- "north"
  utils::write.csv(r, f, row.names = FALSE)
  expect_error(read_k12_registry(f), "Latitude is not a number in row\\(s\\) 3")
  expect_error(read_k12_registry(tempfile()), "no file at")
})

test_that("college registries: both states' files, all-blank columns as text", {
  for (st in c("mt", "wy")) {
    f <- fx(sprintf("registry_%s_he.csv", st))
    r <- read_he_registry(f)
    plain <- utils::read.csv(f, stringsAsFactors = FALSE)
    expect_true(all(HE_REGISTRY_COLUMNS %in% names(r)))
    expect_equal(r$Latitude, plain$Latitude)
    expect_type(r$Location_Pattern, "character")
  }
  expect_true(is.logical(utils::read.csv(fx("registry_wy_he.csv"))$Location_Pattern))   # WY's is blank in every row
  expect_error(read_he_registry(fx("registry_wy_k12.csv")), "lacks column\\(s\\): Institution, Feed_URL")
})

test_that("scraper_registry() given a path reads it the same way", {
  src <- scraper_registry(fx("registry_wy_k12.csv"), "District", "k12_district_registry.csv")
  t <- scraper_source_table(src)
  expect_type(t$NCES_LEAID, "character")
  expect_type(t$City, "character")
  expect_type(t$Latitude, "double")
})

test_that("a relative path is found in a parent directory, as from a state's tests/testthat", {
  root <- withr::local_tempdir()
  file.copy(fx("registry_wy_k12.csv"), file.path(root, "k12_district_registry.csv"))
  file.copy(fx("registry_wy_he.csv"), file.path(root, "he_institution_registry.csv"))
  dir.create(file.path(root, "tests", "testthat"), recursive = TRUE)
  withr::with_dir(file.path(root, "tests", "testthat"), {
    expect_equal(nrow(read_k12_registry()), nrow(read_k12_registry(file.path(root, "k12_district_registry.csv"))))
    expect_equal(nrow(read_he_registry()), 9)
    expect_error(read_k12_registry("nowhere.csv"), "no file at nowhere.csv")
  })
})
