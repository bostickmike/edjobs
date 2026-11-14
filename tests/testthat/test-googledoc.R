rd_fixture <- function(name) paste(readLines(test_path("fixtures", name), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

test_that("an uploaded doc's preview text reads through the filter (Victor)", {
  text <- googledoc_preview_text(rd_fixture("googledoc_victor_preview_2026-11-14.html"))
  expect_match(text, "Certified Staff Openings:", fixed = TRUE)
  r <- parse_googledoc_postings(text, "Victor", "https://www.victor.k12.mt.us/page/employment")
  # "3 Paraprofessional positions" is the kind; "Athletics/Coaching" is a header; the lowercase substitute list is
  # one standing posting
  expect_equal(r$Title, c("HS/MS Special Education Teacher", "Paraprofessional", "Head High School Track Coach", "Substitutes"))
  expect_true(all(r$Link == "https://www.victor.k12.mt.us/page/employment"))
  expect_true(all(r$Location == "Victor"))
  expect_true("Pay_Min" %in% names(r))
})

test_that("two docs, each with the same list rules (Bainville's certified and classified docs)", {
  texts <- strsplit(rd_fixture("googledoc_bainville_2026-11-14.txt"), "\n=====\n", fixed = TRUE)[[1]]
  expect_length(texts, 2)
  r <- parse_googledoc_postings(texts, "Bainville", "https://www.bainvilleschool.com/page/employment",
                                "^(CERTIFIED JOB OPENINGS|Current Openings)$", "^(Bainville School is a small|For a complete application)")
  expect_equal(r$Title, c("K-12 Art Teacher", "7-12 History", "Elementary Teacher", "Paraprofessional",
                          "Substitute Teachers", "Custodian Subs", "Substitute Bus Drivers"))
})

test_that("ids from a registry Slug; empty input gives zero rows with the columns", {
  expect_equal(googledoc_ids("a; b;"), c("a", "b"))
  expect_equal(googledoc_ids(NA), character(0))
  expect_error(fetch_googledoc_postings(NA, "x", "u"), "no doc id")
  r <- parse_googledoc_postings(c(NA, ""), "x", "u")
  expect_equal(nrow(r), 0)
  expect_true(all(c("Title", "Location", "Posted_Date", "Link") %in% names(r)))
  expect_equal(googledoc_preview_text(""), "")
})
