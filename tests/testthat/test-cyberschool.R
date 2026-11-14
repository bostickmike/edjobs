rd_fixture <- function(name) paste(readLines(test_path("fixtures", name), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
cols <- c("Title", "Location", "Posted_Date", "Closing_Date", "Link")

test_that("classic CyberSchool: Highwood's four postings, dates as ISO", {
  r <- parse_cyberschool_postings(rd_fixture("cyberschool_highwood_2026-10-21.html"),
                                  "https://www.highwood.k12.mt.us/District/Jobs/", "Highwood")
  expect_equal(names(r), cols)
  expect_equal(r$Title, c("Route or Substitute Expedition Driver", "Route, Activity or Substitute Bus Drivers",
                          "Janitorial", "Substitute Teachers"))
  expect_true(all(r$Location == "Highwood"))
  expect_equal(r$Posted_Date, c("2026-08-14", "2026-08-14", "2026-04-23", "2025-08-18"))
  # "Deadline: Until position is filled." is not a date.
  expect_true(all(is.na(r$Closing_Date)))
  expect_equal(r$Link[1], "https://www.highwood.k12.mt.us/District/Jobs/359-Route-or-Substitute-Expedition-Driver.html")
})

test_that("classic CyberSchool: Power and Vaughn (same postings as their old scrapers, now with dates)", {
  p <- parse_cyberschool_postings(rd_fixture("cyberschool_power_2026-10-22.html"), "https://power.k12.mt.us/District/Jobs/", "Power")
  expect_setequal(p$Title, c("Substitute Teachers", "Substitute After-School Custodians", "Substitute Kitchen Aides"))
  expect_true(all(grepl("^https://power\\.k12\\.mt\\.us/District/Jobs/[0-9]+-", p$Link)))
  expect_true(all(p$Posted_Date == "2023-03-01"))

  v <- parse_cyberschool_postings(rd_fixture("cyberschool_vaughn_2026-10-22.html"), "https://www.vaughnschool.com/District/Jobs/", "Vaughn")
  expect_equal(v$Title, "Substitutes Needed")
  expect_equal(v$Link, "https://www.vaughnschool.com/District/Jobs/497-Substitutes-Needed.html")
  expect_equal(v$Posted_Date, "2026-09-09")
})

test_that("newer IES platform: Vaughn after its move (2026-11)", {
  v <- parse_cyberschool_postings(rd_fixture("cyberschool_vaughn_ies_2026-11-14.html"), "https://www.vaughnschool.com/jobs", "Vaughn")
  expect_equal(names(v), cols)
  # One row: the "See Description" button links to the same page.
  expect_equal(v$Title, "Substitutes Needed")
  expect_equal(v$Link, "https://vaughn.l.iescentral.com/jobs/substitutes-needed")
  expect_equal(v$Posted_Date, "2026-09-09")
})

test_that("an empty board and non-CyberSchool HTML give zero rows with the columns", {
  e <- parse_cyberschool_postings(rd_fixture("cyberschool_frontier_empty_2026-11-14.html"), "https://www.frontierk8.org/District/Jobs/", "Wolf Point")
  expect_equal(nrow(e), 0)
  expect_equal(names(e), cols)
  for (h in list("", NULL, "<html><body><p>No jobs</p></body></html>")) {
    z <- parse_cyberschool_postings(h, "https://x.org", "X")
    expect_equal(nrow(z), 0)
    expect_equal(names(z), cols)
  }
})

test_that("cyberschool_date reads the platform's date forms, NA otherwise", {
  expect_equal(cyberschool_date(c("Friday, Aug 14, 2026 12:00:00 AM", "09/09/2026", "November 20, 2026", "2026-11-20",
                                  "Until position is filled.", "Feb 30, 2026", NA)),
               c("2026-08-14", "2026-09-09", "2026-11-20", "2026-11-20", NA, NA, NA))
})

test_that("fetch_cyberschool_postings follows a redirect and resolves links against the served page", {
  html <- rd_fixture("cyberschool_highwood_2026-10-21.html")
  httr2::local_mocked_responses(function(req) {
    httr2::response(200, url = "https://www.highwood.k12.mt.us/District/Jobs/",
                    headers = list("Content-Type" = "text/html; charset=utf-8"), body = charToRaw(html))
  })
  r <- fetch_cyberschool_postings("https://www.highwood.k12.mt.us/District/Jobs/", "Highwood")
  expect_equal(nrow(r), 4)
  expect_true(all(grepl("^https://www.highwood.k12.mt.us/District/Jobs/", r$Link)))
})
