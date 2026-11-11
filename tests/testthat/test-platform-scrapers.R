# Shared tests moved from the MT and WY repos' test-direct-api-scrapers.R in phase 3 (2026-11-11). A block both
# states had identically appears once; a same-named block with different contents appears
# twice, the second marked with its state.


test_that("parse_applitrack_output un-escapes document.write literals and preserves the original position/position2 splitting logic", {
  # Real fixture captured from https://www.applitrack.com/bighorn/onlineapp/
  # jobpostings/Output.asp?all=1 -- the endpoint default.aspx's own inline
  # script injects into the page via document.write() rather than being
  # server-rendered directly.
  fixture <- paste(readLines(test_path("fixtures", "applitrack_output.txt"), warn = FALSE), collapse = "\n")

  result <- parse_applitrack_output(fixture)

  expect_equal(nrow(result), 3)
  expect_equal(result$title, c(
    "Life Skills Paraprofessional - Middle/High School",
    "Substitute Teacher",
    "Substitute Bus Driver / Activity Driver"
  ))
  # Regression: "Position Type:" consumes TWO values (category + subcategory)
  # only when the next value isn't itself another label's value -- getting
  # this wrong shifts every subsequent field (date_posted/location/
  # closing_date) by one position for every row after the first mismatch.
  expect_equal(result$position, c("Support Staff/", "Substitute/", "Transportation/"))
  expect_equal(result$position2, c("Para-Educator Special Services", "Substitute Teacher", "Substitute Bus Driver"))
  expect_equal(result$date_posted, c("8/2/2026", "7/5/2017", "12/17/2024"))
  expect_equal(result$closing_date, c("until filled", "Open During School Year", "Open During School Year"))
})

test_that("parse_applitrack_output returns zero rows (not an error) when there's no postingsList content", {
  result <- parse_applitrack_output("document.write('<div>No postings here</div>');")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("title", "position", "position2", "date_posted", "location", "closing_date", "job_id",
                                "Pay_Min", "Pay_Max", "Pay_Unit", "Pay_Text"))
})

test_that("fetch_applitrack_postings decodes Windows-1252 bytes instead of silently dropping them as NA", {
  # Regression: Applitrack serves Windows-1252 with no charset in
  # Content-Type. httr2 guesses UTF-8 by default; if a posting contains a
  # non-ASCII Windows-1252 byte (e.g. an en-dash, 0x96), UTF-8 decoding
  # fails and resp_body_string() returns NA rather than erroring -- which
  # parse_applitrack_output() then silently turns into zero rows,
  # indistinguishable from a district with genuinely no openings. Confirmed
  # live: 10 of 25 Applitrack districts were affected, one hiding 69 real
  # postings. This fixture is the real applitrack_output.txt fixture with
  # an en-dash injected into the first title and re-encoded as raw
  # Windows-1252 bytes, exactly mirroring what the live endpoint sends.
  raw_bytes <- readBin(
    test_path("fixtures", "applitrack_output_windows1252.bin"),
    "raw",
    file.info(test_path("fixtures", "applitrack_output_windows1252.bin"))$size
  )
  httr2::local_mocked_responses(
    list(httr2::response(200, headers = list("Content-Type" = "text/javascript"), body = raw_bytes))
  )

  result <- fetch_applitrack_postings("bighorn")

  expect_equal(nrow(result), 3)
  expect_equal(result$title[1], "Life Skills Para–professional - Middle/High School")
})

test_that("parse_applitrack_output reads each field from its own <li> (one-value Position Type)", {
  # Real fixture captured 2026-11-03 from Teton County SD1 (WY)
  # https://www.applitrack.com/tcsd/onlineapp/jobpostings/Output.asp?all=1 .
  # JobID 836 has a one-value "Position Type:" followed by "Date Posted:
  # 10/26/2026". The old running-index parser took the date as a second
  # Position Type value, so the posting got date_posted "TCSD Schools" and
  # location "10/26/2026".
  path <- test_path("fixtures", "applitrack_output_wy_teton_single_position_type.bin")
  js <- iconv(readChar(path, file.size(path), useBytes = TRUE), "WINDOWS-1252", "UTF-8")
  out <- parse_applitrack_output(js)
  expect_equal(nrow(out), 14)
  expect_true(all(grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", out$date_posted)))
  p <- out[out$job_id == "836", ]
  expect_equal(p$title, "K-5 Special Education Teacher, Life Skills")
  expect_equal(p$position, "Student Support Services")
  expect_true(is.na(p$position2))
  expect_equal(p$date_posted, "10/26/2026")
  expect_equal(p$location, "TCSD Schools")
})

# SchoolSpring detail records: real ones captured 2026-11-03 from
# api.schoolspring.com/api/Jobs/<jobId> (Montana postings; the parser is
# identical in the MT and WY repos).
ss_job_fixture <- function(id) {
  paste(readLines(test_path("fixtures", paste0("schoolspring_job_", id, ".json")), warn = FALSE), collapse = "\n")
}

test_that("parse_schoolspring_job_json reads a filled posting with no deadline", {
  # Lewistown District Nurse: left the board, record still says Filled (50);
  # "Posted until filled" is stored as closeDate 2050-01-01.
  r <- parse_schoolspring_job_json(ss_job_fixture(5803798))
  expect_equal(r$job_id, "5803798")
  expect_equal(r$Job_Status, "filled")
  expect_true(is.na(r$Close_Date))
  expect_equal(c(r$Pay_Min, r$Pay_Max, r$Pay_Unit), c("25.67", "25.67", "hour"))
})

test_that("parse_schoolspring_job_json keeps the deadline applicants see, not the UTC closeDate", {
  # Ronan assistant girls' basketball: closeDate 2026-11-14T06:59 (UTC) is
  # "Nov 13, 2026 11:59 PM (Mountain Standard Time)" on the board. Its
  # "Per Year" $1,919.10 is a coaching stipend, not a salary.
  r <- parse_schoolspring_job_json(ss_job_fixture(5732320))
  expect_equal(r$Close_Date, "2026-11-13")
  expect_equal(r$Job_Status, "posted")
  expect_equal(r$Pay_Unit, "stipend")
  expect_equal(r$Pay_Min, 1919.1)
  expect_match(r$Pay_Text, "^\\$1,919\\.10 Per Year")
})

test_that("parse_schoolspring_job_json reads units from the amount when the label is ambiguous", {
  # Belgrade .5 FTE math teacher: "Hourly and Certified Pay" $22,725-$26,622 is a salary.
  r <- parse_schoolspring_job_json(ss_job_fixture(5785656))
  expect_equal(c(r$Pay_Min, r$Pay_Max), c(22725, 26622))
  expect_equal(r$Pay_Unit, "year")
  # Belgrade substitute bus driver: "Substitute" $45 says neither hour nor day.
  r <- parse_schoolspring_job_json(ss_job_fixture(5747025))
  expect_true(is.na(r$Pay_Min) && is.na(r$Pay_Unit) && is.na(r$Pay_Text))
})
test_that("parse_applitrack_output un-escapes document.write literals and parses real Montana posting fields", {
  # Real fixture captured 2026-08-06 from
  # https://www.applitrack.com/ehps/onlineapp/jobpostings/Output.asp?all=1
  # (East Helena Public Schools) -- the endpoint default.aspx's own inline
  # script injects into the page via document.write() rather than being
  # server-rendered directly.
  fixture <- paste(readLines(test_path("fixtures", "applitrack_output_mt.txt"), warn = FALSE), collapse = "\n")

  result <- parse_applitrack_output(fixture)

  expect_equal(nrow(result), 12)
  expect_equal(result$title[1:4], c(
    "Assistant Wrestling Coaches",
    "Math Teacher",
    "Middle School Teacher-Intervention Specialist",
    "Paraprofessional"
  ))
  # Regression (carried over from the Wyoming port): "Position Type:"
  # consumes TWO values (category + subcategory) only when the next value
  # isn't itself another label's value -- getting this wrong shifts every
  # subsequent field by one position for every row after the first
  # mismatch. Row 1 here exercises the two-value case for real
  # ("Athletics/Activities/" + "Coaching").
  expect_equal(result$position[1:4], c(
    "Athletics/Activities/", "District Wide/", "District Wide/", "District Wide/"
  ))
  expect_equal(result$position2[1:4], c(
    "Coaching", "ELEMENTARY TEACHER", "ELEMENTARY TEACHER", "Paraprofessional"
  ))
  expect_equal(result$location[1:4], c(
    "East Helena High School", "District Wide", "East Valley Middle School", "District Wide"
  ))
  # This district's real postings carry no "Closing Date:" label at all --
  # confirmed real absence, not a parsing gap.
  expect_true(all(is.na(result$closing_date)))
  # Every posting carries AppliTrack's own job ID (block id "p<ID>_").
  expect_true(all(grepl("^[0-9]+$", result$job_id)))
  expect_equal(anyDuplicated(result$job_id), 0)
})

test_that("fetch_applitrack_postings decodes Windows-1252 bytes instead of silently dropping them as NA (Mo)", {
  # Regression: Applitrack serves Windows-1252 with no charset in
  # Content-Type. httr2 guesses UTF-8 by default; if a posting contains a
  # non-ASCII Windows-1252 byte (e.g. an en-dash, 0x96), UTF-8 decoding
  # fails and resp_body_string() returns NA rather than erroring -- which
  # parse_applitrack_output() then silently turns into zero rows,
  # indistinguishable from a district with genuinely no openings. Confirmed
  # as a real bug in the Wyoming port (10 of 25 WY districts were affected,
  # one hiding 69 real postings), so it's tested here from the start rather
  # than waiting to rediscover it in Montana's own data. This fixture is
  # the real applitrack_output_mt.txt fixture with an en-dash injected into
  # the first title and re-encoded as raw Windows-1252 bytes, exactly
  # mirroring what the live endpoint sends.
  raw_bytes <- readBin(
    test_path("fixtures", "applitrack_output_mt_windows1252.bin"),
    "raw",
    file.info(test_path("fixtures", "applitrack_output_mt_windows1252.bin"))$size
  )
  httr2::local_mocked_responses(
    list(httr2::response(200, headers = list("Content-Type" = "text/javascript"), body = raw_bytes))
  )

  result <- fetch_applitrack_postings("ehps")

  expect_equal(nrow(result), 12)
  expect_equal(result$title[1], "Assistant Wrestling Coaches – JV/Varsity")
})
