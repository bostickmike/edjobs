# The platform fetchers moved from the states in 0.11.0. Fixtures are real
# captures named in each test; the tests came with the functions (MT's and
# WY's), with the settled differences tested where they were settled.

read_fixture <- function(name, encoding = "unknown") {
  paste(readLines(test_path("fixtures", name), warn = FALSE, encoding = encoding), collapse = "\n")
}

# ---- SchoolSpring -----------------------------------------------------------

test_that("parse_schoolspring_json extracts real Montana posting fields from a single page", {
  # Captured 2026-08-06 from api.schoolspring.com GetPagedJobsWithSearch?domainName=lewistown.schoolspring.com
  result <- parse_schoolspring_json(read_fixture("schoolspring_lewistown.json"), "lewistown.schoolspring.com")
  expect_equal(nrow(result), 10)
  expect_equal(result$Title[1:3], c("District Nurse", "Fergus High School Head Cheer Coach",
                                    "Lewistown Junior High School Head and Assistant Volleyball Coaches"))
  expect_equal(result$Location[1], "Lewistown, Montana")
  expect_equal(result$Posted_Date[1], "2026-06-25")
  expect_equal(result$Link[1], "https://lewistown.schoolspring.com/jobs/5803798")
  expect_equal(result$Posting_ID[1], "schoolspring:lewistown.schoolspring.com:5803798")
  # A real title with an HTML entity: JSON string decoding, not raw HTML.
  expect_equal(result$Title[6], "Custodian - Lewis &amp; Clark Elementary")
})

test_that("parse_schoolspring_json builds ids and links per job and drops SchoolSpring's sample vacancies", {
  fixture <- '{"success":true,"message":"","validationErrors":[],"value":{"page":1,"size":25,"jobsList":[
    {"jobId":5846934,"employer":"Sundance Secondary","title":"High Needs SPED Paraprofessional","location":"Sundance, Wyoming","displayDate":"2026-07-22T06:00:00"},
    {"jobId":5324570,"employer":"Sundance Secondary","title":"Substitute Custodian","location":"Sundance, Wyoming","displayDate":"2026-06-05T06:00:00"},
    {"jobId":1,"employer":"SchoolSpring","title":"Sample Certified Position","location":"Example, Wyoming","displayDate":"2010-02-16T06:00:00"}
  ]}}'
  result <- parse_schoolspring_json(fixture, "crook1.schoolspring.com")
  expect_equal(nrow(result), 2)
  expect_equal(result$Title, c("High Needs SPED Paraprofessional", "Substitute Custodian"))
  expect_equal(result$Posted_Date, c("2026-07-22", "2026-06-05"))
  expect_equal(result$Posting_ID, c("schoolspring:crook1.schoolspring.com:5846934", "schoolspring:crook1.schoolspring.com:5324570"))
  expect_equal(result$Link, c("https://crook1.schoolspring.com/jobs/5846934", "https://crook1.schoolspring.com/jobs/5324570"))
})

test_that("parse_schoolspring_json returns zero rows (not an error) for an empty or all-sample district", {
  empty <- '{"success":true,"message":"","validationErrors":[],"value":{"page":1,"size":50,"jobsList":[]}}'
  result <- parse_schoolspring_json(empty, "sd6.schoolspring.com")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("Title", "Location", "Posted_Date", "Posting_ID", "Link"))
  only_sample <- '{"success":true,"message":"","validationErrors":[],"value":{"page":1,"size":25,"jobsList":[
    {"jobId":1,"employer":"SchoolSpring","title":"Sample Classified Position","location":"Example, Montana","displayDate":"2010-02-16T06:00:00"}]}}'
  expect_equal(nrow(parse_schoolspring_json(only_sample, "x.schoolspring.com")), 0)
})

test_that("fetch_schoolspring_postings stops paging once a page comes back short", {
  fixture <- read_fixture("schoolspring_lewistown.json")
  # 10 rows < page_size 50: one request, no second response queued.
  httr2::local_mocked_responses(list(
    httr2::response(200, headers = list("Content-Type" = "application/json"), body = charToRaw(fixture))))
  result <- fetch_schoolspring_postings("lewistown.schoolspring.com")
  expect_equal(nrow(result), 10)
})

test_that("schoolspring_domain strips the scheme and trailing slash from a registry link", {
  expect_equal(schoolspring_domain("https://crook1.schoolspring.com/"), "crook1.schoolspring.com")
  expect_equal(schoolspring_domain("http://laurel.schoolspring.com"), "laurel.schoolspring.com")
})

# ---- TedK12 -----------------------------------------------------------------

test_that("parse_tedk12_postings extracts real Goshen (WY) rows and links each posting's own page", {
  # Real chromote-rendered capture of Goshen County SD1's board with a browser UA.
  html <- read_fixture("tedk12_goshen.html", encoding = "UTF-8")
  result <- parse_tedk12_postings(html, "https://goshen.tedk12.com/hire/index.aspx")
  expect_equal(nrow(result), 12)
  expect_true(all(grepl("^https://goshen\\.tedk12\\.com/hire/ViewJob\\.aspx\\?JobID=\\d+$", result$url)))
  expect_equal(result$url[result$title == "Office Clerk - Platte River High School"],
               "https://goshen.tedk12.com/hire/ViewJob.aspx?JobID=941")
  expect_false("Job Title" %in% result$title)
  clerk <- result[result$title == "Office Clerk - Platte River High School", ]
  expect_equal(nrow(clerk), 1)
  expect_equal(clerk$date_posted, "08/04/2026")
  expect_equal(clerk$position, "Support")
  expect_equal(clerk$location, "Platte River High School")
})

test_that("parse_tedk12_postings extracts real Hardin (MT) rows", {
  # Captured 2026-08-06 via curl with a real browser UA.
  html <- read_fixture("tedk12_hardin_mt.html", encoding = "UTF-8")
  result <- parse_tedk12_postings(html, "https://hardinpublic.tedk12.com/hire/index.aspx")
  expect_equal(nrow(result), 6)
  expect_true(all(grepl("^https://hardinpublic\\.tedk12\\.com/hire/ViewJob\\.aspx\\?JobID=", result$url)))
  tech <- result[result$title == "Maintenance Technician IV - District Wide", ]
  expect_equal(nrow(tech), 1)
  expect_equal(tech$date_posted, "07/31/2026")
  expect_equal(tech$position, "Maintenance")
  expect_equal(tech$location, "District Wide")
})

test_that("parse_tedk12_postings falls back to the board URL for a row whose link has no JobID, and handles no table", {
  html <- '<table><tr><td><a href="index.aspx">Job Title</a></td><td>Posting Date</td><td>Position</td><td>Location</td></tr>
    <tr><td><a href="somewhere.aspx">Bus Driver</a></td><td>08/01/2026</td><td>Support</td><td>District</td></tr></table>'
  result <- parse_tedk12_postings(html, "https://x.tedk12.com/hire/index.aspx")
  expect_equal(result$title, "Bus Driver")
  expect_equal(result$url, "https://x.tedk12.com/hire/index.aspx")
  none <- parse_tedk12_postings("<html><body><p>Not a TedK12 page</p></body></html>", "https://example.tedk12.com/hire/index.aspx")
  expect_equal(nrow(none), 0)
  expect_equal(names(none), c("title", "date_posted", "position", "location", "url"))
})

# ---- PeopleAdmin ------------------------------------------------------------

test_that("parse_peopleadmin_atom uses each entry's author (the department) as Location, MT's way", {
  # Captured 2026-08-06 from https://jobs.gfcmsu.edu/postings/search.atom
  result <- parse_peopleadmin_atom(read_fixture("peopleadmin_atom_mt.xml"), location_fallback = "Great Falls College MSU")
  expect_equal(nrow(result), 9)
  expect_equal(result$Title[1:3], c("Early Learning Center Teachers", "Director of Workforce Operations",
                                    "Health Sciences Program Site Coordinator"))
  expect_equal(result$Location[1:3], c("Office of Administration & Finance", "CTE/Health Sciences", "Dental Hygiene"))
  expect_false(any(result$Location == "Great Falls College MSU"))
  expect_equal(result$Posted_Date[1], "2026-08-06")
  expect_equal(result$Link[1], "https://jobs.gfcmsu.edu/postings/2546")
})

test_that("parse_peopleadmin_atom gives every row the fallback when asked, WY's way, and when an entry has no author", {
  fixture <- '<?xml version="1.0" encoding="UTF-8"?>
<feed xml:lang="en-US" xmlns="http://www.w3.org/2005/Atom">
  <entry>
    <id>https://test.peopleadmin.com/postings/1</id>
    <published>2026-03-19T08:16:49-06:00</published>
    <link rel="alternate" type="text/html" href="https://test.peopleadmin.com/postings/1"/>
    <title>Math Instructor</title>
    <author><name>Math/Science</name></author>
  </entry>
  <entry>
    <id>https://test.peopleadmin.com/postings/2</id>
    <published>2026-05-01T08:00:00-06:00</published>
    <link rel="alternate" type="text/html" href="https://test.peopleadmin.com/postings/2"/>
    <title>Adjunct Biology Instructor</title>
  </entry>
</feed>'
  campus <- parse_peopleadmin_atom(fixture, "Test College Campus", location_from_author = FALSE)
  expect_equal(campus$Title, c("Math Instructor", "Adjunct Biology Instructor"))
  expect_equal(campus$Location, rep("Test College Campus", 2))
  expect_equal(campus$Posted_Date, c("2026-03-19", "2026-05-01"))
  expect_equal(campus$Link, c("https://test.peopleadmin.com/postings/1", "https://test.peopleadmin.com/postings/2"))
  dept <- parse_peopleadmin_atom(fixture, "Test College Campus")
  expect_equal(dept$Location, c("Math/Science", "Test College Campus"))
})

test_that("parse_peopleadmin_atom returns zero rows (not an error) for an empty feed", {
  fixture <- '<?xml version="1.0" encoding="UTF-8"?>
<feed xml:lang="en-US" xmlns="http://www.w3.org/2005/Atom"><title>Empty Feed</title></feed>'
  result <- parse_peopleadmin_atom(fixture, "Some Institution")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("Title", "Location", "Posted_Date", "Link"))
})

test_that("fetch_peopleadmin_atom fetches and parses a live-shaped response", {
  httr2::local_mocked_responses(list(
    httr2::response(200, headers = list("Content-Type" = "application/atom+xml; charset=utf-8"),
                    body = charToRaw(read_fixture("peopleadmin_atom_mt.xml")))))
  result <- fetch_peopleadmin_atom("https://jobs.gfcmsu.edu/postings/search.atom",
                                   location_fallback = "Great Falls College MSU", fetch_pay = FALSE)
  expect_equal(nrow(result), 9)
  expect_equal(result$Title[1], "Early Learning Center Teachers")
  expect_true(all(c(POSTED_PAY_COLUMNS, "Close_Date") %in% names(result)))
})

# ---- NEOGOV: the list -------------------------------------------------------

test_that("parse_neogov_html dedupes the card/table double-render and extracts real fields", {
  # Central Wyoming College's real AJAX fragment: every posting is rendered
  # twice (card <ul> with metadata, plain <table> without). Without scoping
  # to ul.job-listing-container it parsed as 20 rows with NA metadata.
  result <- parse_neogov_html(read_fixture("neogov_cwc.html"), "https://www.schooljobs.com")
  expect_equal(nrow(result), 10)
  expect_equal(length(unique(result$Title)), 10)
  expect_true(all(!is.na(result$Location)))
  expect_true(all(!is.na(result$Posted_Date)))
  expect_true("Adjunct" %in% result$Title)
  expect_true(any(grepl("^https://www.schooljobs.com/careers/cwc/jobs/", result$Link)))
  expect_true(all(POSTED_PAY_COLUMNS %in% names(result)))
})

test_that("parse_neogov_html reads UM's real page with its campus names and card pay", {
  # UM's classic-board fragment, page 1, captured 2026-10-30.
  result <- parse_neogov_html(read_fixture("neogov_ummissoula_p1_2026-10-30.html"), "https://www.schooljobs.com")
  expect_equal(nrow(result), 10)
  expect_setequal(unique(result$Location), c("Helena College Airport Campus", "Helena College Donaldson Campus", "Missoula Mountain Campus"))
  expect_true(any(!is.na(result$Pay_Min)))
  expect_equal(neogov_job_id(result$Link), sub("^.*/jobs/([0-9]+).*$", "\\1", result$Link))
})

test_that("parse_neogov_html returns zero rows (not an error) when the fragment has no job-listing-container", {
  result <- parse_neogov_html('<div class="jobs-not-found-container"><h2>No jobs at this time.</h2></div>', "https://www.schooljobs.com")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("Title", "Location", "Posted_Date", "Link", POSTED_PAY_COLUMNS))
})

test_that("fetch_neogov_postings follows pagination, builds links on the domain the redirect lands on, and takes only a slug", {
  page1 <- read_fixture("neogov_cwc.html")
  page2 <- '<ul class="unstyled search-results-listing-container job-listing-container ">
    <li class="list-item" data-job-id="9999999">
      <h3 class="job-item-link-container">
        <a class="item-details-link" href="/careers/cwc/jobs/9999999/extra-adjunct">Extra Adjunct</a>
      </h3>
      <ul class="list-meta"><li>Riverton</li></ul>
      <div class="list-published"><span class="list-entry-starts"><span>Posted 1 day ago</span></span></div>
    </li>
  </ul>'
  page3 <- '<div class="jobs-not-found-container"><h2 class="not-found-text">No jobs at this time.</h2></div>'
  landed <- "https://www.governmentjobs.com/careers/home/index?agency=cwc&page="
  httr2::local_mocked_responses(list(
    httr2::response(200, url = paste0(landed, 1), body = charToRaw(page1)),
    httr2::response(200, url = paste0(landed, 2), body = charToRaw(page2)),
    httr2::response(200, url = paste0(landed, 3), body = charToRaw(page3))))
  result <- fetch_neogov_postings("cwc", details = FALSE)
  expect_equal(nrow(result), 11)
  expect_true("Extra Adjunct" %in% result$Title)
  expect_true(all(grepl("^https://www.governmentjobs.com/careers/cwc/jobs/", result$Link)))
  expect_true(all(is.na(result$Close_Date)))
  expect_error(fetch_neogov_postings("https://www.schooljobs.com", details = FALSE), "agency slug")
})

test_that("fetch_neogov_postings with details takes each posting's exact dates from its page, keeping the card's text where a page fails", {
  page1 <- read_fixture("neogov_ummissoula_p1_2026-10-30.html")
  none <- '<div class="jobs-not-found-container"><h2>No jobs at this time.</h2></div>'
  dated <- read_fixture("neogov_posting_ummissoula_5533670_jsonld_2026-11-12.html")
  undated <- read_fixture("neogov_posting_ummissoula_5287236_jsonld_2026-11-12.html")
  seen <- character(0)
  httr2::local_mocked_responses(function(req) {
    seen <<- c(seen, req$url)
    if (grepl("/careers/home/index", req$url, fixed = TRUE)) {
      body <- if (grepl("page=1", req$url, fixed = TRUE)) page1 else none
      return(httr2::response(200, url = req$url, body = charToRaw(body)))
    }
    if (grepl("/jobs/5533670", req$url, fixed = TRUE)) return(httr2::response(200, url = req$url, body = charToRaw(dated)))
    if (grepl("/jobs/5505116", req$url, fixed = TRUE)) return(httr2::response(500, url = req$url))
    httr2::response(200, url = req$url, body = charToRaw(undated))
  })
  result <- suppressMessages(fetch_neogov_postings("ummissoula", detail_delay = 0))
  expect_equal(nrow(result), 10)
  expect_equal(sum(grepl("/jobs/", seen)), 10)
  row <- result[neogov_job_id(result$Link) == "5533670", ]
  expect_equal(row$Posted_Date, "2026-10-29")
  expect_equal(row$Close_Date, "2026-11-15")
  failed <- result[neogov_job_id(result$Link) == "5505116", ]
  expect_match(failed$Posted_Date, "^Posted ")
  expect_true(is.na(failed$Close_Date))
  others <- result[!neogov_job_id(result$Link) %in% c("5533670", "5505116"), ]
  expect_true(all(others$Posted_Date == "2026-05-25"))
  expect_true(all(is.na(others$Close_Date)))
})

# ---- NEOGOV: the posting page's JobPosting block ----------------------------

test_that("parse_jobposting_jsonld reads a NEOGOV posting's dates, pay and campus, and NA where the page has none", {
  # The JSON-LD blocks of three real posting pages (2026-11-12).
  dated <- parse_jobposting_jsonld(read_fixture("neogov_posting_ummissoula_5533670_jsonld_2026-11-12.html"))
  expect_equal(dated$Title, "Academic Advisor II, College of Humanities and Social Sciences and the College of Science")
  expect_equal(dated$Date_Posted, "2026-10-29")
  expect_equal(dated$Valid_Through, "2026-11-15")
  expect_equal(dated$Locality, "Missoula Mountain Campus")
  expect_equal(dated$Region, "MT")
  expect_equal(c(dated$Pay_Min, dated$Pay_Max), c(22.53, 22.53))
  expect_equal(dated$Pay_Unit, "hour")
  expect_match(dated$Organization, "University of Montana")

  continuous <- parse_jobposting_jsonld(read_fixture("neogov_posting_ummissoula_5287236_jsonld_2026-11-12.html"))
  expect_equal(continuous$Date_Posted, "2026-05-25")
  expect_true(is.na(continuous$Valid_Through))

  cwc <- parse_jobposting_jsonld(read_fixture("neogov_posting_cwc_5473025_jsonld_2026-11-12.html"))
  expect_equal(cwc$Date_Posted, "2026-09-03")
  expect_equal(cwc$Organization, "Central Wyoming College")

  expect_true(all(is.na(parse_jobposting_jsonld("<html><body>no block</body></html>"))))
  expect_true(all(is.na(parse_jobposting_jsonld(NA_character_))))
  expect_true(is.na(parse_jobposting_jsonld('<script type="application/ld+json">{not json</script>')$Date_Posted))
})

test_that("parse_jobposting_jsonld treats a 0-0 salary as none stated and reads an array of blocks", {
  html <- '<html><head><script type="application/ld+json">[{"@type":"Organization","name":"x"},
    {"@context":"https://schema.org/","@type":"JobPosting","title":"Diesel Mechanics Instructor","datePosted":"2025-07-29",
     "validThrough":"2025-08-05","hiringOrganization":{"@type":"Organization","name":"Helena College"},
     "jobLocation":{"@type":"Place","address":{"@type":"PostalAddress","addressLocality":"Helena College Airport Campus","addressRegion":"MT"}},
     "baseSalary":{"@type":"MonetaryAmount","currency":"USD","value":{"@type":"QuantitativeValue","minValue":0,"maxValue":0,"unitText":"YEAR"}}}]</script></head></html>'
  j <- parse_jobposting_jsonld(html)
  expect_equal(j$Title, "Diesel Mechanics Instructor")
  expect_equal(j$Valid_Through, "2025-08-05")
  expect_true(is.na(j$Pay_Min) && is.na(j$Pay_Max) && is.na(j$Pay_Unit))
  expect_equal(j$Organization, "Helena College")
})

test_that("parse_neogov_posting_page returns the two date columns and fetch_neogov_posting_details one row per link", {
  p <- parse_neogov_posting_page(read_fixture("neogov_posting_cwc_5473025_jsonld_2026-11-12.html"))
  expect_equal(names(p), c("Posted_Date", "Close_Date"))
  expect_equal(p$Posted_Date, "2026-09-03")
  expect_equal(nrow(fetch_neogov_posting_details(character(0))), 0)
  httr2::local_mocked_responses(function(req) httr2::response(404, url = req$url))
  d <- suppressMessages(fetch_neogov_posting_details(c("https://www.schooljobs.com/careers/cwc/jobs/1/a"), delay = 0))
  expect_equal(d$Link, "https://www.schooljobs.com/careers/cwc/jobs/1/a")
  expect_true(is.na(d$Posted_Date) && is.na(d$Close_Date))
})

# ---- Shared boards ----------------------------------------------------------

test_that("assign_shared_board_institutions gives each posting the institution whose registry pattern matches its Location", {
  registry <- data.frame(
    Institution = c("University of Montana", "Helena College", "University of Montana Western", "Missoula College", "Other College"),
    Platform = c("Neogov", "SharesBoard", "SharesBoard", "SharesBoard", "SharesBoard"),
    Feed_URL = c("ummissoula", "ummissoula", "ummissoula", "ummissoula", "otherboard"),
    Location_Pattern = c(NA, "^Helena College", "University of Montana Western", "Missoula College", "^Other College"),
    stringsAsFactors = FALSE)
  postings <- data.frame(
    Title = c("a", "b", "c", "d", "e"),
    Location = c("Missoula Mountain Campus", "Helena College Donaldson Campus", "Helena College Airport Campus",
                 "University of Montana Western", NA),
    Institution = "University of Montana", stringsAsFactors = FALSE)
  out <- assign_shared_board_institutions(postings, "ummissoula", registry)
  expect_equal(out$Institution, c("University of Montana", "Helena College", "Helena College",
                                  "University of Montana Western", "University of Montana"))
  # No pattern column, no rows, or another board: unchanged.
  expect_equal(assign_shared_board_institutions(postings, "ummissoula", registry[, 1:3])$Institution, postings$Institution)
  expect_equal(nrow(assign_shared_board_institutions(postings[0, ], "ummissoula", registry)), 0)
  expect_equal(assign_shared_board_institutions(postings, "otherboard", registry)$Institution, postings$Institution)
})

# ---- NEOGOV: card pay through parse_neogov_html (WY's tests) ---------------

test_that("parse_neogov_html carries each card's pay line, or its description's pay", {
  out <- parse_neogov_html(read_fixture("neogov_cwc.html"), "https://www.schooljobs.com")
  expect_true(all(POSTED_PAY_COLUMNS %in% names(out)))
  expect_equal(out$Pay_Min, c(775, NA, NA, NA, 61356, 77411, 21093, 50248, 15, NA))
  expect_equal(out$Pay_Max, c(775, NA, NA, NA, 76695, 87088, 22953, 62810, 17, NA))
  expect_equal(out$Pay_Unit, c("credit hour", NA, NA, NA, "year", "year", "year", "year", "hour", NA))
  expect_equal(out$Pay_Text[5], "Full-Time - $61,356.00 - $76,695.00 Annually")
  # "Adjunct" states pay only in its description preview.
  expect_match(out$Pay_Text[1], "Current adjunct pay is $775 per credit hour", fixed = TRUE)
})

test_that("parse_neogov_html treats NEOGOV's $0.00 pay field as no pay stated", {
  # LCCC (governmentjobs.com), 2026-10-30: adjunct cards read "Adjunct - $0.00 Hourly".
  out <- parse_neogov_html(read_fixture("neogov_lcccwy_2026-10-30.html"), "https://www.governmentjobs.com")
  expect_equal(nrow(out), 10)
  expect_equal(out$Pay_Min, c(NA, NA, NA, NA, NA, NA, 33.39, 33.39, 33.39, NA))
  expect_equal(out$Pay_Unit, c(NA, NA, NA, NA, NA, NA, "hour", "hour", "hour", NA))
  expect_equal(out$Pay_Text[8], "Part-Time - $33.39 Hourly")
  empty <- parse_neogov_html("<html></html>", "https://www.schooljobs.com")
  expect_equal(nrow(empty), 0)
  expect_true(all(POSTED_PAY_COLUMNS %in% names(empty)))
})

test_that("parse_neogov_html keeps every posting when the card pay can't be read", {
  local_mocked_bindings(parse_neogov_card_pay = function(...) stop("markup changed"))
  out <- suppressMessages(parse_neogov_html(read_fixture("neogov_cwc.html"), "https://www.schooljobs.com"))
  expect_equal(nrow(out), 10)
  expect_equal(out$Title[5], "Director of Development- Jackson Campus")
  expect_true(all(POSTED_PAY_COLUMNS %in% names(out)))
  expect_true(all(is.na(out$Pay_Min)))
})

# ---- Review of 0.11.0 (2026-11-12): odd but valid page shapes ---------------

# The UM fixture with one card's markup changed, for the per-card tests.
um_page_with <- function(card_no, edit) {
  doc <- xml2::read_html(read_fixture("neogov_ummissoula_p1_2026-10-30.html"))
  cards <- xml2::xml_find_all(doc, "//ul[contains(@class,'job-listing-container')]/li[contains(@class,'list-item')]")
  edit(cards[[card_no]])
  as.character(doc)
}

test_that("parse_neogov_html reads each card's own Location, so one card without its meta block blanks only that card", {
  full <- parse_neogov_html(read_fixture("neogov_ummissoula_p1_2026-10-30.html"), "https://www.schooljobs.com")
  html <- um_page_with(2, function(card) xml2::xml_remove(xml2::xml_find_all(card, ".//ul[contains(@class,'list-meta')]")))
  out <- parse_neogov_html(html, "https://www.schooljobs.com")
  expect_equal(nrow(out), 10)
  expect_true(is.na(out$Location[2]))
  expect_equal(out$Location[-2], full$Location[-2])
  # Card 2 is Helena College's; the other Helena card keeps its campus.
  expect_equal(full$Location[2], "Helena College Donaldson Campus")
  expect_true("Helena College Airport Campus" %in% out$Location)
  expect_equal(out$Posted_Date, full$Posted_Date)
})

test_that("parse_neogov_html gives NA, not the pay line, when only a card's location line is missing", {
  html <- um_page_with(1, function(card) xml2::xml_remove(xml2::xml_find_first(card, ".//ul[contains(@class,'list-meta')]/li")))
  out <- parse_neogov_html(html, "https://www.schooljobs.com")
  expect_true(is.na(out$Location[1]))
  expect_false(is.na(out$Pay_Min[1]))
  expect_true(all(!is.na(out$Location[-1])))
})

test_that("parse_neogov_html gives NA for one card's missing date and keeps the rest", {
  full <- parse_neogov_html(read_fixture("neogov_ummissoula_p1_2026-10-30.html"), "https://www.schooljobs.com")
  html <- um_page_with(3, function(card) xml2::xml_remove(xml2::xml_find_all(card, ".//*[contains(@class,'list-entry-starts')]")))
  out <- parse_neogov_html(html, "https://www.schooljobs.com")
  expect_true(is.na(out$Posted_Date[3]))
  expect_equal(out$Posted_Date[-3], full$Posted_Date[-3])
})

test_that("fetch_neogov_postings warns when no posting on the board has a Location", {
  page1 <- '<ul class="job-listing-container"><li class="list-item" data-job-id="1">
    <h3><a class="item-details-link" href="/careers/x/jobs/1/a">A job</a></h3></li></ul>'
  none <- '<div class="jobs-not-found-container"><h2>No jobs at this time.</h2></div>'
  landed <- "https://www.schooljobs.com/careers/home/index?agency=x&page="
  httr2::local_mocked_responses(list(
    httr2::response(200, url = paste0(landed, 1), body = charToRaw(page1)),
    httr2::response(200, url = paste0(landed, 2), body = charToRaw(none))))
  expect_warning(out <- fetch_neogov_postings("x", details = FALSE), "no posting has a Location")
  expect_equal(nrow(out), 1)
})

jsonld_page <- function(json) paste0('<html><head><script type="application/ld+json">', json, '</script></head></html>')

test_that("parse_jobposting_jsonld reads the text forms schema.org allows instead of erroring", {
  # A string hiringOrganization, a string address and a bare-number salary
  # value each threw "$ operator is invalid for atomic vectors" in 0.11.0.
  j <- parse_jobposting_jsonld(jsonld_page('{"@type":"JobPosting","title":"Teacher","datePosted":"2026-11-01T08:00:00",
    "hiringOrganization":"Helena College","jobLocation":{"@type":"Place","address":"Helena, MT"},
    "baseSalary":{"@type":"MonetaryAmount","currency":"USD","value":52000,"unitText":"YEAR"}}'))
  expect_equal(j$Title, "Teacher")
  expect_equal(j$Date_Posted, "2026-11-01")
  expect_equal(j$Organization, "Helena College")
  expect_true(is.na(j$Locality) && is.na(j$Region))
  expect_equal(c(j$Pay_Min, j$Pay_Max), c(52000, 52000))
  expect_equal(j$Pay_Unit, "year")

  bare <- parse_jobposting_jsonld(jsonld_page('{"@type":"JobPosting","title":"Aide","baseSalary":18.5,
    "jobLocation":"Missoula","hiringOrganization":{"name":"UM"}}'))
  expect_equal(bare$Pay_Min, 18.5)
  expect_true(is.na(bare$Pay_Unit))
  expect_true(is.na(bare$Locality))
  expect_equal(bare$Organization, "UM")
})

test_that("parse_jobposting_jsonld finds a JobPosting by an array @type, inside @graph, and in an array of locations", {
  j <- parse_jobposting_jsonld(jsonld_page('{"@context":"https://schema.org","@graph":[{"@type":"WebPage"},
    {"@type":["JobPosting"],"title":"Counselor","jobLocation":[{"@type":"Place","address":{"addressLocality":"Butte","addressRegion":"MT"}}],
     "baseSalary":{"value":{"minValue":"40,000","maxValue":"$50,000","unitText":"year"}}}]}'))
  expect_equal(j$Title, "Counselor")
  expect_equal(c(j$Locality, j$Region), c("Butte", "MT"))
  expect_equal(c(j$Pay_Min, j$Pay_Max), c(40000, 50000))
  expect_equal(j$Pay_Unit, "year")
})

test_that("a JobPosting block that cannot be read gives NA for that page and the next block is tried", {
  local_mocked_bindings(jobposting_row = function(j) if (identical(j$title, "bad")) stop("boom") else data.frame(Title = j$title))
  html <- paste0('<html><head><script type="application/ld+json">{"@type":"JobPosting","title":"bad"}</script>',
                 '<script type="application/ld+json">{"@type":"JobPosting","title":"good"}</script></head></html>')
  expect_message(j <- parse_jobposting_jsonld(html), "could not be read")
  expect_equal(j$Title, "good")
  expect_message(none <- parse_jobposting_jsonld(jsonld_page('{"@type":"JobPosting","title":"bad"}')), "could not be read")
  expect_true(all(is.na(none)))
})
