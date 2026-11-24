# The SmartSites (ParentSquare) family reader (R/smartsites.R). Fixtures are real pages, one per widget, captured
# for the per-district scrapers this reader replaced (claude-notes/2026-11-24_smartsites-family/):
#   smartsites_ekalaka_posts.html     posts widget (captured 2026-08-23)
#   smartsites_park16_documents.html  document list (captured 2026-08-02)
#   smartsites_platte1_gallery.html   image gallery (captured 2026-08-02)
#   smartsites_rapelje_no_widget_2026-11-23.html  a SmartSites page whose openings are free text in an editor block

smartsites_fixture <- function(name) {
  paste(readLines(test_path("fixtures", name), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

test_that("parse_smartsites_postings: the posts widget gives each post's title and date, not the forms post", {
  url <- "https://www.ekalaka.net/336413_2"
  r <- parse_smartsites_postings(smartsites_fixture("smartsites_ekalaka_posts.html"), url, "Ekalaka")
  expect_equal(r$Title, c("Paraprofessional for the 26-27 school year", "Temporary Custodial Position",
                          "Assistant cook for the 26-27 school year", "Elementary Teacher for 2026-2027 Year",
                          "Agricultural teacher for the 2026-2027", "Coaching Positions", "Bus Drivers"))
  expect_equal(r$Posted_Date, c("2026-07-10", "2026-06-12", "2026-05-06", "2026-04-01", "2026-04-28",
                                "2026-05-04", "2026-01-26"))
  expect_equal(r$Location, rep("Ekalaka", 7))
  expect_equal(r$Link, rep(url, 7))
  expect_equal(names(r), c("Title", "Location", "Posted_Date", "Link"))
})

test_that("parse_smartsites_postings: the document list gives titles from data-searchText and dates from the link label", {
  # libxml2 lowercases attribute names: the page's data-searchText must be read as data-searchtext, or every title
  # is NA. The list also holds the blank application forms, with no markup to tell them apart.
  url <- "https://pcsd16.com/271372_2"
  r <- parse_smartsites_postings(smartsites_fixture("smartsites_park16_documents.html"), url, NA_character_)
  expect_equal(r$Title, c("HS Head Track Coach", "MS Head Track Coach", "School Guidance Counselor"))
  expect_equal(r$Posted_Date, c("2026-04-28", "2026-04-28", "2026-05-07"))
  expect_equal(r$Location, rep(NA_character_, 3))
  expect_equal(r$Link, rep(url, 3))
})

test_that("parse_smartsites_postings: the image gallery gives the flyers' alt text, without forms or dates", {
  # The gallery template self-closes its <a>, so the images are the link's siblings (an `a > figure > img`
  # selector finds nothing). Application-form flyers sit in the same gallery.
  url <- "https://www.platte1.org/89574_1"
  r <- parse_smartsites_postings(smartsites_fixture("smartsites_platte1_gallery.html"), url, "Wheatland")
  expect_equal(r$Title, c("Seeking Substitutes", "Hiring Subs", "Custodian Subs", "Wheatland Custodian",
                          "Glendo Custodian", "VB Aide", "Bus Driver", "Maintenance Worker",
                          "WHS Assist Girls BBall Coach 2", "SS Teacher", "WMS Assistant VB Coach", "WHS SPED Para"))
  expect_equal(r$Posted_Date, rep(NA_character_, 12))
  expect_equal(r$Location, rep("Wheatland", 12))
  expect_equal(r$Link, rep(url, 12))
})

test_that("parse_smartsites_postings: a SmartSites page with no widget reads as zero rows with the four columns", {
  # Rapelje's page lists its one opening as free text in an editor block: no widget, so nothing for this reader.
  r <- parse_smartsites_postings(smartsites_fixture("smartsites_rapelje_no_widget_2026-11-23.html"),
                                 "https://www.rapelje.k12.mt.us/job-listings", "Rapelje")
  expect_equal(nrow(r), 0)
  expect_equal(names(r), c("Title", "Location", "Posted_Date", "Link"))
  expect_type(r$Posted_Date, "character")
  expect_type(r$Link, "character")
})

test_that("parse_smartsites_postings: a page with no SmartSites markup, or no HTML, is an error, not an empty board", {
  expect_error(parse_smartsites_postings("<html><body><p>Jobs</p></body></html>", "https://x.example/jobs", NA),
               "no SmartSites markup")
  expect_error(parse_smartsites_postings("Site under maintenance", "https://x.example/jobs", NA), "no HTML")
})

test_that("parse_smartsites_postings keeps each document's date with its own title when an item has no link", {
  # An item without a link once shifted every later date onto the wrong title (labels read across all items).
  html <- paste0(
    '<ul><li class="ss-document-item" data-searchText="Bus Driver"><a class="ss-document-link" aria-label="Bus Driver added 2026-09-01"></a></li>',
    '<li class="ss-document-item" data-searchText="Classified Application"></li>',
    '<li class="ss-document-item" data-searchText="Cook"></li>',
    '<li class="ss-document-item" data-searchText="Para"><a class="ss-document-link" aria-label="Para added 2026-09-20"></a></li></ul>'
  )
  r <- parse_smartsites_postings(html, "u", NA_character_)
  expect_equal(r$Title, c("Bus Driver", "Cook", "Para"))
  expect_equal(r$Posted_Date, c("2026-09-01", NA, "2026-09-20"))
})

test_that("parse_smartsites_postings drops decorative \"logo\" gallery images and blank alt text", {
  # Platte 1's gallery started carrying alt="logo"/"Logo" images (2026-08-03), each scraped as a fake posting.
  html <- paste0(
    '<div class="ss-component"><ul><li class="picseries_image"><a href="/a" /><figure><img alt="Bus Driver"></figure></li>',
    '<li class="picseries_image"><a href="/b" /><figure><img alt="logo"></figure></li>',
    '<li class="picseries_image"><a href="/c" /><figure><img alt=" Logo "></figure></li>',
    '<li class="picseries_image"><a href="/d" /><figure><img alt=" "></figure></li></ul></div>'
  )
  expect_equal(parse_smartsites_postings(html, "u", NA_character_)$Title, "Bus Driver")
})

test_that("parse_smartsites_postings reads a post's date by English month name or abbreviation", {
  post <- function(title, date) sprintf(
    '<a class="ss-row ss-post-page-row" href="/p"><h2 class="ss-post-title"> %s </h2><div class="ss-post-date">%s</div></a>',
    title, date)
  html <- paste0("<div>", post("Cook", "December 1, 2026"), post("Para", "Sept. 9, 2026"), post("Aide", "soon"), "</div>")
  r <- parse_smartsites_postings(html, "u", "Town")
  expect_equal(r$Title, c("Cook", "Para", "Aide"))
  expect_equal(r$Posted_Date, c("2026-12-01", NA, NA))
  html2 <- paste0("<div>", post("Cook", "Sep 9, 2026"), post("Driver", "Feb 30, 2026"), "</div>")
  expect_equal(parse_smartsites_postings(html2, "u", "Town")$Posted_Date, c("2026-09-09", NA))
})

test_that("fetch_smartsites_postings fetches the page and reads it", {
  fixture <- smartsites_fixture("smartsites_park16_documents.html")
  httr2::local_mocked_responses(list(
    httr2::response(200, headers = list("Content-Type" = "text/html; charset=utf-8"), body = charToRaw(fixture))))
  r <- fetch_smartsites_postings("https://pcsd16.com/271372_2", "Meeteetse")
  expect_equal(r$Title, c("HS Head Track Coach", "MS Head Track Coach", "School Guidance Counselor"))
  expect_equal(r$Location, rep("Meeteetse", 3))
})

test_that("the SmartSites spec reads Job_Link and City from a registry row", {
  reg <- data.frame(District = "S", Platform = "SmartSites", Platform_Key = "", City = "Ekalaka",
                    Job_Link = "https://www.ekalaka.net/336413_2", List_Start = "", List_Stop = "", List_Skip = "",
                    stringsAsFactors = FALSE)
  call <- resolve_scraper_call("S", list(scraper_registry(reg, "District", "registry", specs = scraper_specs())))
  expect_equal(call$fn, "fetch_smartsites_postings")
  expect_equal(call$args, list("https://www.ekalaka.net/336413_2", "Ekalaka"))
})
