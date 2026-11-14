test_that("k12_link_posting_id keys each platform's per-posting link; board pages are NA", {
  u <- c("https://www.applitrack.com/bighorn1/onlineapp/default.aspx?AppliTrackLayoutMode=detail&AppliTrackJobId=1534",
         "https://laramie1.schoolspring.com/jobs/5569023",
         "https://jobs.redroverk12.com/org/jcsd1/opening/12345",
         "https://fremont2.tedk12.com/hire/ViewJob.aspx?JobID=77",
         "https://kalispellpublicschoolsmt.tylerportico.com/tess/citizen/jobs/job-list/1271",
         "https://k.tylerportico.com/tess/citizen/jobs/job-list/a76ba87a-ff37-4194-9f9d-b47800b87221",
         "https://laramie1.schoolspring.com/", "https://goshen.tedk12.com/hire/index.aspx", NA)
  expect_equal(k12_link_posting_id(u), c("applitrack:bighorn1:1534", "schoolspring:5569023", "redrover:jcsd1:12345",
                                         "tedk12:fremont2:77", "tylerportico:kalispellpublicschoolsmt:1271",
                                         "tylerportico:k:a76ba87a-ff37-4194-9f9d-b47800b87221",
                                         NA, NA, NA))
  expect_equal(k12_link_posting_id(NULL), character(0))
})

test_that("presence_rows reads each fetcher's columns into title, link and key", {
  at <- presence_rows(data.frame(title = c("Teacher", "Aide"), job_id = c("12", "13")),
                      list(args = list("bighorn1")))
  expect_equal(at$Link_Key, c("applitrack:bighorn1:12", "applitrack:bighorn1:13"))
  expect_match(at$url[1], "AppliTrackJobId=12$")
  ss <- presence_rows(data.frame(Title = c("A", "A dup"), Link = c("https://x.schoolspring.com/jobs/9",
                                                                    "https://x.schoolspring.com/jobs/9")),
                      list(args = list("x.schoolspring.com")))
  expect_equal(nrow(ss), 1)  # a key repeated on the board is kept once
  tk <- presence_rows(data.frame(title = c("Cook", "Bus"), url = c("https://g.tedk12.com/hire/ViewJob.aspx?JobID=5",
                                                                    "https://g.tedk12.com/hire/index.aspx")),
                      list(args = list("https://g.tedk12.com/hire/index.aspx")))
  expect_equal(tk$Link_Key, c("tedk12:g:5", NA))
  expect_equal(nrow(presence_rows(data.frame(), list(args = list()))), 0)
})

day <- as.Date("2026-11-16")
chk <- function(dates, status = "ok", district = "A") {
  data.frame(Date = as.Date(dates), Board = district, Platform = "P", Status = status, N_Postings = 1L,
             N_Keyed = 1L, Seconds = 1, Error = NA_character_, stringsAsFactors = FALSE)
}
seen1 <- function(key = "k1", district = "A") {
  data.frame(Board = district, Platform = "P", District = district, Link_Key = key, List_Key = NA_character_,
             title = "T", url = "u", stringsAsFactors = FALSE)
}
stretch <- function(first, last, n, key = "k1") {
  data.frame(Board = "A", Platform = "P", District = "A", Link_Key = key, List_Key = NA_character_,
             First_Day = as.Date(first), Last_Day = as.Date(last), Days_Seen = n, Title = "T", URL = "u",
             stringsAsFactors = FALSE)
}

test_that("a posting seen on consecutive answered checks extends one stretch", {
  out <- update_presence_sightings(stretch("2026-11-14", "2026-11-15", 2L), seen1(), chk(c("2026-11-14", "2026-11-15")), day)
  expect_equal(nrow(out), 1)
  expect_equal(out$Last_Day, day)
  expect_equal(out$Days_Seen, 3L)
  expect_equal(attr(out, "extended"), 1L)
})

test_that("days the board errored or wasn't checked don't break a stretch; an answered miss does", {
  # 11-15 errored: the stretch ending 11-14 continues.
  out <- update_presence_sightings(stretch("2026-11-13", "2026-11-14", 2L), seen1(),
                                   rbind(chk(c("2026-11-13", "2026-11-14")), chk("2026-11-15", "error")), day)
  expect_equal(nrow(out), 1)
  expect_equal(out$Last_Day, day)
  # No check at all on 11-15 (cron skipped): same.
  out <- update_presence_sightings(stretch("2026-11-13", "2026-11-14", 2L), seen1(), chk(c("2026-11-13", "2026-11-14")), day)
  expect_equal(nrow(out), 1)
  # 11-15 answered (even empty) without it: a new stretch starts today.
  out <- update_presence_sightings(stretch("2026-11-13", "2026-11-14", 2L), seen1(),
                                   rbind(chk(c("2026-11-13", "2026-11-14")), chk("2026-11-15", "empty")), day)
  expect_equal(nrow(out), 2)
  expect_equal(out$First_Day, as.Date(c("2026-11-13", "2026-11-16")))
  expect_equal(attr(out, "started"), 1L)
})

test_that("a new posting starts a stretch; the same posting at another district is its own", {
  out <- update_presence_sightings(stretch("2026-11-15", "2026-11-15", 1L), rbind(seen1("k2"), seen1("k1", "B")),
                                   chk("2026-11-15"), day)
  expect_equal(nrow(out), 3)
  expect_equal(sum(out$First_Day == day), 2)
  # Running the same day twice changes nothing.
  again <- update_presence_sightings(out, rbind(seen1("k2"), seen1("k1", "B")), chk("2026-11-15"), day)
  expect_equal(nrow(again), 3)
  expect_equal(attr(again, "extended") + attr(again, "started"), 0L)
})

test_that("refresh_presence checks each board once, records errors, and does nothing on a second run that day", {
  dir <- withr::local_tempdir()
  env <- new.env()
  env$fetch_good <- function(tenant) data.frame(title = c("Teacher", "Cook"), job_id = c("1", "2"))
  env$fetch_bad <- function(tenant) stop("HTTP 503")
  reg <- data.frame(District = c("Good SD", "Bad SD", "Skip SD"), Platform = c("Good", "Bad", "Other"),
                    Slug = c("good", "bad", "skip"))
  # As in WY: the K-12 registry first, then one the daily job can't read (built by code it doesn't source).
  sources <- list(scraper_registry(reg, "District", "test registry",
                                   specs = list(Good = scraper_spec("fetch_good", "Slug"),
                                                Bad = scraper_spec("fetch_bad", "Slug"))),
                  scraper_registry(function() stop("object 'misc' not found"), "District", "unreadable registry"))
  summary_file <- file.path(dir, "summary.md")
  expect_warning(refresh_presence(dir, c("Good", "Bad"), today = day, sources = sources, delay = 0, envir = env,
                                  summary_path = summary_file), "skipped unreadable registry")
  checks <- read.csv(file.path(dir, "presence_checks.csv"))
  expect_equal(checks$Board, c("Bad SD", "Good SD"))
  expect_equal(checks$Status, c("error", "ok"))
  expect_match(checks$Error[1], "HTTP 503")
  sightings <- read.csv(file.path(dir, "presence_sightings.csv"))
  expect_equal(sightings$Link_Key, c("applitrack:good:1", "applitrack:good:2"))
  expect_true(file.exists(file.path(dir, "README.md")))
  expect_match(paste(readLines(summary_file), collapse = "\n"), "Boards: 2 \\(1 ok, 0 empty, 1 error\\)")

  expect_message(r <- suppressWarnings(refresh_presence(dir, c("Good", "Bad"), today = day, sources = sources,
                                                        delay = 0, envir = env)), "already checked")
  expect_null(r)
  expect_equal(nrow(read.csv(file.path(dir, "presence_checks.csv"))), 2)

  # The next day: Cook gone (board answered), Teacher continues.
  env$fetch_good <- function(tenant) data.frame(title = "Teacher", job_id = "1")
  suppressWarnings(refresh_presence(dir, c("Good", "Bad"), today = day + 1, sources = sources, delay = 0, envir = env))
  s <- read.csv(file.path(dir, "presence_sightings.csv"))
  expect_equal(s$Last_Day, c(format(day + 1), format(day)))
  expect_equal(s$Days_Seen, c(2L, 1L))
})

test_that("fingerprint keys: dates in either form, the archived excerpt, occurrence suffixes", {
  expect_equal(presence_iso_date(c("11/12/2026", "2026-11-12", "2026-11-12T06:00:00Z", "Open Until Filled", NA)),
               c("2026-11-12", "2026-11-12", "2026-11-12", "", ""))
  long <- paste(rep("word", 300), collapse = "  \n ")
  excerpt <- substr(gsub("\\s+", " ", long), 1, 600)   # what the weekly archive stores
  k1 <- opi_posting_key("Big Sandy K-12", "F E Miley School", "Music", "05/29/2026", long)
  k2 <- opi_posting_key("big sandy k-12 ", "F E Miley School", "Music", "2026-05-29", excerpt)
  expect_identical(k1, k2)
  expect_match(k1, "^opi:[0-9a-f]{16}$")
  expect_false(identical(k1, opi_posting_key("Big Sandy K-12", "F E Miley School", "Music", "05/29/2026", "other")))
  expect_true(is.na(opi_posting_key(NA, "x", "Music", "05/29/2026", NA)))  # no detail page read
  expect_identical(wsba_vacancy_key("Platte County School District 1", "Para", "11/03/2026"),
                   wsba_vacancy_key("platte county school district 1", "para ", "2026-11-03"))
  expect_equal(presence_occurrence_keys(c("a", "b", "a")), c("a#1", "b#1", "a#2"))
  expect_equal(presence_occurrence_keys(character(0)), character(0))
})

test_that("a feed is one board: its postings keep their own districts, and an error is the feed's error", {
  dir <- withr::local_tempdir()
  reg <- data.frame(District = "Good SD", Platform = "Good", Slug = "good")
  env <- new.env(); env$fetch_good <- function(tenant) data.frame(title = "Teacher", job_id = "1")
  sources <- list(scraper_registry(reg, "District", "test registry", specs = list(Good = scraper_spec("fetch_good", "Slug"))))
  got_known <- NULL
  feed <- presence_feed("Statewide", "FEED", function(known) {
    got_known <<- known
    data.frame(District = c("North SD", "South SD", "South SD"), title = c("Cook", "Aide", "Aide"),
               url = "https://feed", Link_Key = c("f:1#1", "f:2#1", NA), List_Key = c("cook", "aide", "aide"))
  })
  refresh_presence(dir, "Good", feeds = list(feed), today = day, sources = sources, delay = 0, envir = env)
  checks <- read.csv(file.path(dir, "presence_checks.csv"))
  expect_equal(checks$Board, c("Good SD", "Statewide"))
  expect_equal(checks$N_Postings[checks$Board == "Statewide"], 3)
  expect_equal(checks$N_Keyed[checks$Board == "Statewide"], 2)
  s <- read.csv(file.path(dir, "presence_sightings.csv"))
  expect_equal(s$District[s$Board == "Statewide"], c("North SD", "South SD"))
  expect_equal(s$List_Key[s$Board == "Statewide"], c("cook", "aide"))
  expect_equal(nrow(got_known), 0)
  # The next day the feed is handed its own stretches, and only those.
  refresh_presence(dir, "Good", feeds = list(feed), today = day + 1, sources = sources, delay = 0, envir = env)
  expect_equal(sort(got_known$Link_Key), c("f:1#1", "f:2#1"))
  bad <- presence_feed("Statewide", "FEED", function(known) stop("HTTP 500"))
  refresh_presence(dir, "Good", feeds = list(bad), today = day + 2, sources = sources, delay = 0, envir = env)
  checks <- read.csv(file.path(dir, "presence_checks.csv"))
  expect_equal(checks$Status[checks$Date == format(day + 2) & checks$Board == "Statewide"], "error")
})
