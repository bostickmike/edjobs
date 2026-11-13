# Posting spells (R/posting_spells.R), moved from the MT and WY repos in phase 4
# (2026-11-11). Small panels shaped like real cases in the WY archives
# (2026-11-05) under the default rules, and in the MT archives (2026-11-04)
# under MT's rules as that state sets them (its posting_spells.R).
FEED <- "WSBA statewide feed"
ps_row <- function(run, id, title, district = "Sweetwater County School District 1", posted = "2026-09-01",
                   method = "Source fallback: district + board + title + posted date",
                   source = "applitrack.com/sweetwater", url = "https://www.applitrack.com/sweetwater/onlineapp/default.aspx?all=1",
                   position = "Custodial/Maintenance", approx = FALSE) {
  data.frame(Run_Date = as.Date(run), Posting_ID = id, Posting_Identity_Method = method, District = district,
             title = title, date_posted = posted, Posted_Approx = approx, url = url, Posting_Source = source,
             Is_Feed = source == FEED, position = position, stringsAsFactors = FALSE)
}

test_that("Posted dates parse in every format the boards print", {
  x <- c("2026-10-14", "10/14/2026", "Oct 14, 2026 9:41 AM (UTC)", "Sep 9, 2026", "14-Oct-26",
         "3 days ago(10/27/2026 9:12 AM)", "Posted 2 weeks ago", "Today", "Posted more than 30 days ago",
         "Classified", "\n                10/1/2026\n          ", "2026-11-30")
  p <- parse_board_posted_date(x, "2026-10-30")
  expect_equal(p$date, c("2026-10-14", "2026-10-14", "2026-10-14", "2026-09-09", "2026-10-14", "2026-10-27",
                         "2026-10-16", "2026-10-30", NA, NA, "2026-10-01", NA))   # a date after the run is an error
  expect_equal(p$approx, c(rep(FALSE, 6), TRUE, TRUE, rep(FALSE, 4)))
})

test_that("earlier title-identified rows join the job ID the same posting carries later", {
  p <- rbind(ps_row("2026-10-16", "fb-1", "Night Lead Custodian", posted = "2026-10-01"),
             ps_row("2026-10-23", "fb-1", "Night Lead Custodian", posted = "2026-10-01"),
             ps_row("2026-10-30", "applitrack:sweetwater:9", "Night Lead Custodian", posted = "2026-10-01",
                    method = "Stable per-posting ID"))
  b <- bridge_posting_ids(p)
  expect_equal(unique(b$Posting_ID), "applitrack:sweetwater:9")
  expect_equal(b$Bridged_From[1], "fb-1")
})

test_that("ends are censored on a partial run, a source with no rows, and a move to another board", {
  big <- do.call(rbind, lapply(1:12, function(i) ps_row("2026-07-31", paste0("u", i), paste("Job", i),
                                                       district = "University of Wyoming", source = "University of Wyoming")))
  small <- ps_row("2026-08-04", "u1", "Job 1", district = "University of Wyoming", source = "University of Wyoming")
  p <- rbind(big, small,
             # WSBA listed it, then the district's own board did.
             ps_row("2026-07-31", "w", "Science Teacher", district = "Lincoln County School District 1", source = FEED,
                    url = "https://www.wsba-wy.org/vacancies"),
             ps_row("2026-08-04", "own", "Science Teacher", district = "Lincoln County School District 1",
                    source = "lincolnwy.schoolspring.com"),
             # Gone while the board was healthy: a real ending.
             ps_row("2026-07-31", "a", "Custodian"), ps_row("2026-07-31", "b", "Cook"), ps_row("2026-08-04", "b", "Cook"),
             ps_row("2026-08-11", "b", "Cook"))
  sp <- build_posting_spells(p)
  e <- setNames(sp$Ending, sp$Posting_ID)
  expect_equal(unname(e["u2"]), "censored: source partial")     # 12 rows then 1
  expect_equal(unname(e["w"]), "censored: moved boards")
  expect_equal(unname(e["a"]), "no longer listed")
  expect_equal(unname(e["own"]), "censored: source no_rows")    # its board returned nothing on 08-11
  expect_equal(sp$End_Date[sp$Posting_ID == "a"], "2026-08-02")
})

test_that("SchoolSpring endings come from the posting's record", {
  p <- rbind(ps_row("2026-10-23", "schoolspring:5569023", "Golf Coach", source = "acsd1.schoolspring.com",
                    method = "Stable per-posting ID", url = "https://acsd1.schoolspring.com/jobs/5569023"),
             ps_row("2026-10-23", "x", "Other job", source = "acsd1.schoolspring.com"),
             ps_row("2026-10-30", "x", "Other job", source = "acsd1.schoolspring.com"))
  sp <- build_posting_spells(p, endings = data.frame(job_id = "5569023", Job_Status = "filled"))
  expect_equal(sp$Ending[sp$Posting_ID == "schoolspring:5569023"], "filled")
})

test_that("frozen boards are found, broken by a missing run, and their postings flagged", {
  runs <- seq(as.Date("2024-10-18"), by = 7, length.out = 14)
  titles <- paste("Teacher", 1:5)
  p <- do.call(rbind, lapply(runs, function(r) do.call(rbind, lapply(titles, function(t)
    ps_row(format(r), paste0("fb-", t), t, district = "Fremont County School District 38", source = "applitrack.com/fcsd38")))))
  # Another board, active every run, so the schedule has all 14 runs.
  p <- rbind(p, do.call(rbind, lapply(seq_along(runs), function(i) ps_row(format(runs[i]), paste0("o", i), paste("Opening", i)))))
  f <- frozen_board_runs(p)
  expect_equal(nrow(f), 1)
  expect_equal(f$Runs, 14L)
  sp <- build_posting_spells(p, frozen = f)
  expect_true(all(sp$Frozen_Board[sp$District == "Fremont County School District 38"]))
  expect_false(any(sp$In_Durations[sp$District == "Fremont County School District 38"]))
  # A run with no rows from that board splits the stretch: 6 + 7 runs, neither long enough.
  expect_equal(nrow(frozen_board_runs(p[!(p$District == "Fremont County School District 38" & p$Run_Date == runs[7]), ])), 0)
  # Fewer than FROZEN_MIN_POSTINGS postings: a small district's unchanged board may be real.
  expect_equal(nrow(frozen_board_runs(p[!(p$title %in% titles[1:2]), ])), 0)
})

test_that("only indistinguishable same-title postings are shared; pools and standing ads are flagged", {
  p <- rbind(ps_row("2026-08-21", "fb-x#1", "Building Paraprofessional", posted = NA),
             ps_row("2026-08-21", "fb-x#2", "Building Paraprofessional", posted = NA),
             ps_row("2026-08-21", "fb-y#1", "Special Education Teacher", posted = "2026-08-01"),
             ps_row("2026-08-21", "fb-z#1", "Special Education Teacher", posted = "2026-08-02"),
             ps_row("2026-08-21", "p", "Adjunct Faculty - Biology"),
             ps_row("2026-08-21", "s", "2026-2027 Substitute Teacher - Part-Time"),
             ps_row("2026-08-28", "fb-y#1", "Special Education Teacher", posted = "2026-08-01"))
  sp <- build_posting_spells(p)
  r <- function(id, col) sp[[col]][sp$Posting_ID == id]
  expect_true(r("fb-x#1", "Shared_Title") && r("fb-x#2", "Shared_Title"))
  expect_false(r("fb-y#1", "Shared_Title") || r("fb-z#1", "Shared_Title"))
  expect_true(r("p", "Pool")); expect_false(r("p", "In_Durations"))
  expect_true(r("s", "Evergreen"))
  expect_equal(r("fb-y#1", "School_Year"), "2026-27")
})

test_that("a first sighting after a run with no rows from the board is left-censored", {
  p <- rbind(ps_row("2026-08-04", "a", "Cook"), ps_row("2026-08-04", "z", "Aide", source = "other.org"),
             ps_row("2026-08-07", "z", "Aide", source = "other.org"),
             ps_row("2026-08-11", "c", "Bus Driver", posted = NA))
  sp <- build_posting_spells(p)
  expect_true(sp$Left_Censored[sp$Posting_ID == "c"])
})

test_that("start prefers an exact posted date over a relative one", {
  p <- rbind(ps_row("2026-10-23", "u", "Analyst", posted = "2026-10-09", approx = TRUE),
             ps_row("2026-10-30", "u", "Analyst", posted = "2026-10-12", approx = FALSE))
  q <- use_earliest_posted_date(p)
  expect_equal(unique(q$date_posted), "2026-10-12")
  expect_false(any(q$Posted_Approx))
})

test_that("Kaplan-Meier medians use delayed entry and need enough endings", {
  mk <- function(before, days) data.frame(In_Durations = TRUE, Days_Before_First_Seen = before,
                                          Days_Listed = days, Event = TRUE, Position = "Teacher")
  s <- rbind(mk(rep(0, 30), rep(10, 30)), mk(rep(200, 30), rep(210, 30)))
  expect_equal(summarize_posting_durations(s)$Median_Days, 10)
  expect_true(is.na(summarize_posting_durations(s[1:5, ])$Median_Days))
})

test_that("the drift report lists only boards still frozen this run", {
  f <- data.frame(District = c("Fremont County School District 38", "Weston County School District 7"),
                  Posting_Source = c("applitrack.com/fcsd38", "weston7.org"), From = c("2024-10-18", "2024-10-18"),
                  To = c("2026-10-30", "2025-01-10"), Runs = c(57L, 13L), Postings = c(14L, 10L),
                  Ongoing = c(TRUE, FALSE), Segment = "K-12", stringsAsFactors = FALSE)
  md <- build_frozen_board_report_markdown(f, date = "2026-11-06")
  expect_true(any(grepl("Fremont County School District 38\\*\\* \\(applitrack.com/fcsd38\\).*14 posting\\(s\\) for 57 weeks", md)))
  expect_false(any(grepl("Weston", md)))
  expect_length(build_frozen_board_report_markdown(f[2, ]), 0)
})

test_that("a posting's own page confirms or overrules its ending, and dates it by a stated closing date", {
  at <- function(id) paste0("https://www.applitrack.com/sweetwater/onlineapp/default.aspx?AppliTrackLayoutMode=detail&AppliTrackJobId=", id)
  p <- rbind(ps_row("2026-10-23", "applitrack:sweetwater:1", "Cook", method = "Stable per-posting ID", url = at(1), posted = "2026-10-01"),
             ps_row("2026-10-23", "applitrack:sweetwater:2", "Aide", method = "Stable per-posting ID", url = at(2), posted = "2026-10-01"),
             ps_row("2026-10-23", "applitrack:sweetwater:3", "Clerk", method = "Stable per-posting ID", url = at(3), posted = "2026-10-01"),
             ps_row("2026-10-30", "applitrack:sweetwater:4", "Nurse", method = "Stable per-posting ID", url = at(4)))
  p$Close_Date <- c("2026-10-27", NA, NA, NA)
  st <- data.frame(Check_URL = posting_status_check_url(at(1:3)), Status = c("closed", "open", "closed"),
                   Status_Since = c("2026-10-30", NA, "2026-10-20"), Stated_Close_Date = NA,
                   Last_Checked = c("2026-10-30", "2026-10-30", "2026-10-20"), stringsAsFactors = FALSE)
  sp <- build_posting_spells(p, statuses = st)
  r <- function(id, col) sp[[col]][sp$Posting_ID == paste0("applitrack:sweetwater:", id)]
  expect_equal(r(1, "Ending"), "closed (confirmed)")
  expect_equal(r(1, "End_Date"), "2026-10-27"); expect_equal(r(1, "End_Basis"), "stated closing date")
  expect_equal(r(1, "Advertised_Days"), 26)
  expect_equal(r(2, "Ending"), "censored: still open on its page"); expect_false(r(2, "Event"))
  # A check from before the last sighting says nothing about this spell.
  expect_equal(r(3, "Ending"), "no longer listed"); expect_equal(r(3, "End_Basis"), "midpoint to next run")
})

test_that("a posting relisted under a new ID the next run is 'outcome unknown', not an ending", {
  # 2026-11-09: of 23 WY SchoolSpring relistings whose old record has a final
  # status, 8 were "filled" (a new vacancy with the same title), so relistings
  # are neither linked nor counted as endings.
  d <- c("Albany County School District 1", "Albany County School District 1", "Albany County School District 1",
         "Albany County School District 1", "Laramie County School District 1")
  src <- c("acsd1", "acsd1", "acsd1", "other board", "acsd1")
  title <- c("Mechanic", " mechanic ", "Elementary Teacher", "Mechanic", "Mechanic")
  first <- as.Date(c("2026-03-06", "2026-04-17", "2026-04-17", "2026-04-17", "2026-04-17"))
  nxt <- as.Date(c("2026-04-17", NA, NA, NA, NA))
  # Same district, source and title (case and spacing aside), first seen the old one's next run.
  expect_equal(relisted_next_run(d, src, title, first, nxt), c(TRUE, FALSE, FALSE, FALSE, FALSE))
  # Punctuation aside too (WY spells audit 2026-11-10: 41 of these were counted as endings).
  expect_true(relisted_next_run(d[1:2], src[1:2], c("Paraeducator- SPED Related Services", "Paraeducator - SPED Related Services"),
                                first[1:2], nxt[1:2])[1])
  expect_true(relisted_next_run(d[1:2], src[1:2], c("Special Ed, Para Professional", "Special Ed. Para Professional"),
                                first[1:2], nxt[1:2])[1])
  # Words still have to match.
  expect_false(relisted_next_run(d[1:2], src[1:2], c("Para Professional", "Paraprofessional II"), first[1:2], nxt[1:2])[1])
  # A different title, source or district, or a later run, is not a relisting.
  expect_equal(relisted_next_run(d[c(1, 3)], src[c(1, 3)], c("Custodian", "Elementary Teacher"), first[c(1, 3)],
                                 as.Date(c("2026-04-17", NA))), c(FALSE, FALSE))
  expect_false(relisted_next_run(d[1], src[1], title[1], first[1], as.Date("2026-04-24")))
  expect_false(relisted_next_run(d[1], src[1], title[1], first[1], NA))
})

# ---- MT's rules (Montana-Education-Jobs-Dashboard/posting_spells.R) ----
mt_rules <- function(shared_titles = "all") {
  spell_rules(feed_entity = "OPI statewide feed", feed_in_durations = FALSE, shared_titles = shared_titles,
              partial_min_previous = Inf, moved_boards = FALSE, left_censor_after_gap = FALSE,
              evergreen_pattern = paste0("\\bsub(stitute)?s?\\b|substitute|\\bpool\\b|on[- ]call|always accepting|",
                                         "accepting applications|ongoing|continuous|general application|interest (form|list)|^copy of\\b"),
              pool_pattern = NA_character_)
}
mt_row <- function(run, id, title, district = "Billings Public Schools", posted = "2026-09-01",
                   method = "Source fallback: title + location + posted date (no stable posting URL)",
                   source = "AppliTrack (direct district board)", url = "https://www.applitrack.com/billings/onlineapp/default.aspx",
                   opi = FALSE, position = "Custodial/Maintenance") {
  data.frame(Run_Date = as.Date(run), Posting_ID = id, Posting_Identity_Method = method, District = district,
             title = title, date_posted = posted, url = url, Posting_Source = source, Is_Feed = opi,
             position = position, Bridgeable = TRUE, stringsAsFactors = FALSE)
}
mt_cal <- function(run, entity, status) data.frame(Run_Date = run, Segment = "K-12", Entity = entity, Source = entity,
                                                   Status = status, N_Rows = 1L, Error = NA_character_)

test_that("MT rules: earlier title-identified rows join the job ID the same posting carries later", {
  p <- rbind(mt_row("2026-10-20", "fb-1", "Custodian III"), mt_row("2026-10-27", "fb-1", "Custodian III"),
             mt_row("2026-11-03", "url-9", "Custodian III", method = "Stable per-posting URL",
                    url = "https://www.applitrack.com/billings/onlineapp/default.aspx?AppliTrackJobId=9"))
  b <- bridge_posting_ids(p, mt_rules())
  expect_equal(unique(b$Posting_ID), "url-9")
  expect_equal(b$Bridged_From[1], "fb-1")
})

test_that("MT rules: an ID change between consecutive runs is bridged only when the posted date allows it", {
  # Sidney, 11-03: the parser fix filled a posted date that was blank before.
  p <- rbind(mt_row("2026-10-27", "fb-a", "Library Media Specialist", district = "Sidney Public Schools", posted = NA),
             mt_row("2026-11-03", "url-a", "Library Media Specialist", district = "Sidney Public Schools",
                    posted = "2026-05-01", method = "Stable per-posting URL"),
             # Billings "Custodian III": posted 08-04, a new one posted 08-18 is a real repost.
             mt_row("2026-08-18", "fb-b", "Custodian III", posted = "2026-08-04"),
             mt_row("2026-08-25", "fb-c", "Custodian III", posted = "2026-08-18"))
  b <- bridge_posting_ids(p, mt_rules())
  expect_equal(b$Posting_ID[b$title == "Library Media Specialist"], c("url-a", "url-a"))
  expect_equal(sort(unique(b$Posting_ID[b$title == "Custodian III"])), c("fb-b", "fb-c"))
})

test_that("MT rules: an ending counts only when the source was healthy the next run", {
  p <- rbind(mt_row("2026-10-20", "a", "Custodian"), mt_row("2026-10-20", "b", "Cook", district = "Laurel Public Schools"),
             mt_row("2026-10-20", "c", "Bus Driver", district = "Belgrade School District 44", source = "SchoolSpring (direct district board)",
                    url = "https://belgrade.schoolspring.com/jobs/5785656"),
             mt_row("2026-10-27", "d", "Secretary"), mt_row("2026-10-27", "e", "Cook II", district = "Laurel Public Schools"),
             mt_row("2026-10-27", "f", "Aide", district = "Belgrade School District 44", source = "SchoolSpring (direct district board)",
                    url = "https://belgrade.schoolspring.com/jobs/1"))
  cal <- rbind(mt_cal("2026-10-20", "Billings Public Schools", "ok"), mt_cal("2026-10-27", "Billings Public Schools", "ok"),
               mt_cal("2026-10-27", "Laurel Public Schools", "error"), mt_cal("2026-10-27", "Belgrade School District 44", "error"))
  end <- data.frame(job_id = "5785656", Job_Status = "filled")
  sp <- build_posting_spells(p, cal, end, rules = mt_rules())
  e <- setNames(sp$Ending, sp$Posting_ID)
  expect_equal(unname(e[c("a", "b", "c", "d")]),
               c("no longer listed", "censored: source error", "filled", "still listed"))
  expect_equal(sp$Event[sp$Posting_ID %in% c("a", "b", "c", "d")][order(sp$Posting_ID[sp$Posting_ID %in% c("a", "b", "c", "d")])],
               c(TRUE, FALSE, TRUE, FALSE))
  # Ending dated at the midpoint between last sighting and the next run.
  expect_equal(sp$End_Date[sp$Posting_ID == "a"], "2026-10-23")
})

test_that("MT rules: start date, left-censoring, standing ads and shared titles are flagged", {
  p <- rbind(mt_row("2026-08-18", "a", "Custodian", posted = "2026-08-10"),
             mt_row("2026-08-18", "b", "Cook", posted = NA),                               # first week, no date
             mt_row("2026-08-25", "c", "Bus Driver", posted = NA),                         # later week, no date
             mt_row("2026-08-25", "d", "Paraprofessional", posted = "2022-07-14"),         # a 2022 standing ad
             mt_row("2026-08-25", "e", "Substitute Teacher", posted = "2026-08-20"),
             mt_row("2026-08-25", "f", "Special Education Teacher", posted = "2026-08-01"),
             mt_row("2026-08-25", "g", "Special Education Teacher", posted = "2026-08-02"))
  sp <- build_posting_spells(p, rules = mt_rules())
  r <- function(id, col) sp[[col]][sp$Posting_ID == id]
  expect_equal(r("a", "Start_Basis"), "board posted date"); expect_equal(r("a", "Start_Date"), "2026-08-10")
  expect_true(r("b", "Left_Censored")); expect_false(r("c", "Left_Censored"))
  expect_true(r("d", "Evergreen")); expect_true(r("e", "Evergreen")); expect_false(r("a", "Evergreen"))
  expect_true(r("f", "Shared_Title") && r("g", "Shared_Title"))
  expect_equal(sp$Posting_ID[sp$In_Durations], c("a", "c"))
})

test_that("MT rules: a re-uploaded posting document is bridged; a new PeopleAdmin number is not", {
  p <- rbind(mt_row("2026-09-29", "url:old.pdf", "NACTEP Project Assistant", district = "Aaniiih Nakoda College",
                    posted = NA, method = "Stable per-posting URL"),
             mt_row("2026-10-06", "url:new.pdf", "NACTEP Project Assistant", district = "Aaniiih Nakoda College",
                    posted = NA, method = "Stable per-posting URL"),
             mt_row("2026-09-29", "url:postings/1", "Adjunct Instructor", district = "Montana State University",
                    posted = NA, method = "Stable per-posting URL"),
             mt_row("2026-10-06", "url:postings/2", "Adjunct Instructor", district = "Montana State University",
                    posted = NA, method = "Stable per-posting URL"))
  p$Bridgeable <- c(TRUE, TRUE, FALSE, FALSE)
  b <- bridge_posting_ids(p, mt_rules())
  expect_equal(unique(b$Posting_ID[b$District == "Aaniiih Nakoda College"]), "url:new.pdf")
  expect_equal(sort(unique(b$Posting_ID[b$District == "Montana State University"])), c("url:postings/1", "url:postings/2"))
})

test_that("MT rules: HE start is the earliest posted date shown, and shared titles count only for title IDs", {
  # Great Falls: the feed date moved forward when the posting was re-published.
  p <- rbind(mt_row("2026-10-20", "url:gf/1", "Adjunct Instructor", district = "Great Falls College MSU",
                    posted = "2026-10-01", method = "Stable per-posting URL"),
             mt_row("2026-10-27", "url:gf/1", "Adjunct Instructor", district = "Great Falls College MSU",
                    posted = "2026-10-25", method = "Stable per-posting URL"),
             mt_row("2026-10-27", "url:gf/2", "Adjunct Instructor", district = "Great Falls College MSU",
                    posted = "2026-10-26", method = "Stable per-posting URL"),
             mt_row("2026-10-27", "fb-1", "Instructor", district = "Carroll College", posted = NA),
             mt_row("2026-10-27", "fb-2", "Instructor", district = "Carroll College", posted = NA))
  p <- use_earliest_posted_date(p)
  expect_equal(p$date_posted[p$Posting_ID == "url:gf/1"], c("2026-10-01", "2026-10-01"))
  sp <- build_posting_spells(p, rules = mt_rules("fallback only"))
  expect_equal(sp$Shared_Title[order(sp$Posting_ID)], c(TRUE, TRUE, FALSE, FALSE))
})

test_that("MT rules: a posting's own page confirms or overrules its ending, and dates it by a stated closing date", {
  at <- function(id) paste0("https://www.applitrack.com/billings/onlineapp/default.aspx?AppliTrackLayoutMode=detail&AppliTrackJobId=", id)
  p <- rbind(mt_row("2026-10-27", "url-1", "Cook", method = "Stable per-posting URL", url = at(1), posted = "2026-10-01"),
             mt_row("2026-10-27", "url-2", "Aide", method = "Stable per-posting URL", url = at(2), posted = "2026-10-01"),
             mt_row("2026-11-03", "url-4", "Nurse", method = "Stable per-posting URL", url = at(4)))
  p$Close_Date <- c("2026-10-30", NA, NA)
  cal <- rbind(mt_cal("2026-10-27", "Billings Public Schools", "ok"), mt_cal("2026-11-03", "Billings Public Schools", "ok"))
  st <- data.frame(Check_URL = posting_status_check_url(at(1:2)), Status = c("closed", "open"), Stated_Close_Date = NA,
                   Last_Checked = "2026-11-03", stringsAsFactors = FALSE)
  sp <- build_posting_spells(p, cal, statuses = st, rules = mt_rules())
  r <- function(id, col) sp[[col]][sp$Posting_ID == id]
  expect_equal(r("url-1", "Ending"), "closed (confirmed)")
  expect_equal(r("url-1", "End_Date"), "2026-10-30"); expect_equal(r("url-1", "Advertised_Days"), 29)
  expect_equal(r("url-2", "Ending"), "censored: still open on its page")
})

test_that("every rule set writes the same columns, so states' spells stack", {
  p <- rbind(ps_row("2026-10-23", "a", "Cook"), ps_row("2026-10-30", "b", "Aide", source = FEED))
  q <- p; q$Posted_Approx <- NULL; q$Is_Feed <- NULL
  for (r in list(spell_rules(), mt_rules(), mt_rules("fallback only")))
    for (x in list(p, q)) expect_identical(names(build_posting_spells(bridge_posting_ids(x, r), rules = r)), POSTING_SPELL_COLUMNS)
  expect_error(spell_rules(shared_titles = "some"))
})

# ---- Phase 4 review fixes (2026-11-13) ---------------------------------------
test_that("a stable ID that absorbed a fallback one never moves on: two requisition numbers stay two postings", {
  stable <- "Stable per-posting ID"
  p <- rbind(ps_row("2026-10-16", "fbA", "Para", posted = NA),
             ps_row("2026-10-23", "req:100", "Para", posted = NA, method = stable),
             ps_row("2026-10-30", "req:200", "Para", posted = NA, method = stable))
  b <- bridge_posting_ids(p)
  expect_equal(b$Posting_ID, c("req:100", "req:100", "req:200"))
  expect_equal(b$Bridged_From, c("fbA", NA, NA))
})

test_that("a fallback chain still bridges, and Bridged_From keeps the first ID the posting had", {
  p <- rbind(ps_row("2026-10-16", "fbA", "Para", posted = NA),
             ps_row("2026-10-23", "fbB", "Para", posted = NA),
             ps_row("2026-10-30", "req:1", "Para", posted = NA, method = "Stable per-posting ID"))
  b <- bridge_posting_ids(p)
  expect_equal(unique(b$Posting_ID), "req:1")
  expect_equal(b$Bridged_From, c("fbA", "fbB", NA))
})

test_that("a blank posted date is no date: fallback and stable rows never bridge on title alone", {
  p <- rbind(ps_row("2026-10-16", "fb-1", "Cook", posted = ""),
             ps_row("2026-10-23", "other", "Bus Driver"),
             ps_row("2026-10-30", "req:9", "Cook", posted = "", method = "Stable per-posting ID"))
  b <- bridge_posting_ids(p)
  expect_equal(b$Posting_ID[b$title == "Cook"], c("fb-1", "req:9"))
})

test_that("card text in a posted date ('Posted 3 weeks ago') is no date, never an error", {
  p <- rbind(ps_row("2026-10-16", "a", "Cook", posted = "Posted 3 weeks ago"),
             ps_row("2026-10-23", "a", "Cook", posted = "2026-10-01"))
  expect_equal(unique(use_earliest_posted_date(p)$date_posted), "2026-10-01")
  q <- ps_row("2026-10-16", "b", "Cook", posted = "Posted today")
  expect_true(is.na(use_earliest_posted_date(q)$date_posted))
  expect_identical(names(build_posting_spells(bridge_posting_ids(q))), POSTING_SPELL_COLUMNS)
})

test_that("Frozen_Board uses the min_runs the frozen table was built with", {
  runs <- format(as.Date("2026-10-02") + 7 * 0:3)
  p <- do.call(rbind, lapply(runs, function(r) ps_row(r, "a", "Cook")))
  frozen <- data.frame(District = p$District[1], Posting_Source = p$Posting_Source[1], From = runs[1], To = runs[4],
                       Runs = 4L, Postings = 1L, Ongoing = TRUE, stringsAsFactors = FALSE)
  expect_false(build_posting_spells(p, frozen = frozen)$Frozen_Board)
  expect_true(build_posting_spells(p, frozen = frozen, frozen_min_runs = 4)$Frozen_Board)
})

test_that("spell_text and spell_key drop zero-width characters and extra spaces", {
  expect_equal(spell_text(c("Facilities Manager​", "  Math   Teacher ", NA)), c("Facilities Manager", "Math Teacher", NA))
  expect_equal(spell_key(c("Math  TEACHER", NA)), c("math teacher", ""))
})

test_that("parse_stated_close_date reads every closing-date form the boards print", {
  expect_equal(parse_stated_close_date(c("2026-11-09", "11/9/2026", "Nov 9, 2026", "Open Until Filled", NA)),
               c("2026-11-09", "2026-11-09", "2026-11-09", NA, NA))
})

test_that("source_health reads the calendar where it covers the run, and the panel before that", {
  p <- rbind(ps_row("2026-10-16", "a", "Cook"), ps_row("2026-10-23", "b", "Para"))
  d <- p$District[1]; s <- p$Posting_Source[1]
  cal <- data.frame(Run_Date = "2026-10-30", Entity = d, Status = "error", stringsAsFactors = FALSE)
  expect_equal(source_health(p, cal, d, s, FALSE, as.Date("2026-10-30")), "error")
  expect_equal(source_health(p, NULL, d, s, FALSE, as.Date("2026-10-23")), "ok_proxy")
  expect_equal(source_health(p, NULL, d, s, FALSE, as.Date("2026-11-06")), "no_rows")
  expect_equal(source_health(p, cal, "Elsewhere", s, FALSE, as.Date("2026-10-30")), "not_logged")
  expect_true(is.na(source_health(p, cal, d, s, FALSE, NA)))
})

test_that("first_seen_lookup keys the latest run's postings the way the app shows them", {
  p <- rbind(ps_row("2026-10-16", "a", "Cook"), ps_row("2026-10-23", "a", "Cook  "), ps_row("2026-10-23", "b", "Para"))
  sp <- data.frame(Posting_ID = c("a", "b"), First_Seen = as.Date(c("2026-10-16", "2026-10-23")),
                   Left_Censored = c(TRUE, FALSE))
  fs <- first_seen_lookup(p, sp, "K-12")
  expect_equal(fs$Key, first_seen_key(p$District[2:3], c("Cook", "Para"), p$url[2:3]))
  expect_equal(fs$First_Seen, c("2026-10-16", "2026-10-23"))
  expect_equal(fs$Left_Censored, c(TRUE, FALSE))
  expect_equal(first_seen_key(" A  B ", "Math  Teacher", " u "), "a b | math teacher | u")
})
