# Drift checks that differed between MT and WY until phase 4 (R/drift_check.R,
# "Drift checks that differed"). WY's cases as they were; MT's cases under an
# MT-like state config and MT's flag_drift() floor (min_mean_count = 0).
WSBA_STATEWIDE_SOURCE <- "WSBA statewide vacancies"
wy_feed <- list(short_name = "WSBA", label = WSBA_STATEWIDE_SOURCE, url = "https://www.wsba-wy.org/vacancies",
                discrepancy_file = "wy_discrepancy_report.csv")
OPI_STATEWIDE_SOURCE <- "OPI Jobs for Teachers (statewide)"
mt_feed <- list(short_name = "OPI", label = OPI_STATEWIDE_SOURCE,
                url = "https://apps.opi.mt.gov/mtjobsforteachers/frmJobListingPublic.aspx",
                discrepancy_file = "mt_discrepancy_report.csv")
WSBA_VACANCIES_URL <- "https://www.wsba-wy.org/vacancies"
# The states' drift baselines start on their own dates (WY 2026-08-03, MT 2026-08-06).
with_feed <- function(feed, env = parent.frame()) {
  old <- current_state_config()
  from <- if (identical(feed$short_name, "WSBA")) "2026-08-03" else "2026-08-06"
  set_state_config(state_config("Test", "00", "app", feed = feed, drift_baseline_from = from))
  withr::defer(set_state_config(old), envir = env)
}

test_that("WY: build_historical_counts excludes pre-fix snapshots and computes correct means", {
  with_feed(wy_feed)
  snapshots <- list(
    "2026-08-03" = data.frame(District = c("A", "A", "A", "B")),
    "2026-08-10" = data.frame(District = c("A", "A", "B", "B")),
    "2026-08-17" = data.frame(District = c("A", "A", "A")),
    # Predates the Applitrack encoding fix -- must be excluded, or the fix
    # itself would register as suspicious drift on every affected district.
    "2026-02-27" = data.frame(District = c("A", "A", "A", "A", "A", "A", "A", "A", "A", "A"))
  )

  baseline <- build_historical_counts(snapshots, "District")

  expect_equal(baseline$n_weeks[baseline$name == "A"], 3)
  expect_equal(baseline$mean_count[baseline$name == "A"], (3 + 2 + 3) / 3)
  # B had no rows on the third week, after its last week with rows: left out
  # (it may be an outage starting), so its baseline is its two weeks.
  expect_equal(baseline$n_weeks[baseline$name == "B"], 2)
  expect_equal(baseline$mean_count[baseline$name == "B"], (1 + 2) / 2)
})

test_that("WY: build_historical_counts returns an empty frame when no snapshots are post-fix", {
  with_feed(wy_feed)
  snapshots <- list("2026-02-27" = data.frame(District = c("A", "A")))
  baseline <- build_historical_counts(snapshots, "District")
  expect_equal(nrow(baseline), 0)
})

test_that("WY: flag_drift flags a real drop and leaves a stable source alone", {
  with_feed(wy_feed)
  baseline <- data.frame(
    name = c("BigDistrict", "StableDistrict"),
    n_weeks = c(5, 5),
    mean_count = c(50, 10)
  )
  current <- data.frame(name = c("BigDistrict", "StableDistrict"), count = c(0, 9))

  flagged <- flag_drift(current, baseline)

  expect_equal(flagged$name, "BigDistrict")
})

test_that("WY: flag_drift exempts sources whose historical average is below the noise floor", {
  with_feed(wy_feed)
  baseline <- data.frame(
    name = c("TinySource", "RealSource"),
    n_weeks = c(6, 6),
    mean_count = c(2, 12)
  )
  current <- data.frame(name = c("TinySource", "RealSource"), count = c(0, 0))

  flagged <- flag_drift(current, baseline)

  # TinySource averaged 2/week -- dropping to 0 is churn, not a parser
  # failure. RealSource averaged 12 and is still flagged.
  expect_equal(flagged$name, "RealSource")
})

test_that("WY: flag_drift requires a minimum number of historical weeks before flagging", {
  with_feed(wy_feed)
  baseline <- data.frame(name = "BrandNewDistrict", n_weeks = 1, mean_count = 10)
  current <- data.frame(name = "BrandNewDistrict", count = 0)

  flagged <- flag_drift(current, baseline, min_weeks = 2)

  expect_equal(nrow(flagged), 0)
})

test_that("WY: flag_drift treats a source missing from current data as a count of zero", {
  with_feed(wy_feed)
  baseline <- data.frame(name = "VanishedDistrict", n_weeks = 3, mean_count = 20)
  current <- data.frame(name = character(0), count = numeric(0))

  flagged <- flag_drift(current, baseline)

  expect_equal(flagged$name, "VanishedDistrict")
  expect_equal(flagged$count, 0)
})

test_that("WY: summarize_live_check fails on an error or zero rows and passes otherwise", {
  with_feed(wy_feed)
  expect_false(summarize_live_check("X", simpleError("HTTP 520."))$pass)
  expect_false(summarize_live_check("X", data.frame(Title = character(0)))$pass)

  ok <- summarize_live_check("X", data.frame(Title = c("Route Bus Drivers", "Cook")),
                             expected_titles = c("route bus drivers", "Daycare Manager"))
  expect_true(ok$pass)
  md <- paste(ok$markdown, collapse = "\n")
  expect_match(md, "Matched 1 of 2", fixed = TRUE)
  expect_match(md, "- Daycare Manager", fixed = TRUE)
})

test_that("WY: summarize_live_check reads Applitrack/TedK12's lowercase title column", {
  with_feed(wy_feed)
  # Before scraper_titles(), a fixed Applitrack scraper read as "0 postings".
  ok <- summarize_live_check("X", data.frame(title = c("Custodian", "Para")))
  expect_true(ok$pass)
  expect_match(paste(ok$markdown, collapse = "\n"), "2 posting(s)", fixed = TRUE)
})


test_that("WY: summarize_central_safety_net handles empty or NULL inputs gracefully", {
  with_feed(wy_feed)
  summary <- summarize_central_safety_net()
  expect_equal(summary$total_feed_vacancies, 0L)
  expect_equal(summary$total_fallback_vacancies, 0L)
  expect_equal(length(summary$fallback_districts), 0)
  expect_equal(nrow(summary$discrepancies), 0)
  expect_true(any(grepl("Counted from WSBA at directly scraped districts**: none", summary$markdown, fixed = TRUE)))
})

test_that("WY: summarize_central_safety_net identifies active fallback districts and counts", {
  with_feed(wy_feed)
  registry <- data.frame(
    District = c("Fremont County School District 24", "Park County School District 1", "Natrona School District 1"),
    stringsAsFactors = FALSE
  )
  combined <- data.frame(
    District = c("Fremont County School District 24", "Fremont County School District 24", "Park County School District 1", "Unregistered Town", "Natrona School District 1"),
    url = c(
      WSBA_VACANCIES_URL,
      WSBA_VACANCIES_URL,
      WSBA_VACANCIES_URL,
      WSBA_VACANCIES_URL,
      "https://natrona.schoolspring.com"
    ),
    posting_id = c("wsba1", "wsba2", "wsba3", "wsba4", "direct1"),
    stringsAsFactors = FALSE
  )

  summary <- summarize_central_safety_net(
    combined_df = combined,
    registry = registry,
    feed_label = WSBA_STATEWIDE_SOURCE,
    feed_short_name = "WSBA"
  )

  expect_equal(summary$total_feed_vacancies, 4L)
  expect_equal(summary$total_fallback_vacancies, 3L)
  expect_equal(summary$fallback_districts[["Fremont County School District 24"]], 2L)
  expect_equal(summary$fallback_districts[["Park County School District 1"]], 1L)
  expect_null(summary$fallback_districts[["Natrona School District 1"]])

  md <- paste(summary$markdown, collapse = "\n")
  expect_match(md, "3 posting(s) at 2 district(s), not on the district's own board", fixed = TRUE)
  expect_match(md, "Fremont County School District 24**: 2", fixed = TRUE)
  expect_match(md, "Park County School District 1**: 1", fixed = TRUE)
})

test_that("WY: summarize_central_safety_net reads discrepancy report for unmatched postings", {
  with_feed(wy_feed)
  disc <- data.frame(
    District = c("Laramie County School District 1", "Natrona School District 1"),
    Source = c("WSBA", "WSBA"),
    Title = c("Math Teacher", "Custodian (stale)"),
    Date_Posted = c("2026-10-25", "2026-05-01"),
    Discrepancy_Type = c("Unmatched on direct scraper (investigate ATS drift)", "Potential ghost posting on WSBA"),
    stringsAsFactors = FALSE
  )

  summary <- summarize_central_safety_net(
    discrepancy_report = disc,
    feed_short_name = "WSBA"
  )

  expect_equal(nrow(summary$discrepancies), 1)
  expect_equal(summary$discrepancies$District, "Laramie County School District 1")
  md <- paste(summary$markdown, collapse = "\n")
  expect_match(md, "1 unmatched vacancy/vacancies detected on central feed", fixed = TRUE)
  expect_match(md, "Laramie County School District 1**: \"Math Teacher\" (posted 2026-10-25)", fixed = TRUE)
})

test_that("WY: build_drift_report_markdown includes Central Safety Net status annotations and summary", {
  with_feed(wy_feed)
  actionable <- data.frame(
    name = c("Fremont County School District 24", "Park County School District 1", "University of Wyoming"),
    type = c("K-12", "K-12", "Higher Ed"),
    mean_count = c(5.0, 10.0, 30.0),
    count = c(0, 0, 2),
    url = c("https://fremont24.com", "https://park1.net", "https://uwyo.edu"),
    verdict = c("confirmed_broken", "likely_broken", "likely_broken"),
    error_message = c("500 Server Error", NA, NA),
    llm_note = c(NA, "an LLM read 4 postings", NA),
    safety_net_vacancies = c(2L, 0L, NA_integer_),
    stringsAsFactors = FALSE
  )

  safety_net_summary <- list(
    total_feed_vacancies = 50L,
    fallback_districts = list("Fremont County School District 24" = 2L),
    total_fallback_vacancies = 2L,
    markdown = c(
      "### WSBA postings at districts we scrape directly",
      "",
      "- **Statewide Feed (WSBA)**: 50 active vacancies captured.",
      "- **Counted from WSBA at directly scraped districts**: 2 posting(s) at 1 district(s), not on the district's own board:",
      "  - **Fremont County School District 24**: 2",
      "- **Central Feed Discrepancies**: 0 unmatched vacancies detected for active direct scrapers.",
      ""
    )
  )

  lines <- build_drift_report_markdown(
    actionable = actionable,
    safety_net_summary = safety_net_summary,
    date = as.Date("2026-10-29"),
    feed_short_name = "WSBA"
  )

  report <- paste(lines, collapse = "\n")
  # Fremont 24: active fallback
  expect_match(report, "Fremont County School District 24", fixed = TRUE)
  expect_match(report, "On WSBA**: 2 postings for this district that its own board doesn't list (counted from WSBA)", fixed = TRUE)
  # Park 1: inactive fallback
  expect_match(report, "Park County School District 1", fixed = TRUE)
  expect_match(report, "On WSBA**: nothing for this district beyond what its own board lists", fixed = TRUE)
  # Higher Ed: no Central Safety Net bullet under its entry
  expect_match(report, "University of Wyoming", fixed = TRUE)
  expect_false(grepl("University of Wyoming.*\\n\\s+- \\*\\*On WSBA", report))
  # Bottom summary block included
  expect_match(report, "### WSBA postings at districts we scrape directly", fixed = TRUE)
  expect_match(report, "Statewide Feed (WSBA)**: 50 active vacancies captured", fixed = TRUE)
})

test_that("MT: build_historical_counts excludes pre-pipeline snapshots and computes correct means", {
  with_feed(mt_feed)
  snapshots <- list(
    "2026-08-06" = data.frame(District = c("A", "A", "A", "B")),
    "2026-08-13" = data.frame(District = c("A", "A", "B", "B")),
    "2026-08-20" = data.frame(District = c("A", "A", "A")),
    # Predates BASELINE_VALID_FROM -- must be excluded.
    "2026-01-01" = data.frame(District = c("A", "A", "A", "A", "A", "A", "A", "A", "A", "A"))
  )

  baseline <- build_historical_counts(snapshots, "District")

  expect_equal(baseline$n_weeks[baseline$name == "A"], 3)
  expect_equal(baseline$mean_count[baseline$name == "A"], (3 + 2 + 3) / 3)
  # B had no rows on the third week, after its last week with rows: left out
  # (it may be an outage starting), so its baseline is its two weeks.
  expect_equal(baseline$n_weeks[baseline$name == "B"], 2)
  expect_equal(baseline$mean_count[baseline$name == "B"], (1 + 2) / 2)
})

test_that("MT: build_historical_counts returns an empty frame when no snapshots are post-baseline", {
  with_feed(mt_feed)
  snapshots <- list("2026-01-01" = data.frame(District = c("A", "A")))
  baseline <- build_historical_counts(snapshots, "District")
  expect_equal(nrow(baseline), 0)
})

test_that("MT: flag_drift flags a real drop and leaves a stable source alone", {
  with_feed(mt_feed)
  baseline <- data.frame(
    name = c("BigDistrict", "StableDistrict"),
    n_weeks = c(5, 5),
    mean_count = c(50, 10)
  )
  current <- data.frame(name = c("BigDistrict", "StableDistrict"), count = c(0, 9))

  flagged <- flag_drift(current, baseline, min_mean_count = 0)

  expect_equal(flagged$name, "BigDistrict")
})

test_that("MT: flag_drift requires a minimum number of historical weeks before flagging", {
  with_feed(mt_feed)
  baseline <- data.frame(name = "BrandNewDistrict", n_weeks = 1, mean_count = 10)
  current <- data.frame(name = "BrandNewDistrict", count = 0)

  flagged <- flag_drift(current, baseline, min_weeks = 2, min_mean_count = 0)

  expect_equal(nrow(flagged), 0)
})

test_that("MT: flag_drift treats a source missing from current data as a count of zero", {
  with_feed(mt_feed)
  baseline <- data.frame(name = "VanishedDistrict", n_weeks = 3, mean_count = 20)
  current <- data.frame(name = character(0), count = numeric(0))

  flagged <- flag_drift(current, baseline, min_mean_count = 0)

  expect_equal(flagged$name, "VanishedDistrict")
  expect_equal(flagged$count, 0)
})

test_that("MT: summarize_live_check fails on an error or zero rows and passes otherwise", {
  with_feed(mt_feed)
  expect_false(summarize_live_check("X", simpleError("HTTP 520."))$pass)
  expect_false(summarize_live_check("X", data.frame(Title = character(0)))$pass)

  ok <- summarize_live_check("X", data.frame(Title = c("Route Bus Drivers", "Cook")),
                             expected_titles = c("route bus drivers", "Daycare Manager"))
  expect_true(ok$pass)
  md <- paste(ok$markdown, collapse = "\n")
  expect_match(md, "Matched 1 of 2", fixed = TRUE)
  expect_match(md, "- Daycare Manager", fixed = TRUE)
})


test_that("MT: summarize_central_safety_net handles empty or NULL inputs gracefully", {
  with_feed(mt_feed)
  summary <- summarize_central_safety_net()
  expect_equal(summary$total_feed_vacancies, 0L)
  expect_equal(summary$total_fallback_vacancies, 0L)
  expect_equal(length(summary$fallback_districts), 0)
  expect_equal(nrow(summary$discrepancies), 0)
  expect_true(any(grepl("Counted from OPI at directly scraped districts**: none", summary$markdown, fixed = TRUE)))
})

test_that("MT: summarize_central_safety_net identifies active fallback districts and counts", {
  with_feed(mt_feed)
  registry <- data.frame(
    District = c("Bozeman Public Schools", "Polson School District", "Billings Public Schools"),
    stringsAsFactors = FALSE
  )
  combined <- data.frame(
    District = c("Bozeman Public Schools", "Bozeman Public Schools", "Polson School District", "Unregistered Town", "Billings Public Schools"),
    Posting_Source = c(
      OPI_STATEWIDE_SOURCE,
      OPI_STATEWIDE_SOURCE,
      OPI_STATEWIDE_SOURCE,
      OPI_STATEWIDE_SOURCE,
      "AppliTrack (direct district board)"
    ),
    stringsAsFactors = FALSE
  )

  summary <- summarize_central_safety_net(
    combined_df = combined,
    registry = registry,
    feed_label = OPI_STATEWIDE_SOURCE,
    feed_short_name = "OPI"
  )

  expect_equal(summary$total_feed_vacancies, 4L)
  expect_equal(summary$total_fallback_vacancies, 3L)
  expect_equal(summary$fallback_districts[["Bozeman Public Schools"]], 2L)
  expect_equal(summary$fallback_districts[["Polson School District"]], 1L)
  expect_null(summary$fallback_districts[["Billings Public Schools"]])

  md <- paste(summary$markdown, collapse = "\n")
  expect_match(md, "3 posting(s) at 2 district(s), not on the district's own board", fixed = TRUE)
  expect_match(md, "Bozeman Public Schools**: 2", fixed = TRUE)
  expect_match(md, "Polson School District**: 1", fixed = TRUE)
})

test_that("MT: summarize_central_safety_net reads discrepancy report for unmatched postings", {
  with_feed(mt_feed)
  disc <- data.frame(
    District = c("Billings Public Schools", "Helena Public Schools"),
    Source = c("OPI", "OPI"),
    Title = c("Math Teacher", "Custodian (stale)"),
    Date_Posted = c("2026-10-25", "2026-05-01"),
    Discrepancy_Type = c("Unmatched on direct scraper (investigate ATS drift)", "Potential ghost posting on OPI"),
    stringsAsFactors = FALSE
  )

  summary <- summarize_central_safety_net(
    discrepancy_report = disc,
    feed_short_name = "OPI"
  )

  expect_equal(nrow(summary$discrepancies), 1)
  expect_equal(summary$discrepancies$District, "Billings Public Schools")
  md <- paste(summary$markdown, collapse = "\n")
  expect_match(md, "1 unmatched vacancy/vacancies detected on central feed", fixed = TRUE)
  expect_match(md, "Billings Public Schools**: \"Math Teacher\" (posted 2026-10-25)", fixed = TRUE)
})

test_that("MT: build_drift_report_markdown includes Central Safety Net status annotations and summary", {
  with_feed(mt_feed)
  actionable <- data.frame(
    name = c("Bozeman Public Schools", "Havre Public Schools", "Montana State University"),
    type = c("K-12", "K-12", "Higher Ed"),
    mean_count = c(45.0, 10.0, 30.0),
    count = c(0, 0, 2),
    url = c("https://bozeman.k12.mt.us", "https://havre.k12.mt.us", "https://msu.edu"),
    verdict = c("confirmed_broken", "likely_broken", "likely_broken"),
    error_message = c("500 Server Error", NA, NA),
    llm_note = c(NA, "an LLM read 4 postings", NA),
    safety_net_vacancies = c(5L, 0L, NA_integer_),
    stringsAsFactors = FALSE
  )

  safety_net_summary <- list(
    total_feed_vacancies = 350L,
    fallback_districts = list("Bozeman Public Schools" = 5L),
    total_fallback_vacancies = 5L,
    markdown = c(
      "### OPI postings at districts we scrape directly",
      "",
      "- **Statewide Feed (OPI)**: 350 active vacancies captured.",
      "- **Counted from OPI at directly scraped districts**: 5 posting(s) at 1 district(s), not on the district's own board:",
      "  - **Bozeman Public Schools**: 5",
      "- **Central Feed Discrepancies**: 0 unmatched vacancies detected for active direct scrapers.",
      ""
    )
  )

  lines <- build_drift_report_markdown(
    actionable = actionable,
    safety_net_summary = safety_net_summary,
    date = as.Date("2026-10-29"),
    feed_short_name = "OPI"
  )

  report <- paste(lines, collapse = "\n")
  # Bozeman: active fallback
  expect_match(report, "Bozeman Public Schools", fixed = TRUE)
  expect_match(report, "On OPI**: 5 postings for this district that its own board doesn't list (counted from OPI)", fixed = TRUE)
  # Havre: inactive fallback
  expect_match(report, "Havre Public Schools", fixed = TRUE)
  expect_match(report, "On OPI**: nothing for this district beyond what its own board lists", fixed = TRUE)
  # Higher Ed: no Central Safety Net bullet under its entry
  expect_match(report, "Montana State University", fixed = TRUE)
  expect_false(grepl("Montana State University.*\\n\\s+- \\*\\*On OPI", report))
  # Bottom summary block included
  expect_match(report, "### OPI postings at districts we scrape directly", fixed = TRUE)
  expect_match(report, "Statewide Feed (OPI)**: 350 active vacancies captured", fixed = TRUE)
})

test_that("the feed and the drift baseline come from state_config(), with state-free defaults", {
  old <- current_state_config(); withr::defer(set_state_config(old))
  expect_error(state_config("Test", "00", "app", feed = list(short_name = "X")), "feed needs")
  set_state_config(NULL)
  md <- summarize_central_safety_net()$markdown
  expect_true(any(grepl("statewide feed", md)))
  snaps <- list(`2026-01-01` = data.frame(name = "a"), `2026-09-01` = data.frame(name = "a"))
  expect_equal(build_historical_counts(snaps, "name")$n_weeks, 2)            # no config: every snapshot
  set_state_config(state_config("Test", "00", "app", feed = mt_feed, drift_baseline_from = "2026-08-06"))
  expect_equal(build_historical_counts(snaps, "name")$n_weeks, 1)
  expect_true(any(grepl("OPI", summarize_central_safety_net()$markdown)))
})

test_that("build_historical_counts: zero weeks count between a source's first and last weeks with rows", {
  set_state_config(NULL); withr::defer(set_state_config(NULL))
  snaps <- list("2026-10-02" = data.frame(name = c("A", "A", "A")), "2026-10-09" = data.frame(name = c("C", "C")),
                "2026-10-16" = data.frame(name = c("A", "A", "A")), "2026-10-23" = data.frame(name = "C"),
                "2026-10-30" = data.frame(name = character(0)))
  b <- build_historical_counts(snaps, "name")
  # A: 3, 0, 3 (10-09 is an empty week between busy ones); 10-23 and 10-30 come after its last rows.
  expect_equal(b$n_weeks[b$name == "A"], 3); expect_equal(b$mean_count[b$name == "A"], 6 / 3)
  # C: from its first week (10-09) to its last (10-23): 2, 0, 1. 10-02 predates it.
  expect_equal(b$n_weeks[b$name == "C"], 3); expect_equal(b$mean_count[b$name == "C"], 3 / 3)
  # An intermittent board averages under the floor and isn't judged on an empty week.
  expect_false("C" %in% flag_drift(data.frame(name = c("A", "C"), count = c(3, 0)), b, min_mean_count = 2)$name)
})

test_that("a silent outage never lowers its own baseline: it keeps being flagged", {
  set_state_config(NULL); withr::defer(set_state_config(NULL))
  busy <- data.frame(name = rep("A", 5))
  snaps <- setNames(c(rep(list(busy), 4), rep(list(data.frame(name = character(0))), 20)),
                    format(as.Date("2026-06-05") + 7 * 0:23))
  b <- build_historical_counts(snaps, "name")
  expect_equal(b$mean_count, 5); expect_equal(b$n_weeks, 4)
  expect_equal(nrow(flag_drift(data.frame(name = "A", count = 0), b)), 1)
})

test_that("weeks the calendar says failed are left out, not counted as 0", {
  set_state_config(NULL); withr::defer(set_state_config(NULL))
  snaps <- list("2026-10-02" = data.frame(name = rep("A", 4)), "2026-10-09" = data.frame(name = character(0)),
                "2026-10-16" = data.frame(name = rep("A", 4)))
  cal <- data.frame(Run_Date = c("2026-10-09", "2026-10-09", "2026-10-16"), Entity = c("A", "B", "A"),
                    Status = c("error", "empty", "ok"))
  f <- failed_source_weeks(cal)
  expect_equal(f, data.frame(name = "A", date = "2026-10-09"))
  expect_equal(build_historical_counts(snaps, "name")$mean_count, 8 / 3)
  b <- build_historical_counts(snaps, "name", failed = f)
  expect_equal(b$mean_count, 4); expect_equal(b$n_weeks, 2)
  expect_equal(nrow(failed_source_weeks(NULL)), 0)
})
