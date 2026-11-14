# Shared tests moved from the MT and WY repos' test-drift-check.R in phase 2 (2026-11-11). A block both
# states had identically appears once; a same-named block with different contents appears
# twice, the second marked with its state.

test_that("attach_scrape_log_errors attaches the latest error message for a matching source", {
  flagged <- data.frame(name = c("Lincoln County School District 1", "Stable District"),
                        mean_count = c(10, 5), n_weeks = c(3, 3), count = c(0, 4),
                        stringsAsFactors = FALSE)
  scrape_log <- data.frame(
    timestamp = c("2026-08-11 15:39:20", "2026-08-11 15:40:00"),
    source = c("Lincoln County School District 1", "Stable District"),
    status = c("error", "ok"),
    n_rows = c(0, 4),
    error_message = c("HTTP 500 Internal Server Error.", NA_character_),
    stringsAsFactors = FALSE
  )

  result <- attach_scrape_log_errors(flagged, scrape_log)

  expect_equal(result$scrape_error[result$name == "Lincoln County School District 1"], "HTTP 500 Internal Server Error.")
  expect_true(is.na(result$scrape_error[result$name == "Stable District"]))
})

test_that("attach_scrape_log_errors matches by substring for a platform-prefixed scrape_log source name", {
  flagged <- data.frame(name = "Niobrara County School District 1", mean_count = 8, n_weeks = 3, count = 0,
                        stringsAsFactors = FALSE)
  scrape_log <- data.frame(
    timestamp = "2026-08-11 15:39:00",
    source = "Apptegy/chromote: Niobrara County School District 1",
    status = "error", n_rows = 0, error_message = "Page structure changed",
    stringsAsFactors = FALSE
  )

  result <- attach_scrape_log_errors(flagged, scrape_log)

  expect_equal(result$scrape_error, "Page structure changed")
})

test_that("attach_scrape_log_errors keeps only the most recent attempt when a source logged twice", {
  flagged <- data.frame(name = "Sublette County School District 9", mean_count = 4.7, n_weeks = 3, count = 0,
                        stringsAsFactors = FALSE)
  scrape_log <- data.frame(
    timestamp = c("2026-08-11 15:39:22", "2026-08-11 15:45:00"),
    source = c("Sublette County School District 9", "Sublette County School District 9"),
    status = c("error", "ok"),
    n_rows = c(0, 12),
    error_message = c("HTTP 429 Too Many Requests.", NA_character_),
    stringsAsFactors = FALSE
  )

  result <- attach_scrape_log_errors(flagged, scrape_log)

  # The later attempt (a retry/re-run) succeeded -- no error should be attached.
  expect_true(is.na(result$scrape_error))
})

test_that("attach_scrape_log_errors leaves scrape_error NA when there's no scrape_log or no flagged rows", {
  flagged <- data.frame(name = "Some District", mean_count = 5, n_weeks = 3, count = 0, stringsAsFactors = FALSE)
  empty_log <- data.frame(timestamp = character(0), source = character(0), status = character(0),
                          n_rows = integer(0), error_message = character(0))

  result <- attach_scrape_log_errors(flagged, empty_log)
  expect_true(is.na(result$scrape_error))

  empty_flagged <- data.frame(name = character(0), mean_count = numeric(0), n_weeks = integer(0), count = numeric(0))
  result2 <- attach_scrape_log_errors(empty_flagged, empty_log)
  expect_equal(nrow(result2), 0)
  expect_true("scrape_error" %in% names(result2))
})

test_that("check_salary_coverage flags a source that fell below its expected count", {
  flagged <- check_salary_coverage("K-12 teacher base salary (WSBA)", actual = 12, expected = 48)
  expect_equal(flagged$name, "K-12 teacher base salary (WSBA)")
  expect_equal(flagged$expected, 48)
  expect_equal(flagged$actual, 12)
})

test_that("check_salary_coverage returns NULL when a source meets its expected count", {
  expect_null(check_salary_coverage("K-12 teacher base salary (WSBA)", actual = 48, expected = 48))
})

test_that("check_salary_coverage supports a tolerance below the ideal expected count", {
  # IPEDS Professor-rank coverage is legitimately sparse (some two-year
  # colleges report no "Professor" rank at all) -- min_ok lets a check use
  # a looser floor than the full expected universe without that being
  # confused with actual drift.
  expect_null(check_salary_coverage("HE avg faculty salary (IPEDS)", actual = 8, expected = 9, min_ok = 8))
  flagged <- check_salary_coverage("HE avg faculty salary (IPEDS)", actual = 5, expected = 9, min_ok = 8)
  expect_equal(flagged$actual, 5)
})

test_that("check_salary_value_bounds flags a value outside the sane dollar range", {
  actual <- c(A = 51000, B = 52500, C = 5)  # C is obvious garbage (a parser misread)
  flagged <- check_salary_value_bounds("K-12 teacher base salary (WSBA)", actual, min_ok = 25000, max_ok = 150000)
  expect_equal(flagged$entity, "C")
  expect_equal(flagged$value, 5)
})

test_that("check_salary_value_bounds returns NULL when every value is in range", {
  actual <- c(A = 51000, B = 52500, C = 48000)
  expect_null(check_salary_value_bounds("K-12 teacher base salary (WSBA)", actual, min_ok = 25000, max_ok = 150000))
})

test_that("check_salary_value_bounds ignores NA rather than flagging it", {
  actual <- c(A = 51000, B = NA_real_)
  expect_null(check_salary_value_bounds("K-12 teacher base salary (WSBA)", actual, min_ok = 25000, max_ok = 150000))
})

test_that("check_salary_yoy_plausibility flags a district whose change is a real outlier against its peers", {
  # 5 districts move a normal 2-6%; one (Z) jumps 47% -- the signature of a
  # WSBA PDF column misalignment, not a real settlement.
  prior <- c(A = 50000, B = 48000, C = 52000, D = 49000, E = 51000, Z = 50000)
  current <- c(A = 51500, B = 49500, C = 53500, D = 50500, E = 53500, Z = 73500)
  flagged <- check_salary_yoy_plausibility(current, prior)
  expect_equal(flagged$name, "Z")
  expect_equal(round(flagged$pct_change, 2), 0.47)
})

test_that("check_salary_yoy_plausibility does not flag a statewide event where every district moves a lot", {
  # Same ~15% move across the board -- a real (if unusual) statewide
  # settlement year, not a parser bug; no district is an outlier relative
  # to its peers even though the hard ceiling alone would catch the wrong
  # thing here if checked in isolation.
  prior <- c(A = 50000, B = 48000, C = 52000, D = 49000, E = 51000)
  current <- prior * 1.15
  expect_null(check_salary_yoy_plausibility(current, prior))
})

test_that("check_salary_yoy_plausibility does not flag normal small variation", {
  prior <- c(A = 50000, B = 48000, C = 52000, D = 49000, E = 51000)
  current <- c(A = 51000, B = 49200, C = 53000, D = 50100, E = 51800)
  expect_null(check_salary_yoy_plausibility(current, prior))
})

test_that("check_salary_yoy_plausibility returns NULL with too few comparable entities", {
  prior <- c(A = 50000, B = 48000)
  current <- c(A = 70000, B = 48500)
  expect_null(check_salary_yoy_plausibility(current, prior))
})

test_that("check_salary_yoy_plausibility still flags an outlier when every peer moved by exactly zero (MAD == 0)", {
  # Regression: when every OTHER entity's change is identical (here, all
  # zero), stats::mad(pct_change) is itself 0 -- a naive "flag if the
  # deviation exceeds a multiple of the spread" check then requires
  # exceeding a threshold of 0 * mad_multiplier = 0, and an earlier faulty
  # version of this function instead fell back to a threshold of Inf in
  # this exact case, which made the outlier undetectable no matter how
  # extreme. The fallback must make flagging EASIER when peer spread is
  # near zero, not impossible.
  prior <- c(A = 50000, B = 48000, C = 52000, D = 49000, E = 51000, Z = 50000)
  current <- c(A = 50000, B = 48000, C = 52000, D = 49000, E = 51000, Z = 73500)
  flagged <- check_salary_yoy_plausibility(current, prior)
  expect_equal(flagged$name, "Z")
})

test_that("check_salary_yoy_plausibility does not flag a uniform statewide move even with zero peer spread", {
  # Every district moves by the exact same multiplier -- spread is 0 (same
  # edge case as above), but the move itself is below hard_ceiling, so
  # nothing should be flagged.
  prior <- c(A = 50000, B = 48000, C = 52000, D = 49000, E = 51000)
  current <- prior * 1.15
  expect_null(check_salary_yoy_plausibility(current, prior))
})

test_that("score_page_text_for_job_signal identifies real hidden postings as likely_broken", {
  # Real fixture: this is the actual Sweetwater County SD1 page text that
  # exposed the Applitrack encoding bug -- 69 real postings, scraper said 0.
  text <- paste(readLines(test_path("fixtures", "drift_check", "real_postings_sweetwater.txt"), warn = FALSE), collapse = "\n")
  expect_equal(score_page_text_for_job_signal(text), "likely_broken")
})

test_that("score_page_text_for_job_signal identifies a genuinely empty page", {
  # Real fixture: Western Wyoming CC's actual "No jobs at this time." page.
  text <- paste(readLines(test_path("fixtures", "drift_check", "genuinely_empty_western.txt"), warn = FALSE), collapse = "\n")
  expect_equal(score_page_text_for_job_signal(text), "looks_genuinely_empty")
})

test_that("score_page_text_for_job_signal is honestly inconclusive on ambiguous real content", {
  # Real fixture: Sheridan County SD3's Apptegy page -- real postings exist
  # ("Bus Drivers", a coaching opening) but in a bare-line format with none
  # of the structured keywords (JobID, "posted:", etc.) this heuristic looks
  # for, and no explicit "no openings" phrase either. Getting this wrong in
  # either direction would be worse than admitting the heuristic can't tell.
  text <- paste(readLines(test_path("fixtures", "drift_check", "ambiguous_bare_lines_sheridan3.txt"), warn = FALSE), collapse = "\n")
  expect_equal(score_page_text_for_job_signal(text), "inconclusive")
})

test_that("score_page_text_for_job_signal returns inconclusive for NA or empty input", {
  expect_equal(score_page_text_for_job_signal(NA_character_), "inconclusive")
  expect_equal(score_page_text_for_job_signal(""), "inconclusive")
  expect_equal(score_page_text_for_job_signal("   "), "inconclusive")
})

# --- LLM corroboration (Tier 2b) ----------------------------------------

test_that("combine_verdict_with_llm promotes inconclusive to likely_broken when the LLM read real titles", {
  out <- combine_verdict_with_llm("inconclusive", c("Bus Driver", "3rd Grade Teacher"))
  expect_equal(out$verdict, "likely_broken")
  expect_match(out$note, "2 posting\\(s\\)")
  expect_match(out$note, "Bus Driver; 3rd Grade Teacher")
})

test_that("combine_verdict_with_llm truncates a long LLM title list in the note", {
  out <- combine_verdict_with_llm("inconclusive", paste("Role", 1:12))
  expect_equal(out$verdict, "likely_broken")
  expect_match(out$note, "Role 8, \\.\\.\\.$")
})

test_that("combine_verdict_with_llm downgrades inconclusive to genuinely_empty only when the LLM also found nothing", {
  out <- combine_verdict_with_llm("inconclusive", character(0))
  expect_equal(out$verdict, "looks_genuinely_empty")
  expect_match(out$note, "no postings")
})

test_that("combine_verdict_with_llm does not touch looks_genuinely_empty or a confirmed_broken verdict", {
  expect_equal(combine_verdict_with_llm("looks_genuinely_empty", character(0))$verdict, "looks_genuinely_empty")
  expect_true(is.na(combine_verdict_with_llm("looks_genuinely_empty", character(0))$note))
  # confirmed_broken stays put even if the LLM happens to read something
  expect_equal(combine_verdict_with_llm("confirmed_broken", c("Bus Driver"))$verdict, "confirmed_broken")
})

test_that("combine_verdict_with_llm doesn't count titles the scraper already returned (Sublette 1, 2026-11-13)", {
  # The scraper returned its one item; the LLM read the same one off the page.
  out <- combine_verdict_with_llm("inconclusive", "Substitutes", history_titles = c("Substitutes", "Substitute School Nurse"),
                                  current_titles = "Substitutes")
  expect_equal(out$verdict, "looks_genuinely_empty")
  expect_match(out$note, "only what the scraper returned \\(1 title")
  # Without current titles, the old behaviour.
  expect_equal(combine_verdict_with_llm("inconclusive", "Substitutes", c("Substitutes"))$verdict, "likely_broken")
  # A title the scraper missed still promotes, and the note says how many it did return.
  out <- combine_verdict_with_llm("inconclusive", c("Substitutes", "Bus Driver"), c("Substitutes"),
                                  current_titles = "Substitutes")
  expect_equal(out$verdict, "likely_broken")
  expect_match(out$note, "1 posting\\(s\\) off the live page that the scraper didn't return \\(1 more it did\\): Bus Driver$")
  # A scraper that returned nothing (every real break so far) is unaffected.
  expect_equal(combine_verdict_with_llm("inconclusive", "Route Bus Drivers", current_titles = character(0))$verdict,
               "likely_broken")
})

test_that("drop_titles_already_scraped matches loosely but not on short words", {
  cur <- c("High School Girls Assistant Basketball Coach 8/7/2026.docx", "Substitutes", "Para")
  expect_equal(drop_titles_already_scraped(c("High School Girls Assistant Basketball Coach", "substitutes "), cur),
               character(0))
  # "Para" (under 8 characters) doesn't cover a longer title that contains it.
  expect_equal(drop_titles_already_scraped("Special Education Paraprofessional", cur), "Special Education Paraprofessional")
  expect_equal(drop_titles_already_scraped("Bus Driver", NULL), "Bus Driver")
  expect_equal(title_match_key(c("Coach 24-25.docx", NA)), c("coach 24 25", ""))
})

test_that("combine_verdict_with_llm keeps an already-likely_broken verdict and still lists what the LLM found", {
  out <- combine_verdict_with_llm("likely_broken", c("Custodian"))
  expect_equal(out$verdict, "likely_broken")
  expect_match(out$note, "Custodian")
})

test_that("llm_titles_from_page_text returns character(0) with no key or a blank page (never throws)", {
  withr::with_envvar(c(GEMINI_API_KEY = "", LLM_EXTRACT_KEY_ENV = ""), {
    expect_identical(llm_titles_from_page_text("Food Service Worker\nBus Driver"), character(0))
  })
  withr::with_envvar(c(GEMINI_API_KEY = "fake-key"), {
    expect_identical(llm_titles_from_page_text(NA_character_), character(0))
    expect_identical(llm_titles_from_page_text("   "), character(0))
  })
})

test_that("autofix_engine defaults to claude and rejects typos", {
  expect_equal(autofix_engine(""), "claude")
  expect_equal(autofix_engine(NA_character_), "claude")
  expect_equal(autofix_engine(" Copilot "), "copilot")
  expect_equal(autofix_engine("none"), "none")
  expect_error(autofix_engine("claud"), "AUTOFIX_ENGINE must be one of")
})

test_that("parse_autofix_marker returns NA for bodies without a marker", {
  expect_true(is.na(parse_autofix_marker("just a normal issue")))
  expect_true(is.na(parse_autofix_marker(NA_character_)))
  expect_true(is.na(parse_autofix_marker(character(0))))
})

# --- Posted-pay coverage -----------------------------------------------------

test_that("pay_coverage_counts tallies postings and postings with pay per source", {
  out <- pay_coverage_counts(c("A", "A", "B", "A"), c(TRUE, FALSE, FALSE, TRUE))
  expect_equal(out$name, c("A", "B"))
  expect_equal(out$n, c(3L, 1L))
  expect_equal(out$with_pay, c(2L, 0L))
  expect_equal(nrow(pay_coverage_counts(character(0), logical(0))), 0)
})

test_that("flag_pay_coverage_drop flags a source whose postings arrive but lost their pay", {
  week <- function(n, with_pay) data.frame(name = c("Paycom College", "Free Text District"),
                                           n = n, with_pay = with_pay)
  history <- list(week(c(30, 10), c(16, 1)), week(c(31, 12), c(15, 2)))
  current <- data.frame(name = c("Paycom College", "Free Text District"), n = c(31, 11), with_pay = c(0, 0))
  out <- flag_pay_coverage_drop(current, history)
  # The free-text district never carried much pay, so its 0 is not a drop.
  expect_equal(out$name, "Paycom College")
  expect_equal(out$with_pay, 0)
  expect_equal(out$n_weeks, 2L)
  expect_equal(round(out$mean_share, 3), round(mean(c(16 / 30, 15 / 31)), 3))
})

test_that("flag_pay_coverage_drop stays quiet without enough history or postings", {
  hist1 <- list(data.frame(name = "A", n = 10, with_pay = 9))
  hist2 <- c(hist1, hist1)
  expect_equal(nrow(flag_pay_coverage_drop(data.frame(name = "A", n = 10, with_pay = 0), hist1)), 0)
  # Too few postings this run to judge (and a lost-postings source is flag_drift()'s job).
  expect_equal(nrow(flag_pay_coverage_drop(data.frame(name = "A", n = 2, with_pay = 0), hist2)), 0)
  # A normal week-to-week dip is not flagged.
  expect_equal(nrow(flag_pay_coverage_drop(data.frame(name = "A", n = 10, with_pay = 6), hist2)), 0)
  expect_equal(nrow(flag_pay_coverage_drop(data.frame(name = "A", n = 10, with_pay = 0), list())), 0)
  expect_equal(nrow(flag_pay_coverage_drop(data.frame(name = "A", n = 10, with_pay = 0), hist2)), 1)
})

test_that("build_pay_coverage_report_markdown names each flagged source and its numbers", {
  expect_equal(build_pay_coverage_report_markdown(data.frame()), character(0))
  flagged <- data.frame(name = "Paycom College", mean_share = 0.51, n_weeks = 2L, n = 31L,
                        with_pay = 0L, type = "Higher Ed")
  md <- build_pay_coverage_report_markdown(flagged, date = as.Date("2026-11-07"))
  expect_true(any(grepl("Posted pay went missing (2026-11-07)", md, fixed = TRUE)))
  expect_true(any(grepl("**Paycom College** (Higher Ed): usually 51% of postings state pay, now 0 of 31", md, fixed = TRUE)))
})

# ---- Title quality (classification safeguards items 5 and 6) ----
# Titles below are real, copied from the MT and WY raw archives.

test_that("lint_title_shape flags the misread titles found in the archives", {
  bad <- c(
    "High School - Head" = "level and rank only, no role",                      # Plevna
    "Jr. High - Asst." = "level and rank only, no role",                        # Plevna
    "FULL TIME. MON - FRI." = "schedule or detail sentence",                    # Florence-Carlton
    "YEAR-ROUND POSITION." = "schedule or detail sentence",                     # Florence-Carlton
    "Bus Drivers- Must have a CDL with a school bus passenger endorsement." = "schedule or detail sentence",
    "Food Service is hiring for a cook position." = "schedule or detail sentence",  # Uinta 6
    "Special Education Teachers - Open Until Filled." = "schedule or detail sentence",  # Fremont 21
    "Dish_Server_Cashiers 26" = "file name",                                    # Livingston
    "Dish_Server_Cashier 26 - Copy" = "file name",
    "Other" = "no job named"                                                    # OPI subject field
  )
  expect_equal(lint_title_shape(names(bad)), unname(bad))
  ennis <- paste("Summary: Ennis Schools is seeking a Virtual Programs Paraprofessional to join our team for the",
                 "remainder of the 2026-2027 school year. The ideal candidate will work closely with the building",
                 "principal to ensure proper supervision of students, work with staff to help provide appropriate",
                 "virtual classes, and help to develop virtual programs at the secondary level.")
  expect_equal(lint_title_shape(ennis), "schedule or detail sentence; field label; too long")
  expect_equal(lint_title_shape("CustodianEndFragment (NEW)$18.90–$20.00/hour"), "markup left in")  # Cheyenne Classical
  expect_equal(lint_title_shape(c(NA_character_, "", "  ")), rep("blank title", 3))
  expect_equal(lint_title_shape(character(0)), character(0))
})

test_that("lint_title_shape leaves real titles alone, abbreviations and start dates included", {
  good <- c(
    "Junior High 7-8", "Track Coach - Jr. High - Asst.", "COMMUNICATIONS COORD.",
    "Instructional Asst./Technology Asst.", "Director of Budget and Finance.", "Seasonal Grounds Keeper.",
    "Assistant High School Girls Basketball Coach.",
    "Journey Administrative Specialist beginning for the 2026-2027 school year.",
    "Project Coordinator, Senior - Fed. Relations/Employee Success Coord.", "Elementary",
    "Substitute Certified Teachers $150/day",
    "Special Education Maternity Leave (2) - PMS - (8/26/26-12/14/26)/TCH00532 - (11/2/26-3/15/27)/TCH00073",
    paste("Monthly Pooled - WCC AmeriCorps Individual Placement - Habitat Biologist Technician with Wyoming",
          "Game and Fish Department and Bighorn National Forest")
  )
  expect_equal(lint_title_shape(good), rep(NA_character_, length(good)))
})

test_that("title_pay_unrecorded flags pay stated in the title only when Posted Pay is blank", {
  t <- c("Secretary – $14 per hour, up to 32 hours per week",                       # Turner
         "Substitute Certified Teachers $150/day",                                      # Platte 2
         "Science Teacher - Torrington Middle School - Eligible for $10,000 Hiring Incentive Based on Certification",
         "Custodian", NA)
  expect_equal(title_pay_unrecorded(t, c(NA, 150, NA, NA, NA)), c(TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(title_pay_unrecorded(character(0), numeric(0)), logical(0))
})

test_that("build_title_lint_report_markdown counts postings and caps the titles shown per source", {
  hits <- data.frame(source = c(rep("OPI", 7), "Turner"), type = "K-12",
                     title = c("Other", "Other", paste("T", 1:5), "Secretary $14"),
                     reason = "x", stringsAsFactors = FALSE)
  md <- build_title_lint_report_markdown(hits, date = "2026-11-05", max_per_source = 3)
  expect_true(any(grepl("**OPI** (K-12), 7 posting(s):", md, fixed = TRUE)))
  expect_true(any(grepl("...and 3 more", md, fixed = TRUE)))    # 6 distinct titles, 3 shown
  expect_equal(sum(grepl("\"Other\"", md, fixed = TRUE)), 1)
  expect_length(build_title_lint_report_markdown(hits[0, ]), 0)
})

test_that("flag_other_share_jump flags a source whose Other share jumped against its recent weeks", {
  # Florence-Carlton, MT 2026-09-08: 2 of 10 postings were schedule lines, none before.
  history <- list(data.frame(name = c("Florence", "Steady"), n = c(8, 10), other = c(0, 1)),
                  data.frame(name = c("Florence", "Steady"), n = c(9, 10), other = c(0, 1)))
  current <- data.frame(name = c("Florence", "Steady"), n = c(10, 10), other = c(2, 2))
  out <- flag_other_share_jump(current, history)
  expect_equal(out$name, "Florence")
  expect_equal(out$mean_share, 0)
  expect_equal(out$other, 2)
})

test_that("flag_other_share_jump stays quiet without enough history, postings or Other rows", {
  hist1 <- list(data.frame(name = "A", n = 10, other = 0))
  expect_equal(nrow(flag_other_share_jump(data.frame(name = "A", n = 10, other = 5), hist1)), 0)
  hist2 <- c(hist1, hist1)
  expect_equal(nrow(flag_other_share_jump(data.frame(name = "A", n = 2, other = 2), hist2)), 0)
  expect_equal(nrow(flag_other_share_jump(data.frame(name = "A", n = 10, other = 1), hist2)), 0)
  # already mostly Other: a rise from 50% to 60% isn't a jump
  hist3 <- list(data.frame(name = "A", n = 10, other = 5), data.frame(name = "A", n = 10, other = 5))
  expect_equal(nrow(flag_other_share_jump(data.frame(name = "A", n = 10, other = 6), hist3)), 0)
  expect_equal(nrow(flag_other_share_jump(data.frame(name = "A", n = 10, other = 5), list())), 0)
})

test_that("other_share_counts and build_other_share_report_markdown", {
  counts <- other_share_counts(c("A", "A", "B"), c(TRUE, FALSE, FALSE))
  expect_equal(counts$other, c(1L, 0L))
  flagged <- data.frame(name = "Florence", mean_share = 0, n_weeks = 8L, n = 10L, other = 2L,
                        type = "K-12", examples = "\"YEAR-ROUND POSITION.\"", stringsAsFactors = FALSE)
  md <- build_other_share_report_markdown(flagged, date = "2026-11-05")
  expect_true(any(grepl("**Florence** (K-12): usually 0% Other, now 2 of 10", md, fixed = TRUE)))
  expect_length(build_other_share_report_markdown(flagged[0, ]), 0)
})

test_that("find_keyword_conflicts flags titles whose words name another bucket, and spares real exceptions", {
  # Real archived titles (2026-11-08 tuning run). Flagged: what the classifier
  # got wrong before its fixes that day, given the bucket it had.
  flagged <- find_keyword_conflicts(
    c("Temporary Business Office Support Clerk", "CCA Cross Country Aide", "Bus Paraprofessional - Transportation",
      "Kitchen Aide"),
    c("Support Services", "Support Services", "Paraprofessional", "Support Services"),
    k12_keyword_conflict_rules)
  expect_equal(flagged, c("secretary or clerk outside Staff", "coach aide outside Athletics",
                          "bus aide or monitor outside Transportation", "food-service word outside Food Services"))
  expect_equal(find_keyword_conflicts("Custodian - Washakie Dining Center", "Culinary", he_keyword_conflict_rules),
               "custodian outside Maintenance & Service")
  # Spared: the word is secondary, or the title is a dual role.
  spared <- find_keyword_conflicts(
    c("Nutrition Services Secretary", "Summer Nutrition Services Van Driver", "Secretary/Paraprofessional",
      "Alternative High School Principal / Counselor", "Bus Driver/Classroom Paraprofessional", "Para Teacher Assistant"),
    c("Staff", "Transportation", "Paraprofessional", "Support Services", "Paraprofessional", "Paraprofessional"),
    k12_keyword_conflict_rules)
  expect_true(all(is.na(spared)))
  expect_true(is.na(find_keyword_conflicts("Hourly Pooled - Custodian/Dish Room Attendant - Bakery", "Culinary",
                                           he_keyword_conflict_rules)))
  # The bucket the classifier gives today raises nothing for any of these.
  t <- c("Temporary Business Office Support Clerk", "CCA Cross Country Aide", "Bus Paraprofessional - Transportation")
  expect_true(all(is.na(find_keyword_conflicts(t, classify_k12_position(t), k12_keyword_conflict_rules))))
})

test_that("build_keyword_conflict_report_markdown lists each title once, and nothing when there are none", {
  expect_identical(build_keyword_conflict_report_markdown(
    data.frame(source = character(), type = character(), title = character(), bucket = character(), rule = character())),
    character(0))
  hits <- data.frame(source = c("Laramie #1", "Laramie #1"), type = "K-12", title = "Kitchen Aide",
                     bucket = "Support Services", rule = "food-service word outside Food Services")
  md <- build_keyword_conflict_report_markdown(hits, date = "2026-11-08")
  expect_equal(sum(grepl("Kitchen Aide", md)), 1)
  expect_match(md[1], "different bucket \\(2026-11-08\\)")
})

# Standing substitute lists (MT issue #13, Lone Rock, 2026-11-10): an LLM-read
# list of substitute pools doesn't make a source "likely_broken" unless the
# source posts substitute jobs itself.
test_that("substitute lines are set aside only for a source whose own titles have none", {
  pool <- c("bus driver substitutes", "classroom teacher substitutes", "kitchen substitutes", "para professionals substitutes")
  expect_length(drop_standing_pool_titles(pool, c("Special Education Paraprofessional", "Bus Driver and Maintenance Director")), 0)
  # a board that posts substitute jobs: the same lines are real misses
  expect_equal(drop_standing_pool_titles(pool, c("Substitute Teacher", "3rd Grade Teacher")), pool)
  # no history to judge by: keep everything
  expect_equal(drop_standing_pool_titles(pool, NULL), pool)
  # real titles survive next to pool lines; "Subject"-like words aren't substitutes
  expect_equal(drop_standing_pool_titles(c("kitchen substitutes", "Subject Area Specialist", "Bus subs"), "Math Teacher"),
               "Subject Area Specialist")
})

test_that("an LLM read of only a standing substitute list no longer promotes to likely_broken", {
  pool <- c("bus driver substitutes", "classroom teacher substitutes", "custodial substitutes",
            "kitchen substitutes", "office staff substitutes", "para professionals substitutes")
  out <- combine_verdict_with_llm("inconclusive", pool, c("Special Education Paraprofessional"))
  expect_equal(out$verdict, "looks_genuinely_empty")
  expect_match(out$note, "standing substitute list \\(6")
  # the old behaviour stands where the source does post substitutes, or without history
  expect_equal(combine_verdict_with_llm("inconclusive", pool, "Substitute Teacher")$verdict, "likely_broken")
  expect_equal(combine_verdict_with_llm("inconclusive", pool)$verdict, "likely_broken")
  # a real title among the pool lines still promotes
  expect_equal(combine_verdict_with_llm("inconclusive", c(pool, "Head Cook"), "Bus Driver")$verdict, "likely_broken")
})

test_that("recent_source_titles reads one source's titles from the K-12 and HE archives", {
  dir <- withr::local_tempdir(); k <- file.path(dir, "k"); h <- file.path(dir, "h"); dir.create(k); dir.create(h)
  write.csv(data.frame(District = c("A", "B"), title = c("Math Teacher", "Substitute Teacher")), file.path(k, "combined_2026-11-03.csv"), row.names = FALSE)
  write.csv(data.frame(District = "A", title = "Bus Driver"), file.path(k, "combined_2026-11-10.csv"), row.names = FALSE)
  writexl::write_xlsx(data.frame(Institution = "U", Title = "Adjunct"), file.path(h, "hedata_2026-11-10.xlsx"))
  expect_setequal(recent_source_titles("A", "K-12", k12_dir = k, he_dir = h), c("Math Teacher", "Bus Driver"))
  expect_equal(recent_source_titles("A", "K-12", weeks = 1, k12_dir = k, he_dir = h), "Bus Driver")
  expect_equal(recent_source_titles("U", "Higher Ed", k12_dir = k, he_dir = h), "Adjunct")
  expect_null(recent_source_titles("A", "K-12", k12_dir = file.path(dir, "none"), he_dir = h))
})

test_that("autofix_escalation_decision hands a stale, unfixed issue to Claude once", {
  none <- data.frame(state = character(0), changed_files = integer(0))
  pr <- function(state, files) data.frame(state = state, changed_files = files)
  # Filed 2026-11-06, still flagged 2026-11-13, Copilot never opened a PR.
  expect_equal(autofix_escalation_decision("2026-11-06T17:23:00Z", "2026-11-13", character(0), none), "escalate")
  # Same-week re-run: Copilot gets its week.
  expect_equal(autofix_escalation_decision("2026-11-11T17:23:00Z", "2026-11-13", character(0), none), "too_new")
  # Copilot's PR closed unmerged, or open with no changes ("no scraper change proposed"): escalate.
  expect_equal(autofix_escalation_decision("2026-11-06", "2026-11-13", character(0), pr("CLOSED", 3)), "escalate")
  expect_equal(autofix_escalation_decision("2026-11-06", "2026-11-13", character(0), pr("OPEN", 0)), "escalate")
  # An open PR with changes is waiting for Mike; a merged fix that's still flagged needs a person.
  expect_equal(autofix_escalation_decision("2026-11-06", "2026-11-13", character(0), pr("OPEN", 2)), "pr_awaiting_review")
  expect_equal(autofix_escalation_decision("2026-11-06", "2026-11-13", character(0), pr(c("CLOSED", "MERGED"), c(1, 1))), "fix_merged")
  # Only once: the marker comment from the first hand-over blocks a second.
  expect_equal(autofix_escalation_decision("2026-11-06", "2026-11-20",
                                           c("Still flagged", paste("Handed to Claude.", AUTOFIX_ESCALATION_MARKER)), none),
               "already_escalated")
  expect_equal(autofix_escalation_decision(NA_character_, "2026-11-13", character(0), none), "too_new")
})

test_that("autofix_linked_prs finds only PRs that close that issue", {
  prs <- data.frame(number = c(9L, 10L, 11L, 12L), state = c("OPEN", "CLOSED", "OPEN", "MERGED"),
                    changedFiles = c(0L, 3L, 2L, 1L),
                    body = c("- Fixes #7", "Closes #7 too", "Fixes #70", NA))
  got <- autofix_linked_prs(prs, 7)
  expect_equal(got$number, c(9L, 10L))
  expect_equal(got$changed_files, c(0L, 3L))
  expect_equal(nrow(autofix_linked_prs(prs[0, ], 7)), 0)
  expect_equal(nrow(autofix_linked_prs(prs, 99)), 0)
})
# Ported from Wyoming's test-drift-check.R. All the pure-function tests
# carry over unchanged (the functions themselves are state-agnostic) --
# only build_source_url_lookup()'s test is rewritten for Montana's simpler
# two-registry setup, and score_page_text_for_job_signal()'s tests use
# synthetic (not real-captured) text, since that function is pure text-
# pattern matching with no Montana-specific behavior to verify against real
# fixtures.

test_that("attach_scrape_log_errors attaches the latest error message for a matching source (Mo)", {
  flagged <- data.frame(name = c("Butte School District 1", "Stable District"),
                        mean_count = c(10, 5), n_weeks = c(3, 3), count = c(0, 4),
                        stringsAsFactors = FALSE)
  scrape_log <- data.frame(
    timestamp = c("2026-09-01 10:00:00", "2026-09-01 10:05:00"),
    source = c("Butte School District 1", "Stable District"),
    status = c("error", "ok"),
    n_rows = c(0, 4),
    error_message = c("HTTP 500 Internal Server Error.", NA_character_),
    stringsAsFactors = FALSE
  )

  result <- attach_scrape_log_errors(flagged, scrape_log)

  expect_equal(result$scrape_error[result$name == "Butte School District 1"], "HTTP 500 Internal Server Error.")
  expect_true(is.na(result$scrape_error[result$name == "Stable District"]))
})

test_that("attach_scrape_log_errors matches by substring for prefixed scrape_log source names (Apptegy)", {
  flagged <- data.frame(name = "Wolf Point Public Schools", mean_count = 19, n_weeks = 3, count = 0,
                        stringsAsFactors = FALSE)
  scrape_log <- data.frame(
    timestamp = "2026-09-01 10:00:00",
    source = "Apptegy/chromote: Wolf Point Public Schools",
    status = "error", n_rows = 0, error_message = "Page structure changed",
    stringsAsFactors = FALSE
  )

  result <- attach_scrape_log_errors(flagged, scrape_log)

  expect_equal(result$scrape_error, "Page structure changed")
})

test_that("attach_scrape_log_errors keeps only the most recent attempt when a source logged twice (Mo)", {
  flagged <- data.frame(name = "Rocky Mountain College", mean_count = 4.7, n_weeks = 3, count = 0,
                        stringsAsFactors = FALSE)
  scrape_log <- data.frame(
    timestamp = c("2026-09-01 09:00:00", "2026-09-01 09:05:00"),
    source = c("Rocky Mountain College", "Rocky Mountain College"),
    status = c("error", "ok"),
    n_rows = c(0, 12),
    error_message = c("HTTP 429 Too Many Requests.", NA_character_),
    stringsAsFactors = FALSE
  )

  result <- attach_scrape_log_errors(flagged, scrape_log)

  # The later attempt succeeded -- no error should be attached.
  expect_true(is.na(result$scrape_error))
})

test_that("check_salary_coverage flags a source that fell below its expected count (Mo)", {
  flagged <- check_salary_coverage("K-12 teacher salary (MT DLI)", actual = 5, expected = 18)
  expect_equal(flagged$name, "K-12 teacher salary (MT DLI)")
  expect_equal(flagged$expected, 18)
  expect_equal(flagged$actual, 5)
})

test_that("check_salary_coverage returns NULL when a source meets its expected count (Mo)", {
  expect_null(check_salary_coverage("K-12 teacher salary (MT DLI)", actual = 18, expected = 18))
})

test_that("check_salary_coverage supports a tolerance below the ideal expected count (Mo)", {
  expect_null(check_salary_coverage("HE avg faculty salary (IPEDS)", actual = 5, expected = 6, min_ok = 5))
  flagged <- check_salary_coverage("HE avg faculty salary (IPEDS)", actual = 3, expected = 6, min_ok = 5)
  expect_equal(flagged$actual, 3)
})

test_that("check_salary_value_bounds flags a value outside the sane dollar range (Mo)", {
  actual <- c(A = 51000, B = 52500, C = 5)  # C is obvious garbage (a parser misread)
  flagged <- check_salary_value_bounds("K-12 teacher salary (MT DLI)", actual, min_ok = 25000, max_ok = 150000)
  expect_equal(flagged$entity, "C")
  expect_equal(flagged$value, 5)
})

test_that("check_salary_value_bounds returns NULL when every value is in range (Mo)", {
  actual <- c(A = 51000, B = 52500, C = 48000)
  expect_null(check_salary_value_bounds("K-12 teacher salary (MT DLI)", actual, min_ok = 25000, max_ok = 150000))
})

test_that("check_salary_value_bounds ignores NA rather than flagging it (Mo)", {
  actual <- c(A = 51000, B = NA_real_)
  expect_null(check_salary_value_bounds("K-12 teacher salary (MT DLI)", actual, min_ok = 25000, max_ok = 150000))
})

test_that("check_salary_yoy_plausibility flags an entity whose change is a real outlier against its peers", {
  prior <- c(A = 50000, B = 48000, C = 52000, D = 49000, E = 51000, Z = 50000)
  current <- c(A = 51500, B = 49500, C = 53500, D = 50500, E = 53500, Z = 73500)
  flagged <- check_salary_yoy_plausibility(current, prior)
  expect_equal(flagged$name, "Z")
  expect_equal(round(flagged$pct_change, 2), 0.47)
})

test_that("check_salary_yoy_plausibility does not flag a statewide event where every entity moves a lot", {
  prior <- c(A = 50000, B = 48000, C = 52000, D = 49000, E = 51000)
  current <- prior * 1.15
  expect_null(check_salary_yoy_plausibility(current, prior))
})

test_that("score_page_text_for_job_signal identifies real job-board content as likely_broken", {
  text <- "Current Openings\nMath Teacher - Posted: 08/01/2026 - Closing Date: 08/15/2026 - Apply Now\nBus Driver - JobID 4821 - View Details"
  expect_equal(score_page_text_for_job_signal(text), "likely_broken")
})

test_that("score_page_text_for_job_signal identifies a genuinely empty page (Mo)", {
  text <- "Employment Opportunities\nThere are currently no open positions. Please check back later."
  expect_equal(score_page_text_for_job_signal(text), "looks_genuinely_empty")
})

test_that("score_page_text_for_job_signal is honestly inconclusive on ambiguous content", {
  text <- "Now Hiring\nBus Drivers\nCoaching Positions Available\nContact HR for details."
  expect_equal(score_page_text_for_job_signal(text), "inconclusive")
})

# ---- auto-fix evidence (the CI capture handed to the agent) ----------------
# Issue #7: the agent's sandbox rendered Lone Rock's Apptegy page empty and
# it misdiagnosed the break. The real cause -- a renamed stop line plus one
# new posting -- is plain in a diff of the old fixture against the text the
# drift check rendered in CI. drift_autofix_lonerock_innertext_2026-09-30.txt
# is that text, captured live with scripts/repro_scraper.R.

lonerock_capture <- function() {
  paste(readLines(test_path("fixtures", "drift_autofix_lonerock_innertext_2026-09-30.txt"),
                  warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

test_that("text_diff of the pre-fix Lone Rock fixture vs the CI capture shows the real break", {
  skip_if(!nzchar(Sys.which("diff")), "no system diff")
  old <- paste(readLines(test_path("fixtures", "apptegy_lonerock_rendered.txt"), warn = FALSE), collapse = "\n")
  d <- text_diff(old, lonerock_capture(), "old.txt", "ci")
  expect_true("-District Employment Applications." %in% d)
  expect_true("+Classified Employee Application" %in% d)
  expect_true("+Bus Driver and Maintenance Director" %in% d)
  expect_lt(length(d), 40)
  expect_equal(text_diff("a\n\nb ", "a\nb"), character(0))
})
