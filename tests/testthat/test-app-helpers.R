# Dashboard app helpers (phase 6, slice 1). The first tests moved from both states' test-app-reactives.R and
# test-first-seen.R (they ran on the app's copies); the rest are new.

test_that("higher-ed appointment classifier separates adjunct, faculty, and other roles", {
  expect_identical(
    classify_he_appointment(c("Adjunct Mathematics Instructor", "Biology Professor", "Admissions Specialist")),
    c("Adjunct/part-time faculty", "Faculty/instructor (non-adjunct)", "Other / not faculty")
  )
  expect_equal(summarize_he_faculty_postings(c("Adjunct Instructor", "Professor", "Lecturer", "Registrar")),
               list(faculty_instructor = 2L, adjunct_part_time = 1L))
})

test_that("make_sparkline_svg draws a rising trend with a green endpoint and a falling trend with a red one", {
  rising <- make_sparkline_svg(c(10, 15, 20))
  expect_match(rising, "^<svg")
  expect_match(rising, "#1baf7a", fixed = TRUE)
  expect_match(make_sparkline_svg(c(20, 15, 10)), "#e34948", fixed = TRUE)
  expect_match(make_sparkline_svg(c(10, 10, 10)), "#999999", fixed = TRUE)
  expect_match(make_sparkline_svg_light(c(1, 3, 2)), "^<svg width=\"160\"")
})

test_that("make_sparkline_svg handles too little data without erroring", {
  expect_equal(make_sparkline_svg(numeric(0)), "")
  expect_equal(make_sparkline_svg(c(5)), "")
  expect_equal(make_sparkline_svg(c(NA, NA)), "")
  # One real value plus NAs still isn't enough to draw a line between two points.
  expect_equal(make_sparkline_svg(c(5, NA, NA)), "")
  expect_equal(make_sparkline_svg_light(c(5, NA)), "")
})

test_that("compute_wow_delta skips an extra same-week snapshot instead of comparing against it", {
  # A real week-old snapshot, plus an extra same-week re-run 1 day before "latest" (the failure mode min_days_back
  # guards against; WY's archive has a real one, 2026-08-03). Without the guard: 111 - 108.
  weekly <- data.frame(Archive_Date = as.Date(c("2026-07-30", "2026-08-05", "2026-08-06")), n = c(100, 108, 111))
  expect_equal(compute_wow_delta(weekly), 111 - 100)
})

test_that("compute_wow_delta returns NA when no snapshot is old enough to count as 'last week'", {
  weekly <- data.frame(Archive_Date = as.Date(c("2026-08-05", "2026-08-06")), n = c(50, 55))
  expect_true(is.na(compute_wow_delta(weekly)))
})

test_that("compute_vacancy_rate_domain falls back to a placeholder range instead of Inf/-Inf when every rate is NA", {
  expect_equal(compute_vacancy_rate_domain(c(NA_real_, NA_real_, NA_real_)), c(0, 1))
  expect_equal(compute_vacancy_rate_domain(c(0.05, NA_real_, 0.20)), c(0.05, 0.20))
})

test_that("weekly-totals downloads keep only the latest snapshot in each week", {
  weekly <- data.frame(District = c("A", "B", "A", "B", "A"),
                       Archive_Date = c("2026-08-03", "2026-08-03", "2026-08-07", "2026-08-07", "2026-08-11"),
                       n = c(5, 2, 6, 3, 7))
  out <- latest_snapshot_per_week(weekly)
  expect_equal(out$Archive_Date, c("2026-08-07", "2026-08-07", "2026-08-11"))
  expect_equal(out$n, c(6, 3, 7))
})

test_that("fill_first_seen fills blanks from the first-seen table and keeps posted dates", {
  lookup <- data.frame(Segment = c("K-12", "K-12", "HE"),
                       Key = c(first_seen_key("Frazer Public Schools", "Head Cook", "u1"),
                               first_seen_key("Frazer Public Schools", "Bus Driver", "u2"),
                               first_seen_key("Montana Tech", "Lab Aide", "u3")),
                       First_Seen = c("2026-10-27", "2026-08-18", "2026-09-01"), Left_Censored = c("FALSE", "TRUE", "FALSE"))
  got <- fill_first_seen(c(NA, "", "2026-10-01", NA), c("Frazer Public Schools", " frazer  public schools", "Frazer Public Schools", "X"),
                         c("Head Cook", "Bus Driver", "Head Cook", "Teacher"), c("u1", "u2", "u1", "u9"), "K-12", lookup)
  expect_equal(got, c("First seen 2026-10-27", "On the board since at least 2026-08-18", "2026-10-01", NA))
  # HE rows only match HE keys; no file means the column is unchanged.
  expect_equal(fill_first_seen(NA, "Montana Tech", "Lab Aide", "u3", "K-12", lookup), NA_character_)
  expect_equal(fill_first_seen(NA, "Montana Tech", "Lab Aide", "u3", "HE", lookup), "First seen 2026-09-01")
  expect_equal(fill_first_seen(c(NA, "2026-10-01"), "a", "b", "c", "K-12", NULL), c(NA, "2026-10-01"))
  expect_error(fill_first_seen(NA, "a", "b", "c", "K-12"), "lookup")
})

test_that("old posted dates read 'Listed since'", {
  old <- format(Sys.Date() - 400, "%Y-%m-%d")
  new <- format(Sys.Date() - 3, "%Y-%m-%d")
  expect_equal(format_posted_or_listed_date(c(old, new, NA, "not a date")),
               c(paste("Listed since", old), new, NA, "not a date"))
})

test_that("pay text keeps the posting's own unit", {
  expect_equal(format_posted_pay(c(21.46, 46500, 3691, NA), c(22.81, 71803, NA, NA), c("hour", "year", "stipend", NA)),
               c("$21.46\u2013$22.81/hr", "$46,500\u2013$71,803/yr", "$3,691 stipend", "\u2014"))
  expect_equal(format_posted_pay(150, 150, "day"), "$150.00/day")
  est <- format_bls_estimate(34750, "year", "Teaching Assistants", "West Montana nonmetro", FALSE)
  expect_match(est, "title=\"BLS median for teaching assistants, West Montana nonmetro\"", fixed = TRUE)
  expect_match(est, "$34,750/yr</span>", fixed = TRUE)
  expect_equal(format_bls_estimate(34750, "year", "x", "y", TRUE), "\u2014")
  expect_match(format_pay_estimate(10, "hour", "a \"quoted\" <tip>", FALSE), "&quot;quoted&quot; &lt;tip&gt;", fixed = TRUE)
})

test_that("posting_pay_value uses posted hourly/yearly pay, leaves other posted units blank, else the fallback", {
  out <- posting_pay_value(c(20, 40000, 150, NA), c(22, NA, NA, NA), c("hour", "year", "day", NA), 55000, "year")
  expect_equal(out$value, c(21, 40000, NA, 55000))
  expect_equal(out$unit, c("hour", "year", NA, "year"))
})

test_that("ACS margins of error and the SAIPE range read as text", {
  expect_equal(format_poverty_range(c(0, 0.1), c(0.32, NA)), c("about 0%\u201332%", NA))
  expect_equal(format_moe_dollars(c(2190, NA)), c(" \u00b1$2,190", ""))
  expect_equal(format_moe_points(0.004), " \u00b10.4 pts")
  expect_equal(indistinct_from_zero(c(0.01, 0.05, NA), c(0.02, 0.01, 0.01)), c(TRUE, FALSE, FALSE))
  expect_equal(county_moe_summary(c(2190, NA), c(85, NA), c(0.01, NA), c(0.02, NA)),
               c("income \u00b1$2,190; rent \u00b1$85; mining \u00b12.0 pts (not distinguishable from 0)", NA))
})

test_that("college map dot names and job searches", {
  expect_equal(he_map_name(c("MSU", "MSU", "MSU"), c(NA, "", "Havre")), c("MSU", "MSU", "MSU (Havre)"))
  titles <- c("Special Education Teacher", "SPED Para", "Math Teacher", "Bus Driver", "PE Teacher", NA, "Teacher of Art")
  expect_equal(match_job_query(titles, "sped"), c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(match_job_query(titles, "driver"), c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(match_job_query(titles, "pe"), c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE))
  expect_equal(match_job_query(titles, "art teacher"), c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE))
  expect_equal(match_job_query(titles, "  "), rep(TRUE, 7))
  # Regex characters in what a visitor types are literal text, never an error.
  odd <- c("Teacher (Math)", "C++ Developer", "Math Teacher", "Cook")
  expect_equal(match_job_query(odd, "("), c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(match_job_query(odd, "math ("), c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(match_job_query(odd, "c++"), c(FALSE, TRUE, FALSE, FALSE))
  expect_equal(match_job_query(odd, "teacher."), c(FALSE, FALSE, FALSE, FALSE))
})

test_that("validate_and_pad_schema pads a header-only file", {
  empty <- data.frame(District = character(0))
  msgs <- character(0)
  out <- validate_and_pad_schema(empty, c("District", "Title"), "x.csv", on_missing = function(m) msgs <<- c(msgs, m))
  expect_equal(names(out), c("District", "Title"))
  expect_equal(nrow(out), 0)
  expect_length(msgs, 1)
})

test_that("trends table and biggest mover compare against the nearest earlier snapshot", {
  d <- as.Date("2026-11-09") - c(0, 7, 30, 100, 400)
  h <- data.frame(Archive_Date = rep(d, each = 2), Category = rep(c("Teacher", "Para"), 5),
                  n = c(10, 4, 8, 4, 6, 5, 9, 1, 2, 4))
  t <- build_current_trends_table(h, "#2a78d6")
  expect_equal(t$Category, c("Teacher", "Para"))
  expect_equal(t$`Current Postings`, c(10, 4))
  expect_equal(unname(t$`vs Month Ago`), c("<span style='color:#1baf7a;'>\u25b2 4</span>", "<span style='color:#e34948;'>\u25bc 1</span>"))
  expect_equal(unname(t$`vs Year Ago`), c("<span style='color:#1baf7a;'>\u25b2 8</span>", "<span style='color:#999999;'>\u25b2 0</span>"))
  expect_match(t$`Last 12 Weeks`[1], "^<svg")
  expect_null(build_current_trends_table(h[0, ], "#000"))

  w <- data.frame(District = rep(c("A", "B"), 3), Archive_Date = rep(as.Date(c("2026-10-01", "2026-10-20", "2026-11-09")), each = 2),
                  n = c(5, 9, 6, 9, 7, 2))
  m <- find_biggest_mover(w, "District")
  expect_equal(m[c("name", "prior", "latest", "delta")], list(name = "B", prior = 9, latest = 2, delta = -7))
  expect_equal(m$prior_date, as.Date("2026-10-01"))
  expect_null(find_biggest_mover(w[5:6, ], "District"))
})

# Phase 6, slice 2: helpers that read an app global now take it as an argument.

test_that("validate_and_pad_schema pads missing columns with NA and reports the file once", {
  got <- character(0)
  df <- data.frame(a = 1:2)
  out <- validate_and_pad_schema(df, c("a", "b", "c"), "x.csv", on_missing = function(m) got <<- c(got, m))
  expect_identical(names(out), c("a", "b", "c"))
  expect_true(all(is.na(out$b)) && all(is.na(out$c)))
  expect_identical(got, "x.csv is missing expected column(s): b, c. That data will show blank until the pipeline is fixed.")
  expect_identical(validate_and_pad_schema(df, "a", "x.csv", on_missing = function(m) stop("not called")), df)
  expect_warning(validate_and_pad_schema(df, "z", "y.csv"), "y.csv is missing expected column\\(s\\): z")
})

test_that("in_pay_band compares each posting in its own unit, lo inclusive and hi exclusive", {
  bands <- list(year_50_60 = list(unit = "year", lo = 50000, hi = 60000, label = "x"),
                hour_25p = list(unit = "hour", lo = 25, hi = Inf, label = "y"))
  v <- c(50000, 60000, 55000, 30, 55000, NA)
  u <- c("year", "year", "hour", "hour", NA, "year")
  expect_identical(in_pay_band(v, u, "year_50_60", bands), c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_identical(in_pay_band(v, u, "hour_25p", bands), c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE))
  expect_identical(in_pay_band(v, u, NULL, bands), rep(TRUE, 6))
  expect_identical(in_pay_band(v, u, "nope", bands), rep(TRUE, 6))
})

test_that("relabel_spell_status relabels Closed / Filled only, and leaves empty or Status-less frames alone", {
  df <- data.frame(Status = c("Closed / Filled", "Open", NA), stringsAsFactors = FALSE)
  expect_identical(relabel_spell_status(df)$Status, c("No longer listed", "Open", NA))
  expect_identical(relabel_spell_status(data.frame()), data.frame())
  expect_identical(relabel_spell_status(data.frame(x = 1)), data.frame(x = 1))
})

test_that("map_marker_radius grows with the square root of the count and is capped", {
  expect_equal(map_marker_radius(c(0, 25, 10000)), c(6, 12, 24))
  expect_equal(map_marker_radius(4, min_radius = 2, max_radius = 3), 3)
})

test_that("dictionary_columns_help lists a file's columns, NULL for an unknown file", {
  d <- data.frame(File_Name = c("a.csv", "a.csv", "b.csv"), Column_Name = c("X", "Y", "Z"),
                  Definition = c("ex <1>", "why", "zed"), stringsAsFactors = FALSE)
  h <- as.character(dictionary_columns_help("a.csv", d))
  expect_match(h, "<strong>\\s*2\\s*columns:\\s*</strong>")
  expect_match(h, "<dt>\\s*<code>X</code>\\s*</dt>")
  expect_match(h, "ex &lt;1&gt;", fixed = TRUE)
  expect_false(grepl("zed", h))
  expect_null(dictionary_columns_help("c.csv", d))
})

test_that("footer_retrieved gives one date, a range, or nothing", {
  r <- data.frame(Source = c("A", "B", "B", "C"), Retrieved = c("2026-11-01", "2026-11-03", "2026-10-02", ""),
                  stringsAsFactors = FALSE)
  expect_identical(footer_retrieved("A", r), " (retrieved 2026-11-01)")
  expect_identical(footer_retrieved(c("A", "B"), r), " (retrieved 2026-10-02 to 2026-11-03)")
  expect_identical(footer_retrieved("C", r), "")
  expect_identical(footer_retrieved("A", r[0, ]), "")
})

test_that("part-time staff aren't adjunct faculty (2026-11-17 review, bug 9)", {
  t <- c("Cook I - Part Time", "Grounds Worker (Part-time & Student)", "Lab Assistant, Science (Part-Time)",
         "Part-time Faculty Pool - Spanish Instructor", "Part-Time Librarian", "Adjunct - Biology", "Professor of History")
  expect_equal(classify_he_appointment(t), c(rep("Other / not faculty", 3), rep("Adjunct/part-time faculty", 3),
                                             "Faculty/instructor (non-adjunct)"))
})

test_that("find_k12_new_postings(): an ID change or a title edit is not new; a second same-title opening is", {
  h <- data.frame(
    Archive_Date = as.Date(c(rep("2026-11-13", 3), rep("2026-11-20", 4))),
    Posting_ID = c("a", "b", "c", "a2", "b", "d", "c"),
    title = c("Math Teacher", "Aide (Open Until Filled)", "Cook", "Math Teacher", "Aide", "Counselor", "Cook"),
    location = c("HS", "MS", "ES", "HS", "MS", "HS", "ES"),
    District = "Fort Washakie",
    stringsAsFactors = FALSE
  )
  # a -> a2: a gone, same title/location/district (ID change); b: same ID, edited title; d: new
  expect_equal(find_k12_new_postings(h)$Posting_ID, "d")
  expect_equal(nrow(find_k12_new_postings(h[h$Archive_Date == as.Date("2026-11-20"), ])), 0)
  # Same title at another location is new
  h2 <- rbind(h, transform(h[4, ], Posting_ID = "e", location = "MS"))
  expect_setequal(find_k12_new_postings(h2)$Posting_ID, c("d", "e"))
  # A second Cook next to c (still listed) is new; two new Math Teachers replacing one gone: one is new
  h3 <- rbind(h, transform(h[7, ], Posting_ID = "c2"), transform(h[4, ], Posting_ID = "a3"))
  expect_setequal(find_k12_new_postings(h3)$Posting_ID, c("d", "c2", "a3"))
})

test_that("group_weekly_series counts a week with no row as 0 and starts at the group's first row", {
  # WY's Fremont County SD 25 (k12_district_weekly_totals.csv, 2026-11-24): no rows on 10-09 and 10-16, so its
  # last 12 rows reached back to 08-28 (full review 2026-11-27).
  runs <- as.character(as.Date("2026-09-11") + 7 * 0:10)
  totals <- rbind(
    data.frame(District = "Other", Archive_Date = c(runs, "2026-11-24"), n = 5, n_not_marked = 4),
    data.frame(District = "Fremont 25", Archive_Date = c("2026-08-28", "2026-09-04", runs[c(1:4, 7:11)], "2026-11-24"),
               n = c(2, 2, 2, 2, 2, 2, 4, 4, 4, 26, 26, 25), n_not_marked = 1))
  # 11-20 and 11-24 are in different ISO weeks, so both stay.
  expect_equal(group_weekly_series(totals, totals$District == "Fremont 25", "n"),
               c(2, 2, 2, 2, 0, 0, 4, 4, 4, 26, 26, 25))
  # A group first read three weeks ago gets three values, not nine leading zeros.
  new <- data.frame(District = "New", Archive_Date = tail(runs, 2), n = 3, n_not_marked = 3)
  all <- rbind(totals, new)
  expect_equal(group_weekly_series(all, all$District == "New", "n"), c(3, 3, 0))
  expect_equal(group_weekly_series(all, all$District == "None", "n"), numeric(0))
})

test_that("js_attr_string keeps an apostrophe inside the JS string once the browser decodes the attribute", {
  x <- js_attr_string(c("St Mary's School", 'A "B" & C\\D'))
  expect_equal(x[1], "St Mary\\&#39;s School")  # decodes to St Mary\'s School
  expect_equal(x[2], "A &quot;B&quot; &amp; C\\\\D")
})

test_that("county_subtitle adds County only where the name lacks it", {
  expect_equal(county_subtitle(c("Gallatin County", "Albany County, Wyoming", "Park", NA, "")),
               c("Gallatin County", "Albany County", "Park County", NA, NA))
})

test_that("trend_date_labels shows the year for month-or-wider ticks and the day for closer ones", {
  expect_equal(trend_date_labels(as.Date(c("2025-01-01", "2025-04-01", NA))), c("Jan 2025", "Apr 2025", NA))
  expect_equal(trend_date_labels(as.Date(c("2026-11-02", "2026-11-09"))), c("Nov 02", "Nov 09"))
  expect_equal(trend_date_labels(as.Date("2026-11-02")), "Nov 2026")
})
