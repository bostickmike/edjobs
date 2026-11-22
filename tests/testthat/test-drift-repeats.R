# New vs still unresolved in the drift issue (test-kit item 3, option B). Report shapes are cut from MT drift issue
# #11's comments (claude-notes/2026-11-21_step8-review/mt_issue11_comments.md).

report <- function(...) unlist(strsplit(paste(c(...), collapse = "\n"), "\n"))
opi <- function(n_baker = 4) report(
  "### OPI cross-check", "",
  "#### On OPI but not on our scrape (counted from OPI; possible scraper misses)", "",
  sprintf("- **Baker Public Schools** (%d): ", n_baker), "  - **Teacher** nested, not an item",
  "- **Richey Public Schools** (5): ",
  "#### Districts we don't scrape that posted on OPI (adoption candidates)", "",
  "- **Kinsey Elem** (Custer County, 3 postings) -- no website on OPI")

test_that("drift_report_items keys items by section and name, ignoring counts and summary sections", {
  md <- report(opi(),
    "### OPI postings at districts we scrape directly", "",
    "- **Statewide Feed (OPI)**: 392 active vacancies captured.",
    "### Titles that don't look like job titles (2026-11-20)", "",
    "- **Ulm Public Schools** (K-12): \"Annual Parent Notice ...\"",
    "---", "", "Automated salary VALUE plausibility check flagged something worth a human look.", "",
    "- **K-12 teacher 10th pctile salary (MT DLI)** (Swan River School District #4): $16,900 is outside ...")
  it <- drift_report_items(md)
  expect_equal(it$item, c("Baker Public Schools", "Richey Public Schools", "Kinsey Elem (Custer County)",
                          "Ulm Public Schools (K-12)",
                          "K-12 teacher 10th pctile salary (MT DLI) (Swan River School District #4)"))
  expect_equal(it$section[c(1, 3, 4, 5)], c("On OPI but not on our scrape (counted from OPI; possible scraper misses)",
                                            "Districts we don't scrape that posted on OPI (adoption candidates)",
                                            "Titles that don't look like job titles", "salary VALUE plausibility check"))
  # The same item with other numbers is the same key.
  expect_equal(drift_report_items(opi(9))$key, drift_report_items(opi(4))$key)
  expect_equal(nrow(drift_report_items(character(0))), 0)
})

test_that("classify_drift_repeats counts reports in a row, with the date of the first", {
  prev <- list(list(created = "2026-11-03T20:00:00Z", body = paste(opi(), collapse = "\n")),
               list(created = "2026-11-10T20:00:00Z", body = paste(opi(3), collapse = "\n")))
  cur <- report(opi(2), "### Titles that don't look like job titles", "", "- **Ulm Public Schools** (K-12): \"...\"")
  r <- classify_drift_repeats(cur, prev, min_runs = 3)
  expect_equal(r$runs, c(3L, 3L, 3L, 1L))
  expect_equal(r$status, c("unresolved", "unresolved", "unresolved", "new"))
  expect_equal(r$since[1], "2026-11-03")
  # A real gap restarts the count: the section is there, the item isn't.
  gap <- list(prev[[1]], list(created = "2026-11-10T20:00:00Z", body = paste(report(
    "#### On OPI but not on our scrape (counted from OPI; possible scraper misses)", "", "- **Sidney Public Schools** (2): ",
    "#### Districts we don't scrape that posted on OPI (adoption candidates)", "", "- **Dodson K-12** (Phillips County)"),
    collapse = "\n")))
  expect_equal(classify_drift_repeats(opi(), gap)$status, rep("new", 3))
  expect_equal(classify_drift_repeats(opi(), prev[2], min_runs = 3)$status, rep("repeat", 3))
})

test_that("a report without the item's section, or with it cut short, neither extends nor breaks the run", {
  # A crashed check writes nothing; MT's OPI list stops at 15 districts with "- ... and N more".
  crashed <- list(created = "2026-11-10", body = "### Titles that don't look like job titles\n\n- **Ulm Public Schools** (K-12): x")
  cut <- list(created = "2026-11-12", body = paste(report(
    "#### On OPI but not on our scrape (counted from OPI; possible scraper misses)", "", "- **Sidney Public Schools** (9): ",
    "- ... and 3 more districts"), collapse = "\n"))
  prev <- list(list(created = "2026-11-03", body = paste(opi(), collapse = "\n")), crashed, cut)
  r <- classify_drift_repeats(opi(), prev)
  expect_equal(r$runs[1:2], c(2L, 2L))
  expect_equal(r$since[1], "2026-11-03")
})

test_that("runs are run days: re-runs on one day count once, and today's earlier reports not at all", {
  day <- function(t) list(created = t, body = paste(opi(), collapse = "\n"))
  prev <- list(day("2026-11-17T20:49:00Z"), day("2026-11-17T21:32:00Z"), day("2026-11-17T22:15:00Z"))
  expect_equal(classify_drift_repeats(opi(), prev, today = "2026-11-18")$runs, rep(2L, 3))
  expect_equal(classify_drift_repeats(opi(), prev, today = "2026-11-17")$status, rep("new", 3))
})

test_that("each 'Automated ... flagged' line starts a section, so a salary coverage drop doesn't rename the value items", {
  value <- report("---", "", "Automated salary VALUE plausibility check flagged something worth a human look.", "",
                  "- **K-12 teacher 10th pctile salary (MT DLI)** (Swan River School District #4): $16,900 ...")
  both <- report("---", "", "Automated salary-source coverage check flagged a drop.", "", "- **WSBA teacher salary**: 40 -> 10 districts",
                 "", "Automated salary VALUE plausibility check flagged something worth a human look.", "",
                 "- **K-12 teacher 10th pctile salary (MT DLI)** (Swan River School District #4): $16,900 ...",
                 "", "Reference tables behind the pay estimates need a look.", "", "- **DLI wages**: stale")
  it <- drift_report_items(both)
  expect_equal(it$section, c("salary-source coverage check", "salary VALUE plausibility check",
                             "Reference tables behind the pay estimates"))
  prev <- list(list(created = "2026-11-03", body = paste(value, collapse = "\n")),
               list(created = "2026-11-10", body = paste(value, collapse = "\n")))
  r <- classify_drift_repeats(both, prev)
  expect_equal(r$status[r$item == "K-12 teacher 10th pctile salary (MT DLI) (Swan River School District #4)"], "unresolved")
})

test_that("sections that flag without per-source items are named in the block", {
  md <- report(opi(), "---", "", "### Rebuild parity", "", "| file | ok |", "|---|---|", "| combinedclean.csv | FALSE |",
               "---", "", "**Monitoring self-check:** step(s) crashed: check_pay_coverage.R",
               "---", "", "### Titles whose words point to a different bucket", "", "- \"Para Cook\" (K-12): Para")
  p <- parse_drift_report(md)
  expect_equal(p$unitemized, c("Rebuild parity", "Monitoring self-check", "Titles whose words point to a different bucket"))
  g <- build_drift_glance_markdown(classify_drift_repeats(md), unitemized = p$unitemized)
  expect_true(any(grepl("Also in this report, not counted above (read them below): Rebuild parity; Monitoring self-check;",
                        g, fixed = TRUE)))
  # A report of only unitemized sections still gets a block.
  expect_true(length(build_drift_glance_markdown(classify_drift_repeats(character(0)), unitemized = "Rebuild parity")) > 0)
})

test_that("acknowledged items leave the lists; the block counts them and skips an earlier block", {
  prev <- list(list(created = "2026-11-03", body = paste(opi(), collapse = "\n")),
               list(created = "2026-11-10", body = paste(opi(), collapse = "\n")))
  ack <- data.frame(Section = c("", "Districts we don't scrape that posted on OPI (adoption candidates)"),
                    Item = c("Richey Public Schools", "Kinsey Elem (Custer County)"),
                    Reason = c("posts teaching jobs only on OPI", "not adopting"), Acknowledged = "2026-11-22")
  r <- classify_drift_repeats(opi(), prev, ack)
  expect_equal(r$acknowledged, c(FALSE, TRUE, TRUE))
  g <- build_drift_glance_markdown(r)
  expect_equal(g[1], GLANCE_START)
  expect_true(any(grepl("^0 new, 1 still unresolved .* 2 acknowledged", g)))
  expect_true("- On OPI but not on our scrape (counted from OPI; possible scraper misses): Baker Public Schools -- 3 run days, since 2026-11-03" %in% g)
  expect_false(any(grepl("Richey|Kinsey", g)))
  # A report that already carries a block (last week's comment) is read without it.
  expect_equal(drift_report_items(c(g, opi()))$key, drift_report_items(opi())$key)
  expect_length(build_drift_glance_markdown(classify_drift_repeats(character(0))), 0)
})

test_that("mark_drift_repeats_step puts the block on top, and works with no history", {
  dir <- withr::local_tempdir(); withr::local_dir(dir)
  writeLines(opi(), "report.md")
  jsonlite::write_json(list(list(created = "2026-11-10T20:00:00Z", body = paste(opi(), collapse = "\n"))),
                       "prev.json", auto_unbox = TRUE)
  utils::capture.output(r <- mark_drift_repeats_step("report.md", "prev.json", "none.csv"))
  out <- readLines("report.md")
  expect_equal(out[1], GLANCE_START)
  expect_true(any(grepl("^0 new, 0 still unresolved .* 3 seen on the last run day too", out)))
  expect_equal(utils::tail(out, length(opi())), opi())
  writeLines(opi(), "report.md")
  utils::capture.output(mark_drift_repeats_step("report.md", "missing.json", "none.csv"))
  expect_true(any(grepl("^3 new, 0 still unresolved", readLines("report.md"))))
  unlink("report.md")
  expect_output(mark_drift_repeats_step("report.md", "prev.json"), "No drift report")
})

test_that("an acknowledgement with Details covers only what was checked; anything else is new again", {
  sec <- "On OPI but not on our scrape (counted from OPI; possible scraper misses)"
  baker <- function(...) report("#### On OPI but not on our scrape (counted from OPI; possible scraper misses)", "",
                                sprintf("- **Baker Public Schools** (%d): %s", length(c(...)), paste(c(...), collapse = "; ")))
  checked <- c("English (posted 2026-10-28)", "Special Education (posted 2026-10-05)")
  expect_equal(drift_report_items(baker(checked))$details, paste(checked, collapse = "; "))
  prev <- lapply(c("2026-11-03", "2026-11-10"), function(d) list(created = d, body = paste(baker(checked), collapse = "\n")))
  ack <- data.frame(Section = sec, Item = "Baker Public Schools", Details = paste(checked, collapse = "; "),
                    Reason = "checked on the board", Acknowledged = "2026-11-22")
  # The checked postings, or fewer of them (one closed): acknowledged.
  r <- classify_drift_repeats(baker(checked), prev, ack)
  expect_true(r$acknowledged); expect_equal(r$unchecked, "")
  expect_true(classify_drift_repeats(baker(checked[1]), prev, ack)$acknowledged)
  # A posting nobody checked: back as new, naming it; the run count is kept.
  r <- classify_drift_repeats(baker(c(checked, "Mathematics (posted 2026-11-20)")), prev, ack)
  expect_false(r$acknowledged); expect_equal(r$status, "new"); expect_equal(r$runs, 3L)
  expect_equal(r$unchecked, "Mathematics (posted 2026-11-20)")
  g <- build_drift_glance_markdown(r)
  expect_true(paste0("- ", sec, ": Baker Public Schools -- not yet checked: Mathematics (posted 2026-11-20)") %in% g)
  # A relisting with a new date is a new posting too.
  expect_false(classify_drift_repeats(baker("English (posted 2026-11-21)"), prev, ack)$acknowledged)
  # A figure: acknowledged while the report says the same thing.
  swan <- function(v) report("Automated salary VALUE plausibility check flagged something worth a human look.", "",
    sprintf("- **K-12 teacher 10th pctile salary (MT DLI)** (Swan River School District #4): %s is outside the expected $20,000-$150,000 range", v))
  a2 <- data.frame(Section = "salary VALUE plausibility check", Item = "K-12 teacher 10th pctile salary (MT DLI) (Swan River School District #4)",
                   Details = "$16,900 is outside the expected $20,000-$150,000 range")
  expect_true(classify_drift_repeats(swan("$16,900"), list(), a2)$acknowledged)
  expect_false(classify_drift_repeats(swan("$1,690"), list(), a2)$acknowledged)
  # Empty or missing Details: the whole item, as before.
  a3 <- transform(ack, Details = ""); expect_true(classify_drift_repeats(baker("Mathematics (posted 2026-11-20)"), prev, a3)$acknowledged)
  expect_true(classify_drift_repeats(baker("Mathematics (posted 2026-11-20)"), prev, ack[, c("Section", "Item")])$acknowledged)
})
