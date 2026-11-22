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
  # A gap restarts the count: missing from the last report means new again.
  gap <- list(prev[[1]], list(created = "2026-11-10T20:00:00Z", body = "### Something else"))
  expect_equal(classify_drift_repeats(opi(), gap)$status, rep("new", 3))
  expect_equal(classify_drift_repeats(opi(), prev[2], min_runs = 3)$status, rep("repeat", 3))
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
  expect_true("- On OPI but not on our scrape (counted from OPI; possible scraper misses): Baker Public Schools -- 3 reports, since 2026-11-03" %in% g)
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
  expect_true(any(grepl("^0 new, 0 still unresolved .* 3 seen in the last report too", out)))
  expect_equal(utils::tail(out, length(opi())), opi())
  writeLines(opi(), "report.md")
  utils::capture.output(mark_drift_repeats_step("report.md", "missing.json", "none.csv"))
  expect_true(any(grepl("^3 new, 0 still unresolved", readLines("report.md"))))
  unlink("report.md")
  expect_output(mark_drift_repeats_step("report.md", "prev.json"), "No drift report")
})
