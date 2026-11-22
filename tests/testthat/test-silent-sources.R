test_that("find_silent_sources lists sources with no rows in each of the last n runs", {
  counts <- list(`2026-10-27` = c(A = 3L, B = 1L), `2026-11-03` = c(A = 2L),
                 `2026-11-10` = c(A = 1L, C = 0L), `2026-11-17` = c(A = 4L))
  s <- find_silent_sources(counts, c("A", "B", "C", "D"), n_runs = 3)
  expect_identical(s$source, c("B", "C", "D"))
  expect_identical(s$last_with_rows, c("2026-10-27", NA, NA))
  expect_false(any(s$acknowledged))
  # B had rows inside the window of four runs, so it isn't silent there
  expect_identical(find_silent_sources(counts, c("A", "B", "C", "D"), n_runs = 4)$source, c("C", "D"))
})

test_that("find_silent_sources marks acknowledged sources and needs n runs on hand", {
  counts <- list(a = c(A = 1L), b = c(A = 1L))
  s <- find_silent_sources(counts, c("A", "Z"), n_runs = 2, acknowledged = "Z")
  expect_identical(s$source, "Z")
  expect_true(s$acknowledged)
  expect_identical(nrow(find_silent_sources(counts, c("A", "Z"), n_runs = 3)), 0L)
  expect_identical(nrow(find_silent_sources(counts, character(0), n_runs = 2)), 0L)
})

test_that("the report lists unacknowledged sources only and says how many were left out", {
  silent <- data.frame(source = c("Zurich", "Clancy", "Missoula College"), acknowledged = c(FALSE, TRUE, FALSE),
                       last_with_rows = c(NA, NA, "2026-09-01"), type = c("K-12", "K-12", "Higher Ed"))
  md <- build_silent_sources_report_markdown(silent, 4, date = "2026-11-23")
  expect_match(md[1], "no rows in their last 4 weekly runs \\(2026-11-23\\)")
  expect_true("- **Zurich** (K-12): no rows in any archived weekly run" %in% md)
  expect_true("- **Missoula College** (Higher Ed): last rows 2026-09-01" %in% md)
  expect_false(any(grepl("Clancy", md)))
  expect_true(any(grepl("^1 acknowledged silent source", md)))
  expect_length(build_silent_sources_report_markdown(silent[2, ], 4), 0)
})

test_that("check_silent_sources_step counts board rows, not the feed's, and reads the acknowledgement file", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  dir.create("Archivek12_Data"); dir.create("Archived_HE_Data")
  feed <- "Statewide feed"
  for (d in c("2026-11-02", "2026-11-11", "2026-11-16")) {
    utils::write.csv(data.frame(title = c("Teacher", "Aide", "Clerk"), District = c("Busy", "FeedOnly", "Acked"),
                                Posting_Source = c("Busy board", feed, feed)),
                     file.path("Archivek12_Data", paste0("combined_", d, ".csv")), row.names = FALSE)
    writexl::write_xlsx(data.frame(Title = "Dean", Institution = "College A"),
                        file.path("Archived_HE_Data", paste0("hedata_", d, ".xlsx")))
  }
  # the week's last run counts: an earlier run that week with rows for FeedOnly is not a weekly run
  utils::write.csv(data.frame(title = "Teacher", District = "FeedOnly", Posting_Source = "FeedOnly board"),
                   file.path("Archivek12_Data", "combined_2026-11-09.csv"), row.names = FALSE)
  utils::write.csv(data.frame(District = c("Busy", "FeedOnly", "Acked", "NoBoard"),
                              Platform = c("X", "X", "X", "OPIOnly")), "k12_district_registry.csv", row.names = FALSE)
  utils::write.csv(data.frame(Institution = c("College A", "College B")), "he_institution_registry.csv", row.names = FALSE)
  utils::write.csv(data.frame(Type = "K-12", Source = "Acked", Reason = "empty board", Acknowledged = "2026-11-22"),
                   "silent_sources_acknowledged.csv", row.names = FALSE)
  old <- current_state_config(); withr::defer(set_state_config(old))
  set_state_config(state_config("Test", "99", "app", feed = list(short_name = "F", label = feed, url = "https://example.org", discrepancy_file = "d.csv")))
  out <- file.path(dir, "silent.csv"); rep <- file.path(dir, "report.md")
  s <- suppressMessages(utils::capture.output(
    res <- check_silent_sources_step(today = "2026-11-16", n_runs = 3, skip_platforms = "OPIOnly",
                                     out_csv = out, report_path = rep)))
  expect_identical(res$source, c("FeedOnly", "Acked", "College B"))
  expect_identical(res$acknowledged, c(FALSE, TRUE, FALSE))
  md <- readLines(rep)
  expect_true("- **FeedOnly** (K-12): no rows in any archived weekly run" %in% md)
  expect_false(any(grepl("Acked|NoBoard|Busy", md)))
})
