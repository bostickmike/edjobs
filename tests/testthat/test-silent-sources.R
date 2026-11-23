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

test_that("a silent source whose scrape errored this run is listed, acknowledged or not", {
  # An acknowledgement says the board is really empty; an error means it may be broken (test-kit item 6).
  silent <- data.frame(source = c("Clancy", "Zurich", "Saco"), acknowledged = c(TRUE, FALSE, TRUE),
                       last_with_rows = NA_character_, type = "K-12",
                       scrape_error = c("CyberSchool page has no jobs list (.listItem_list): not the jobs page, or the site changed",
                                        NA, NA))
  md <- build_silent_sources_report_markdown(silent, 4, date = "2026-11-23")
  expect_true(paste0("- **Clancy** (K-12): no rows in any archived weekly run (acknowledged as empty); this run's ",
                     "scrape errored: CyberSchool page has no jobs list (.listItem_list): not the jobs page, or the site changed") %in% md)
  expect_true("- **Zurich** (K-12): no rows in any archived weekly run" %in% md)
  expect_false(any(grepl("Saco", md)))
  expect_true(any(grepl("^1 acknowledged silent source", md)))
})

test_that("check_silent_sources_step reads this run's scrape log", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  dir.create("Archivek12_Data"); dir.create("Archived_HE_Data")
  for (d in c("2026-11-02", "2026-11-09")) {
    utils::write.csv(data.frame(title = "Teacher", District = "Busy", Posting_Source = "Busy board"),
                     file.path("Archivek12_Data", paste0("combined_", d, ".csv")), row.names = FALSE)
    writexl::write_xlsx(data.frame(Title = "Dean", Institution = "College A"),
                        file.path("Archived_HE_Data", paste0("hedata_", d, ".xlsx")))
  }
  utils::write.csv(data.frame(District = c("Busy", "Acked"), Platform = "CyberSchool"), "k12_district_registry.csv",
                   row.names = FALSE)
  utils::write.csv(data.frame(Institution = "College A"), "he_institution_registry.csv", row.names = FALSE)
  utils::write.csv(data.frame(Type = "K-12", Source = "Acked", Reason = "empty board", Acknowledged = "2026-11-22"),
                   "silent_sources_acknowledged.csv", row.names = FALSE)
  utils::write.csv(data.frame(timestamp = c("2026-11-09 10:00:00", "2026-11-09 10:00:01"),
                              source = c("Busy", "CyberSchool: Acked"), status = c("ok", "error"), n_rows = 0L,
                              error_message = c(NA, "no jobs list")), "scrape_log.csv", row.names = FALSE)
  old <- current_state_config(); withr::defer(set_state_config(old))
  set_state_config(state_config("Test", "99", "app"))
  rep <- file.path(dir, "report.md")
  utils::capture.output(res <- check_silent_sources_step(today = "2026-11-09", n_runs = 2,
                                                         out_csv = file.path(dir, "s.csv"), report_path = rep))
  expect_identical(res$source, "Acked")
  expect_identical(res$scrape_error, "no jobs list")
  expect_true(any(grepl("**Acked** (K-12): no rows in any archived weekly run (acknowledged as empty); this run's scrape errored: no jobs list",
                        readLines(rep), fixed = TRUE)))
})

test_that("a district's LLM shadow-pilot or reference-table error is not the district's scrape error", {
  log <- data.frame(timestamp = c("2026-11-23 10:00:00", "2026-11-23 10:00:05", "2026-11-23 10:00:06"),
                    source = c("Zurich Elementary School District #17", "LLMExtract: Zurich Elementary School District #17",
                               "Reference: Zurich Elementary School District #17"),
                    status = c("empty", "error", "error"), n_rows = 0L,
                    error_message = c(NA, "HTTP 429 (Gemini)", "timeout"))
  got <- attach_scrape_log_errors(data.frame(name = "Zurich Elementary School District #17"), log)
  expect_true(is.na(got$scrape_error))
  log$status[1] <- "error"; log$error_message[1] <- "no jobs list"
  expect_identical(attach_scrape_log_errors(data.frame(name = "Zurich Elementary School District #17"), log)$scrape_error,
                   "no jobs list")
})

# Empty_Text (2026-11-22): an acknowledgement holds only while the board still reads as empty. Fixtures: the real
# Fishtail (blank WordPress page) and Cottonwood (Google Sites "no job openings") boards, fetched 2026-11-22,
# scripts and styles removed.
fx <- function(name) paste(readLines(testthat::test_path("fixtures", name), warn = FALSE), collapse = "\n")

test_that("page_visible_text reads the text a person sees, not scripts", {
  expect_match(page_visible_text(fx("silent_ack_fishtail_2026-11-22.html")), "Home > Job Openings sample@email.com", fixed = TRUE)
  expect_match(page_visible_text(fx("silent_ack_cottonwood_2026-11-22.html")), "There are currently no job openings", fixed = TRUE)
  expect_identical(page_visible_text("<html><body><script>var x = 'Teacher';</script><p>Hi\n  there</p></body></html>"), "Hi there")
})

test_that("check_ack_empty_text passes an empty board and flags a posting, a failed fetch or a bad pattern", {
  fish <- fx("silent_ack_fishtail_2026-11-22.html")
  # The same page with one posting between the breadcrumb and the footer.
  posted <- sub("sample@email.com", "</p><h2>4th Grade Teacher</h2><p>sample@email.com", fish, fixed = TRUE)
  expect_false(grepl("Home > Job Openings sample@email", page_visible_text(posted), fixed = TRUE))
  ack <- data.frame(Type = "K-12", Source = c("Fishtail", "Nye", "Cottonwood", "Bad", "NoText"),
                    Empty_Text = c("Home > Job Openings sample@email\\.com", "x", "There are currently no job openings", "(", ""))
  links <- c("K-12|Fishtail" = "f", "K-12|Nye" = "n", "K-12|Cottonwood" = "c", "K-12|Bad" = "f")
  pages <- list(f = fish, c = fx("silent_ack_cottonwood_2026-11-22.html"))
  fetch <- function(url) if (url %in% names(pages)) pages[[url]] else stop("HTTP 503")
  res <- check_ack_empty_text(ack, links, fetch)
  expect_identical(names(res), c("K-12|Fishtail", "K-12|Nye", "K-12|Cottonwood", "K-12|Bad"))   # NoText: no check
  expect_true(is.na(res[["K-12|Fishtail"]])); expect_true(is.na(res[["K-12|Cottonwood"]]))
  expect_match(res[["K-12|Nye"]], "didn't load.*HTTP 503")
  expect_match(res[["K-12|Bad"]], "isn't a valid pattern")
  pages$f <- posted
  expect_match(check_ack_empty_text(ack[1, ], links, fetch)[[1]], "no longer shows its empty text")
  expect_length(check_ack_empty_text(ack[, c("Type", "Source")], links, fetch), 0)
})

test_that("check_silent_sources_step lists an acknowledged source whose board no longer reads as empty", {
  fish <- fx("silent_ack_fishtail_2026-11-22.html")
  pages <- list(f = sub("sample@email.com", "</p><h2>4th Grade Teacher</h2><p>sample@email.com", fish, fixed = TRUE),
                c = fx("silent_ack_cottonwood_2026-11-22.html"))
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  dir.create("Archivek12_Data"); dir.create("Archived_HE_Data")
  for (d in c("2026-11-02", "2026-11-09")) {
    utils::write.csv(data.frame(title = "Teacher", District = "Busy", Posting_Source = "Busy board"),
                     file.path("Archivek12_Data", paste0("combined_", d, ".csv")), row.names = FALSE)
    writexl::write_xlsx(data.frame(Title = "Dean", Institution = "College A"),
                        file.path("Archived_HE_Data", paste0("hedata_", d, ".xlsx")))
  }
  utils::write.csv(data.frame(District = c("Busy", "Fishtail", "Cottonwood"), Platform = "WordPress",
                              Job_Link = c("b", "f", "c")), "k12_district_registry.csv", row.names = FALSE)
  utils::write.csv(data.frame(Institution = "College A"), "he_institution_registry.csv", row.names = FALSE)
  utils::write.csv(data.frame(Type = "K-12", Source = c("Fishtail", "Cottonwood"), Reason = "empty board",
                              Acknowledged = "2026-11-22",
                              Empty_Text = c("Home > Job Openings sample@email\\.com", "There are currently no job openings")),
                   "silent_sources_acknowledged.csv", row.names = FALSE)
  old <- current_state_config(); withr::defer(set_state_config(old))
  set_state_config(state_config("Test", "99", "app"))
  rep <- file.path(dir, "report.md")
  out <- utils::capture.output(res <- check_silent_sources_step(today = "2026-11-09", n_runs = 2, out_csv = file.path(dir, "s.csv"),
                                                                report_path = rep, fetch = function(u) pages[[u]]))
  expect_identical(res$source, c("Fishtail", "Cottonwood"))
  expect_match(res$empty_check[1], "no longer shows its empty text"); expect_true(is.na(res$empty_check[2]))
  md <- readLines(rep)
  expect_true(any(grepl("**Fishtail** (K-12): no rows in any archived weekly run (acknowledged as empty); needs a look: board page no longer shows",
                        md, fixed = TRUE)))
  expect_false(any(grepl("Cottonwood", md)))
  expect_true(any(grepl("(1 acknowledged and not erroring)", out, fixed = TRUE)))
})
