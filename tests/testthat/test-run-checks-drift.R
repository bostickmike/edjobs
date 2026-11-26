# The drift-family weekly checks (run_checks_drift.R; package review step 6, slice 2). Parity with the states' old
# scripts was checked on their committed data and archives (claude-notes/2026-11-19_step6-parity/parity2.sh); these
# tests cover the state hooks and one small end-to-end run.

test_that("state_config validates the new hooks", {
  expect_error(state_config("T", "00", "app", k12_source_names = "District"), "k12_source_names must be a function")
  expect_error(state_config("T", "00", "app", direct_registry = list()), "direct_registry must be a function")
  expect_error(state_config("T", "00", "app", drift_min_mean = -1), "drift_min_mean")
  expect_equal(state_config("T", "00", "app")$drift_min_mean, 3)
})

test_that("source names: District by default, the state's function otherwise, NULL leaves an archive out", {
  df <- data.frame(District = c("A", "B"), Posting_Source = c("feed", "board"))
  expect_equal(state_k12_source_names(df, state_config("T", "00", "app")), c("A", "B"))
  cfg <- state_config("T", "00", "app", k12_source_names = function(d) ifelse(d$Posting_Source == "feed", "Feed", d$District))
  expect_equal(state_k12_source_names(df, cfg), c("Feed", "B"))
  cfg <- state_config("T", "00", "app", k12_source_names = function(d) if (!"Posting_Source" %in% names(d)) NULL else d$District)
  expect_null(state_k12_source_names(data.frame(District = "A"), cfg))
})

test_that("the drift step flags a source that went quiet, at the state's floor, and writes its table", {
  root <- withr::local_tempdir(); withr::local_dir(root)
  dir.create("app"); dir.create("Archivek12_Data"); dir.create("Archived_HE_Data")
  weeks <- format(as.Date("2026-10-01") + 7 * 0:3)
  for (w in weeks) {
    write.csv(data.frame(District = c(rep("A", 5), rep("B", 2))), file.path("Archivek12_Data", paste0("combined_", w, ".csv")), row.names = FALSE)
    writexl::write_xlsx(data.frame(Institution = rep("U", 4)), file.path("Archived_HE_Data", paste0("hedata_", w, ".xlsx")))
  }
  write.csv(data.frame(District = c("B", "B"), Posting_Source = "board"), file.path("app", "combinedclean.csv"), row.names = FALSE)
  writexl::write_xlsx(data.frame(Institution = rep("U", 4)), file.path("app", "hedata.xlsx"))
  feed <- list(short_name = "F", label = "Feed", url = "https://example.org", discrepancy_file = "none.csv")
  old <- current_state_config(); withr::defer(set_state_config(old))

  set_state_config(state_config("T", "00", "app", feed = feed))
  out <- capture.output(f <- check_drift_step(today = "2026-11-01", out_csv = file.path(root, "flagged.csv")))
  expect_equal(f$name, "A")                     # 5 a week, now 0
  expect_true(file.exists(file.path(root, "flagged.csv")))
  expect_match(out[1], "^Drift check: 2 K-12 baseline sources, 1 HE baseline sources, 1 flagged")

  # A district moved to a no-board Platform keeps its old baseline: skipped, and said so.
  write.csv(data.frame(District = c("A", "B"), Platform = c("OPIOnly", "Board")), "k12_district_registry.csv", row.names = FALSE)
  out <- capture.output(f <- check_drift_step(today = "2026-11-01", out_csv = file.path(root, "flagged.csv"),
                                              skip_platforms = "OPIOnly"))
  expect_equal(nrow(f), 0)
  expect_true(any(grepl("not judged (no board of their own, Platform OPIOnly): A", out, fixed = TRUE)))
  capture.output(f <- check_drift_step(today = "2026-11-01", out_csv = file.path(root, "flagged.csv")))
  expect_equal(f$name, "A")                     # without the argument, as before
  unlink("k12_district_registry.csv")

  # A floor above A's average: nothing is judged.
  set_state_config(state_config("T", "00", "app", feed = feed, drift_min_mean = 6))
  capture.output(f <- check_drift_step(today = "2026-11-01", out_csv = file.path(root, "flagged.csv")))
  expect_equal(nrow(f), 0)
})
