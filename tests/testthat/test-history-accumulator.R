# append_weekly_rows() and the K-12 builders (R/k12_history.R).

test_that("append_weekly_rows bootstraps from a missing file", {
  new_rows <- data.frame(District = c("A", "B"), Archive_Date = "2026-08-06", n = c(1, 2))
  tmp <- withr::local_tempfile()
  expect_false(file.exists(tmp))

  result <- append_weekly_rows(tmp, new_rows)
  expect_equal(nrow(result), 2)
  expect_equal(result$Archive_Date, c("2026-08-06", "2026-08-06"))
})

test_that("append_weekly_rows appends onto an existing file without disturbing prior weeks", {
  tmp <- withr::local_tempfile()
  existing <- data.frame(District = c("A", "B"), Archive_Date = "2026-07-30", n = c(5, 6))
  write.csv(existing, tmp, row.names = FALSE)

  new_rows <- data.frame(District = c("A", "B"), Archive_Date = "2026-08-06", n = c(1, 2))
  result <- append_weekly_rows(tmp, new_rows)

  expect_equal(nrow(result), 4)
  expect_setequal(unique(result$Archive_Date), c("2026-07-30", "2026-08-06"))
  expect_equal(result$n[result$Archive_Date == "2026-07-30" & result$District == "A"], 5)
})

test_that("append_weekly_rows is idempotent -- a same-day re-run replaces, not duplicates, that day", {
  tmp <- withr::local_tempfile()
  existing <- data.frame(District = c("A", "B"), Archive_Date = c("2026-07-30", "2026-08-06"), n = c(5, 1))
  write.csv(existing, tmp, row.names = FALSE)

  rerun_rows <- data.frame(District = "A", Archive_Date = "2026-08-06", n = 99)
  result <- append_weekly_rows(tmp, rerun_rows)

  expect_equal(nrow(result), 2)
  expect_equal(result$n[result$Archive_Date == "2026-08-06"], 99)
  expect_equal(result$n[result$Archive_Date == "2026-07-30"], 5)
})

test_that("append_weekly_rows refuses to append rows with more than one Archive_Date", {
  new_rows <- data.frame(District = c("A", "B"), Archive_Date = c("2026-08-06", "2026-08-07"), n = c(1, 2))
  expect_error(append_weekly_rows(withr::local_tempfile(), new_rows), "exactly one")
})

test_that("append_weekly_rows fails loudly on a column mismatch instead of writing a jagged file", {
  tmp <- withr::local_tempfile()
  write.csv(data.frame(District = "A", Archive_Date = "2026-07-30", n = 5), tmp, row.names = FALSE)

  new_rows <- data.frame(District = "A", Archive_Date = "2026-08-06", Category = "Math", n = 1)
  expect_error(append_weekly_rows(tmp, new_rows), "column mismatch")
})

test_that("append_weekly_rows handles a genuinely empty new-rows data frame without erroring", {
  tmp <- withr::local_tempfile()
  write.csv(data.frame(District = "A", Archive_Date = "2026-07-30", n = 5), tmp, row.names = FALSE)

  empty <- data.frame(District = character(0), Archive_Date = character(0), n = numeric(0))
  result <- append_weekly_rows(tmp, empty)
  expect_equal(nrow(result), 1)
})

# ---- the K-12 builders -------------------------------------------------------

k12_rows <- function() data.frame(
  title = c("Math Teacher", "Mathematics", "Custodian", "Sample Certified Position", "English Teacher"),
  Archive_Date = "2026-11-10", date_posted = "2026-11-01", location = "HS",
  url = paste0("https://x.example/", 1:5),
  District = c("Alpha Schools", "BETA", "Alpha Schools", "Alpha Schools", "Gamma Schools"),
  stringsAsFactors = FALSE)
oews_none <- function() data.frame(Area_Code = character(0), stringsAsFactors = FALSE)
with_feed_label <- function(env = parent.frame()) {
  old <- current_state_config()
  set_state_config(state_config("Test", "00", "app", feed = list(short_name = "F", label = "Statewide feed",
                                                                 url = "https://feed.example", discrepancy_file = "d.csv")))
  withr::defer(set_state_config(old), envir = env)
}
# attach_oews_estimates() is tested with the OEWS scraper; here it only adds its columns.
stub_oews <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(attach_oews_estimates = function(postings, wages, county, state, ...) {
    for (col in OEWS_ESTIMATE_COLUMNS) postings[[col]] <- NA
    postings$Est_Area <- ifelse(is.na(county), state, county)
    postings
  }, .env = env)
}

test_that("build_k12_combinedclean writes the shared columns from a state's own column names", {
  with_feed_label(); stub_oews()
  rows <- k12_rows()
  rows$posting_id <- c("applitrack:alpha:1", "fallback | beta | x", "applitrack:alpha:3", "applitrack:alpha:4", "ss:5")
  out <- build_k12_combinedclean(rows, oews_none(), data.frame(District = "Alpha Schools", County = "Alpha County"), "WY",
                                 columns = c(Posting_ID = "posting_id"))
  expect_equal(names(out), K12_POSTING_COLUMNS)
  expect_equal(out$Posting_ID, rows$posting_id[-4])         # the placeholder is dropped, ids carried through
  expect_equal(out$position, c("Teacher", classify_k12_position("Mathematics"), classify_k12_position("Custodian"), "Teacher"))
  expect_true(all(is.na(out$Posting_Source)) && all(is.na(out$Feed_District)) && all(is.na(out$Also_On_Feed)) && all(is.na(out$Role_Hint)))
  expect_equal(out$Est_Area, c("Alpha County", "WY", "Alpha County", "WY"))
})

test_that("build_k12_combinedclean reads feed columns, role hints for feed rows only, and feed counties", {
  with_feed_label(); stub_oews()
  rows <- k12_rows()
  rows$Posting_ID <- paste0("id", 1:5)
  rows$Posting_Identity_Method <- "Stable per-posting URL"
  rows$Posting_Source <- c("AppliTrack (direct district board)", "Statewide feed", "AppliTrack (direct district board)",
                           "AppliTrack (direct district board)", "Statewide feed")
  rows$OPI_District <- c(NA, "Beta Elem", NA, NA, NA)
  rows$Also_On_OPI <- c(TRUE, FALSE, FALSE, FALSE, FALSE)
  rows$OPI_Description <- c("Long-term substitute for a teacher", "Long-term substitute for the spring semester", NA, NA, "")
  out <- build_k12_combinedclean(rows, oews_none(), data.frame(District = "Alpha Schools", County = "Alpha County"), "MT",
                                 columns = c(Feed_District = "OPI_District", Also_On_Feed = "Also_On_OPI", Feed_Description = "OPI_Description"),
                                 feed_counties = c("Beta Elem" = "Beta County"),
                                 canonicalize_district = function(x) sub("^BETA$", "Beta Schools", x))
  expect_equal(names(out), K12_POSTING_COLUMNS)
  expect_false(any(c("OPI_District", "Also_On_OPI", "OPI_Description", "Feed_Description") %in% names(out)))
  expect_equal(out$District, c("Alpha Schools", "Beta Schools", "Alpha Schools", "Gamma Schools"))
  expect_equal(out$Feed_District, c(NA, "Beta Elem", NA, NA))
  expect_equal(out$Also_On_Feed, c(TRUE, FALSE, FALSE, FALSE))
  # A direct row's description is never read for a hint; the feed row's is.
  expect_equal(out$Role_Hint, c(NA, k12_role_hint("Long-term substitute for the spring semester"), NA, NA))
  expect_equal(out$position[2], classify_k12_position("Mathematics", out$Role_Hint[2]))
  expect_equal(out$Est_Area, c("Alpha County", "Beta County", "Alpha County", "MT"))
})

test_that("build_k12_combinedclean refuses rows without an id or with a column under both names", {
  with_feed_label(); stub_oews()
  expect_error(build_k12_combinedclean(k12_rows(), oews_none(), data.frame(District = "a", County = "b"), "WY"), "posting id")
  rows <- k12_rows(); rows$posting_id <- "a"; rows$Posting_ID <- "b"
  expect_error(build_k12_combinedclean(rows, oews_none(), data.frame(District = "a", County = "b"), "WY",
                                       columns = c(Posting_ID = "posting_id")), "both")
})

test_that("build_k12_teacher_rows and summarize_k12_posting_counts count distinct ids per district", {
  with_feed_label(); stub_oews()
  rows <- rbind(k12_rows(), k12_rows()[1, ])        # the same display text twice, two ids
  rows$Posting_ID <- paste0("id", 1:6)
  cc <- build_k12_combinedclean(rows, oews_none(), data.frame(District = "Alpha Schools", County = "Alpha County"), "MT")
  teach <- build_k12_teacher_rows(cc)
  expect_equal(names(teach), K12_TEACHER_COLUMNS)
  expect_true(all(teach$position == "Teacher"))
  counts <- summarize_k12_posting_counts(teach)
  expect_equal(sum(counts$sum[counts$District == "Alpha Schools"]), 2)
  expect_error(summarize_k12_posting_counts(teach[, setdiff(names(teach), "Posting_ID")]), "Posting_ID")
  names(teach)[names(teach) == "Posting_ID"] <- "posting_id"
  expect_equal(summarize_k12_posting_counts(teach, id_col = "posting_id")$sum, counts$sum)
})

# Phase 4 review fixes (2026-11-13).
test_that("append_weekly_rows keeps an existing row whose date is NA instead of turning it into an all-NA row", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  write.csv(data.frame(Archive_Date = c("2026-11-06", NA), n = c(1, 2)), tmp, row.names = FALSE)
  out <- append_weekly_rows(tmp, data.frame(Archive_Date = "2026-11-13", n = 3))
  expect_equal(nrow(out), 3)
  expect_equal(out$n, c(1, 2, 3))
  expect_false(any(rowSums(is.na(out)) == ncol(out)))
})

test_that("build_k12_combinedclean says which mapped columns are missing, and warns with no feed label at all", {
  stub_oews()
  rows <- k12_rows(); rows$Posting_ID <- paste0("id", seq_len(nrow(rows)))
  with_feed_label()
  expect_message(build_k12_combinedclean(rows, oews_none(), data.frame(District = "Alpha Schools", County = "Alpha County"),
                                         "MT", columns = c(Also_On_Feed = "Also_On_OPI_typo")),
                 "no Also_On_OPI_typo column \\(mapped to Also_On_Feed\\)")
  old <- current_state_config(); set_state_config(NULL); withr::defer(set_state_config(old))
  expect_warning(build_k12_combinedclean(rows, oews_none(), data.frame(District = "Alpha Schools", County = "Alpha County"), "MT"),
                 "no state_config")
})
