# "New This Week" over every role (full review 2026-11-27): the app compares the last two weekly updates kept in
# k12_recent_postings.csv / he_recent_postings.csv, written by write_k12_history() / write_he_history().

test_that("write_recent_postings keeps each week's latest update, the last two weeks, and replaces a same-day re-run", {
  path <- withr::local_tempfile(fileext = ".csv")
  run <- function(date, ids) data.frame(Archive_Date = date, Posting_ID = ids, title = paste("Job", ids),
                                        location = "Town", District = "D", position = "Paraprofessional", extra = 1)
  write_recent_postings(path, run("2026-11-16", c("a", "b")), K12_RECENT_POSTING_COLUMNS)
  write_recent_postings(path, run("2026-11-19", c("a", "b", "c")), K12_RECENT_POSTING_COLUMNS)  # same ISO week
  write_recent_postings(path, run("2026-11-23", c("b", "d")), K12_RECENT_POSTING_COLUMNS)
  out <- write_recent_postings(path, run("2026-11-23", c("b", "d", "e")), K12_RECENT_POSTING_COLUMNS)  # re-run
  expect_named(out, K12_RECENT_POSTING_COLUMNS)
  expect_equal(sort(unique(out$Archive_Date)), c("2026-11-19", "2026-11-23"))
  expect_equal(sort(out$Posting_ID[out$Archive_Date == "2026-11-23"]), c("b", "d", "e"))
  # Rule A on every role: d and e are new; c is gone.
  expect_equal(sort(find_k12_new_postings(read.csv(path))$Posting_ID), c("d", "e"))
  out <- write_recent_postings(path, run("2026-11-30", "f"), K12_RECENT_POSTING_COLUMNS)
  expect_equal(sort(unique(out$Archive_Date)), c("2026-11-23", "2026-11-30"))
})

test_that("find_he_new_postings compares title, location and institution with the previous update", {
  h <- data.frame(Archive_Date = c("2026-11-17", "2026-11-17", "2026-11-24", "2026-11-24", "2026-11-24"),
                  Title = c("Custodian", "Professor", "Custodian", "Professor", "Professor"),
                  Location = c("Laramie", "Laramie", "Laramie", "Laramie", "Casper"),
                  Institution = "University of Wyoming")
  new <- find_he_new_postings(h)
  expect_equal(new$Location, "Casper")
  expect_equal(nrow(find_he_new_postings(h[1:2, ])), 0)
})
