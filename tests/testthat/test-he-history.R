he_hist_rows <- function(dates = "2026-11-06") {
  one <- function(d) data.frame(Title = c("Assistant Professor of Biology", "Adjunct Instructor, Mathematics", "Accountant", "Professor of History"),
                                Location = "L", Posted_Date = "2026-11-01", Institution = c("A", "A", "A", "B"), Link = "u",
                                Archive_Date = d, Pay_Min = NA_real_, Close_Date = NA_character_, stringsAsFactors = FALSE)
  do.call(rbind, lapply(dates, one))
}

test_that("combine_he_frames adds the missing columns, keeps Page_Places and mixes date types", {
  a <- data.frame(Title = "t1", Location = "L", Posted_Date = as.Date("2026-11-01"), Institution = "A", Link = "u", Page_Places = "P")
  b <- data.frame(Title = "t2", Location = "L", Posted_Date = "11/02/2026", Institution = "B", Link = "v", Close_Date = as.Date("2026-12-01"), Pay_Min = 5)
  empty <- a[0, ]
  out <- combine_he_frames(list(x = a, y = b, z = empty), as.Date("2026-11-24"))
  expect_equal(nrow(out), 2)
  expect_named(out, c("Title", "Location", "Posted_Date", "Institution", "Link", "Close_Date", POSTED_PAY_COLUMNS, "Page_Places", "Archive_Date"))
  expect_equal(out$Posted_Date, c("2026-11-01", "11/02/2026"))
  expect_equal(out$Close_Date, c(NA, "2026-12-01"))
  expect_equal(unique(out$Archive_Date), as.Date("2026-11-24"))
  expect_equal(nrow(combine_he_frames(a)), 1)
})

test_that("finish_he_rows drops placeholders, and skips the estimates without a staff file", {
  app <- withr::local_tempdir()
  d <- combine_he_frames(data.frame(Title = c("No Jobs", "Accountant"), Location = "L", Posted_Date = "x", Institution = "A", Link = "u"))
  out <- finish_he_rows(d, app)
  expect_equal(out$Title, "Accountant")
  expect_false("Est_Pay" %in% names(out))
  expect_error(finish_he_rows(d, NULL))
})

test_that("build_he_history's totals are the sum of the institutions, and allnow_he is the latest run's", {
  h <- build_he_history(he_hist_rows(c("2026-11-06", "2026-11-10")))
  expect_named(h, c("facultydata", "he_institution_weekly_totals", "he_jobtype_weekly_totals", "allsum_he", "allnow_he"))
  expect_named(h$facultydata, HE_FACULTY_COLUMNS)
  expect_false("Accountant" %in% h$facultydata$Title)
  expect_equal(nrow(h$facultydata), 6)
  tot <- h$allsum_he[h$allsum_he$Institution == "Total" & h$allsum_he$Archive_Date == "2026-11-10", ]
  expect_equal(sum(tot$sum), 3)
  expect_equal(sum(h$allnow_he$Sum[h$allnow_he$Institution == "Total"]), 3)
  # Every posting counts in the institution totals, not only faculty.
  t1 <- h$he_institution_weekly_totals
  expect_equal(t1$n[t1$Institution == "A" & t1$Archive_Date == "2026-11-10"], 3)
})

test_that("write_he_history appends a run, replaces a same-day re-run, and writes nothing when one file doesn't match", {
  app <- withr::local_tempdir()
  write_he_history(he_hist_rows("2026-11-06"), app)
  write_he_history(he_hist_rows("2026-11-10"), app)
  write_he_history(he_hist_rows("2026-11-10"), app)
  expect_equal(nrow(read.csv(file.path(app, "facultydata.csv"))), 6)
  expect_named(read.csv(file.path(app, "allnow_he.csv")), c("Category", "Job_Type", "Sum", "Institution"))
  bad <- read.csv(file.path(app, "allsum_he.csv")); bad$extra <- 1
  write.csv(bad, file.path(app, "allsum_he.csv"), row.names = FALSE)
  before <- tools::md5sum(list.files(app, full.names = TRUE))
  expect_error(write_he_history(he_hist_rows("2026-11-13"), app), "column mismatch")
  expect_equal(tools::md5sum(list.files(app, full.names = TRUE)), before)
})
