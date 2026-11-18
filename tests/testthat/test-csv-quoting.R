test_that("write_csv_keep_quoting() reproduces a write.csv() file byte for byte", {
  df <- data.frame(Title = c("Teacher, Grade 3", 'Aide "B"', "Line\nbreak", NA, ""),
                   Archive_Date = as.Date("2026-11-17") + 0:4, n = c(1L, 2L, NA, 4L, 5L),
                   Pay = c(16.5, NA, 100, 42000, 0.25), Flag = c(TRUE, FALSE, NA, TRUE, FALSE),
                   stringsAsFactors = FALSE)
  orig <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(df, orig, row.names = FALSE)
  expect_equal(unname(csv_quoted_columns(orig)), c(TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(names(csv_quoted_columns(orig)), names(df))
  back <- utils::read.csv(orig, colClasses = "character", check.names = FALSE)
  out <- withr::local_tempfile(fileext = ".csv")
  write_csv_keep_quoting(back, out, like = orig)
  expect_identical(readLines(out), readLines(orig))
  # Plain write.csv() of the same data quotes the dates and numbers too (what the reclassify script did).
  plain <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(back, plain, row.names = FALSE)
  expect_false(identical(readLines(plain), readLines(orig)))
})

test_that("write_csv_keep_quoting() quotes a bare column once it holds a comma", {
  orig <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(data.frame(a = 1:2, b = c("x", "y")), orig, row.names = FALSE)
  out <- withr::local_tempfile(fileext = ".csv")
  write_csv_keep_quoting(data.frame(a = c("1", "2,5"), b = c("x", "y")), out, like = orig)
  expect_equal(utils::read.csv(out, colClasses = "character")$a, c("1", "2,5"))
})
