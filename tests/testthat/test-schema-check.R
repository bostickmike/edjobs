# Shared tests moved from the MT and WY repos' test-schema-check.R in phase 2 (2026-11-11). A block both
# states had identically appears once; a same-named block with different contents appears
# twice, the second marked with its state.

test_that("check_file_schema returns NULL when every required column is present", {
  expect_null(check_file_schema("allsum.csv", c("Broad_Category", "Archive_Date", "District", "sum"),
                                 c("Broad_Category", "Archive_Date", "District", "sum")))
})

test_that("check_file_schema returns NULL when the file has EXTRA columns too", {
  expect_null(check_file_schema("allsum.csv", c("Broad_Category", "Archive_Date", "District", "sum", "X"),
                                 c("Broad_Category", "Archive_Date", "District", "sum")))
})

test_that("check_file_schema flags a single missing column", {
  result <- check_file_schema("salarymap2.csv", c("District", "County", "Latitude"),
                               c("District", "County", "Latitude", "Teachers_Total_FTE"))
  expect_equal(result$file, "salarymap2.csv")
  expect_equal(result$missing_column, "Teachers_Total_FTE")
})

test_that("check_file_schema flags every missing column, one row each", {
  result <- check_file_schema("salarymap2.csv", c("District"),
                               c("District", "County", "Latitude"))
  expect_equal(nrow(result), 2)
  expect_setequal(result$missing_column, c("County", "Latitude"))
})

test_that("check_file_schema flags a single missing column (Mo)", {
  result <- check_file_schema("combinedclean.csv", c("title", "date_posted", "position"),
                               c("title", "date_posted", "position", "District"))
  expect_equal(result$file, "combinedclean.csv")
  expect_equal(result$missing_column, "District")
})

test_that("check_file_schema flags every missing column, one row each (Mo)", {
  result <- check_file_schema("combinedclean.csv", c("title"),
                               c("title", "date_posted", "position"))
  expect_equal(nrow(result), 2)
  expect_setequal(result$missing_column, c("date_posted", "position"))
})
