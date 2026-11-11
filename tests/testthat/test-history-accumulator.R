# Shared tests moved from the MT and WY repos' test-history-accumulator.R in phase 2 (2026-11-11). A block both
# states had identically appears once; a same-named block with different contents appears
# twice, the second marked with its state.


test_that("check_total_matches_parts passes when the Total genuinely equals the sum of parts", {
  total <- data.frame(Broad_Category = "Math", Archive_Date = "2026-08-06", District = "Total", sum = 5)
  parts <- data.frame(Broad_Category = "Math", Archive_Date = "2026-08-06",
                       District = c("A", "B"), sum = c(2, 3))
  expect_true(check_total_matches_parts(total, parts, group_cols = c("Broad_Category", "Archive_Date")))
})

test_that("check_total_matches_parts stops when the Total doesn't equal the sum of parts", {
  total <- data.frame(Broad_Category = "Math", Archive_Date = "2026-08-06", District = "Total", sum = 999)
  parts <- data.frame(Broad_Category = "Math", Archive_Date = "2026-08-06",
                       District = c("A", "B"), sum = c(2, 3))
  expect_error(check_total_matches_parts(total, parts, group_cols = c("Broad_Category", "Archive_Date")),
               "don't equal the sum")
})

test_that("rebuild_parity_row ignores row order, NA vs blank and \\r\\n, and counts real differences", {
  committed <- data.frame(a = c("x", "y", "y"), b = c("1", "", "2\n3"), c = "k", stringsAsFactors = FALSE)
  same <- committed[c(3, 1, 2), c("a", "b", "c")]
  same$b[same$b == ""] <- NA
  same$b <- sub("\n", "\r\n", same$b, fixed = TRUE)
  r <- rebuild_parity_row("f.csv", committed, same)
  expect_equal(c(r$Only_Committed, r$Only_Rebuilt), c(0, 0))
  expect_equal(r$Missing_Cols, "")
  # A dropped column and a duplicated / missing row both show.
  r2 <- rebuild_parity_row("f.csv", committed, rbind(committed[c(1, 1, 3), c("a", "b")]))
  expect_equal(r2$Missing_Cols, "c")
  expect_equal(c(r2$Only_Committed, r2$Only_Rebuilt), c(1, 1))
  expect_match(r2$Example, "committed: y \\| <NA>")
})

test_that("rebuild_parity_row ignores row order and NA vs blank, and counts real differences", {
  committed <- data.frame(a = c("x", "y", "y"), b = c("1", "", "2"), c = "k", stringsAsFactors = FALSE)
  same <- committed[c(3, 1, 2), c("a", "b", "c")]
  same$b[same$b == ""] <- NA
  r <- rebuild_parity_row("f.csv", committed, same)
  expect_equal(c(r$Only_Committed, r$Only_Rebuilt), c(0, 0))
  expect_equal(r$Missing_Cols, "")
  # A dropped column and a duplicated / missing row both show.
  r2 <- rebuild_parity_row("f.csv", committed, rbind(committed[c(1, 1, 3), c("a", "b")]))
  expect_equal(r2$Missing_Cols, "c")
  expect_equal(c(r2$Only_Committed, r2$Only_Rebuilt), c(1, 1))
  expect_match(r2$Example, "committed: y \\| <NA>")
})
