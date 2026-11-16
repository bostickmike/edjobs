test_that("nces_district_url() builds the CCD district page from a 7-digit id", {
  expect_identical(nces_district_url(c("5600730", 3000001, NA, "", "560073", "56007301", "5600730.0")),
                   c("https://nces.ed.gov/ccd/districtsearch/district_detail.asp?ID2=5600730",
                     "https://nces.ed.gov/ccd/districtsearch/district_detail.asp?ID2=3000001",
                     NA, NA, NA, NA, "https://nces.ed.gov/ccd/districtsearch/district_detail.asp?ID2=5600730"))
})

test_that("college_navigator_url() builds the College Navigator page from a 6-digit id", {
  expect_identical(college_navigator_url(c(240727, "180461", NA, "24072", "x40727")),
                   c("https://nces.ed.gov/collegenavigator/?id=240727", "https://nces.ed.gov/collegenavigator/?id=180461",
                     NA, NA, NA))
})

test_that("add_source_record_links() puts each link after its id and is the same on a second run", {
  d <- data.frame(District = c("A", "B"), NCES_LEAID = c(5600730, NA), County = c("x", "y"))
  out <- add_source_record_links(d)
  expect_identical(names(out), c("District", "NCES_LEAID", "CCD_Link", "County"))
  expect_identical(out$CCD_Link, c("https://nces.ed.gov/ccd/districtsearch/district_detail.asp?ID2=5600730", NA))
  expect_identical(add_source_record_links(out), out)
  # A stale link is replaced from the id.
  stale <- out; stale$CCD_Link <- "https://old.invalid"
  expect_identical(add_source_record_links(stale), out)

  h <- data.frame(Name = "UW", IPEDS_UNITID = 240727, Link = "https://uwyo.edu/jobs")
  outh <- add_source_record_links(h)
  expect_identical(names(outh), c("Name", "IPEDS_UNITID", "IPEDS_Link", "Link"))
  expect_identical(outh$IPEDS_Link, "https://nces.ed.gov/collegenavigator/?id=240727")

  plain <- data.frame(Title = "x")
  expect_identical(add_source_record_links(plain), plain)
})
