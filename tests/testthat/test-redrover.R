# Red Rover's GraphQL API (moved from WY in 0.14.0; MT on it since then).

test_that("parse_redrover_json extracts fields and builds a per-job link from the GraphQL response shape", {
  fixture <- '{"data":{"jobSeekerSiteUnauthenticated":{"jobPostingSearch":{"results":[
    {"id":"190114","name":"Part-time Custodian","organizationName":"Johnson County School District #1","location":{"name":"Kaycee K-12 School"},"activePublicOnDateUtc":"2026-07-27T15:02:24.7935011Z"},
    {"id":"191643","name":"Assistant Girls Basketball Coach","organizationName":"Johnson County School District #1","location":{"name":"Buffalo High School"},"activePublicOnDateUtc":"2026-07-27T15:01:36.1969706Z"}
  ]}}}}'
  result <- parse_redrover_json(fixture, "jcsd1")
  expect_equal(nrow(result), 2)
  expect_equal(result$Title, c("Part-time Custodian", "Assistant Girls Basketball Coach"))
  expect_equal(result$Location, c("Kaycee K-12 School", "Buffalo High School"))
  expect_equal(result$Posted_Date, c("2026-07-27", "2026-07-27"))
  expect_equal(result$Posting_ID, c("redrover:jcsd1:190114", "redrover:jcsd1:191643"))
  expect_equal(result$Link, c("https://jobs.redroverk12.com/org/jcsd1/opening/190114",
                              "https://jobs.redroverk12.com/org/jcsd1/opening/191643"))
  expect_equal(k12_link_posting_id(result$Link), result$Posting_ID)
})

test_that("parse_redrover_json returns zero rows (not an error) for an empty results list", {
  result <- parse_redrover_json('{"data":{"jobSeekerSiteUnauthenticated":{"jobPostingSearch":{"results":[]}}}}', "jcsd1")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("Title", "Location", "Posted_Date", "Posting_ID", "Link",
                                "Pay_Min", "Pay_Max", "Pay_Unit", "Pay_Text"))
})

test_that("standing application forms are dropped (St. Ignatius, MT, real response 2026-11-13)", {
  json <- paste(readLines(test_path("fixtures", "redrover_graphql_stignatius_2261_2026-11-13.json"), warn = FALSE), collapse = "\n")
  raw <- jsonlite::fromJSON(json)$data$jobSeekerSiteUnauthenticated$jobPostingSearch$results$name
  expect_true(all(c("Classified Application", "Volunteer Application") %in% raw))
  result <- parse_redrover_json(json, "2261")
  expect_equal(nrow(result), length(raw) - 2)
  expect_false(any(result$Title %in% REDROVER_PLACEHOLDER_TITLES))
  expect_true(all(grepl("^https://jobs\\.redroverk12\\.com/org/2261/opening/\\d+$", result$Link)))
  # An all-placeholder board is an empty one, with the full column set.
  only <- '{"data":{"jobSeekerSiteUnauthenticated":{"jobPostingSearch":{"results":[{"id":"1","name":"Volunteer Application","location":{"name":"x"},"activePublicOnDateUtc":"2026-07-01T00:00:00Z"}]}}}}'
  expect_equal(nrow(parse_redrover_json(only, "x")), 0)
})

test_that("redrover_org_slug reads the slug from a board URL", {
  expect_equal(redrover_org_slug(c("https://jobs.redroverk12.com/org/jcsd1", "https://jobs.redroverk12.com/org/FCSD25",
                                   "https://jobs.redroverk12.com/org/2261?x=1")), c("jcsd1", "FCSD25", "2261"))
})
