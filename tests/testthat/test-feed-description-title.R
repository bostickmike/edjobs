# feed_description_title(): an OPI Jobs for Teachers title that names no job ("Other",
# "Administrative - Other") gives way to the job named in the description. Descriptions are the
# opening of real OPI postings (MT archives 2026-11-03 to 2026-11-22; drift triage 2026-11-22).

test_that("a labeled title in the description is used", {
  st_labre <- paste("High School VoTech/VoAg Teacher Ashland, Montana, United States | Full-time Apply by: No close",
                    "date St. Labre Indian Catholic School Job Description Job Title: High School VoTech/VoAg Teacher",
                    "Department: St. Labre Indian Catholic School Reports To: High School Principal")
  ncts <- paste("NORTHERN CHEYENNE TRIBAL SCHOOL JOB ADVERTISEMENT POSITION: CULTURE TEACHER SALARY: CERTIFIED",
                "SALARY SCALE DEPARTMENT: NCTS ACCOUNTABLE TO: PRINCIPAL")
  expect_equal(feed_description_title(c("Administrative - Other", "Other"), c(st_labre, ncts)),
               c("High School VoTech/VoAg Teacher", "CULTURE TEACHER"))
  expect_equal(classify_k12_subject(feed_description_title("Administrative - Other", st_labre)), "Agriculture Education")
})

test_that("the role after 'looking for' / 'seeking' is used, without leading adjectives", {
  d <- c(
    "The Wyola School District is looking for a Head and Assistant Cook for K-8th grade students.",
    paste("Jordan Public Schools is seeking a dependable, motivated, and skilled Head of Maintenance to join our",
          "team. This position plays an essential role in keeping our school facilities safe"),
    paste("Stevensville Public Schools is accepting applications for a full-time, year-round District Office",
          "Assistant Clerk and Payroll position. This skilled person will support the district"),
    paste("Description We are seeking a dedicated and compassionate Teacher to join our multidisciplinary team,",
          "providing individualized education to hospitalized students."))
  out <- feed_description_title(rep("Other", 4), d)
  expect_equal(out, c("Head and Assistant Cook", "Head of Maintenance", "District Office Assistant Clerk and Payroll",
                      "Teacher"))
  expect_equal(classify_k12_position(out), c("Food Services", "Custodial/Maintenance", "Staff", "Teacher"))
})

test_that("a name that classifies as Other is not used, and the feed's title stays", {
  rocky_boy <- paste("The Rocky Boy School District is recruiting a professional to serve as Project Coordinator/Pre-School",
                     "Teacher for a one-year federally funding grant.")
  expect_equal(feed_description_title("Administrative - Other", rocky_boy), "Administrative - Other")
  st_marys <- "This part-time, hourly position is approximately 16 hours per week. Responsibilities include planning and teaching Spanish lessons"
  expect_equal(feed_description_title("Other", st_marys), "Other")
})

test_that("only generic titles change, and missing descriptions are left alone", {
  westby <- "Westby School District is seeking a full- or half-time K-12 Music Teacher for the 2026-27 school year."
  expect_equal(feed_description_title(c("Music", "Other", NA), c(westby, NA, "seeking a Cook.")), c("Music", "Other", NA))
  expect_equal(feed_description_title(character(0), character(0)), character(0))
})

test_that("held-out flaws stay fixed: 'Report To' and 'full- or half-time' end or skip cleanly", {
  # Loyola-Sacred Heart (OPI "English"): "Report To:" ended nothing, so the name ran on to the principal.
  loyola <- "Job Title: Part-Time Catholic School Teacher Report To: Principal Status: Part-Time"
  expect_equal(feed_description_title("Other", loyola), "Part-Time Catholic School Teacher")
  westby <- "Westby School District is seeking a full- or half-time K-12 Music Teacher for the 2026-27 school year."
  expect_equal(feed_description_title("Other", westby), "K-12 Music Teacher")
})

test_that("Family Consumer Sciences without 'and' is its subject", {
  expect_equal(classify_k12_subject(c("Family Consumer Sciences teacher", "Family & Consumer Science Teacher")),
               rep("Family and Consumer Science", 2))
})
