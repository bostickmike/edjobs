# classify_work_schedule(): titles are real ones from the MT and WY archives (2026-11-18 replay,
# Jobs_Dash_repos claude-notes/2026-11-18_not-full-time/).

k12 <- function(title, role_hint = NULL) {
  classify_work_schedule(title, k12_position = classify_k12_position(title, role_hint), role_hint = role_hint)
}
college <- function(title) classify_work_schedule(title, he_job_type = classify_he_job_type(title))

test_that("K-12 substitutes and extra-duty jobs come from the position", {
  expect_equal(k12(c("Substitute Teacher", "Assistant Middle School Track Coach -Rocky", "Speech Coach")),
               c("substitute", "extra duty", "extra duty"))
  expect_equal(k12(c("STIPEND - INST. LEADER (DEPT. HEAD) HS SPED", "Kinder Boost Teacher Stipend",
                     "Extra-Curricular Positions for 2026-2027 School Year")), rep("extra duty", 3))
})

test_that("titles saying part-time, a fraction of an FTE or a short schedule are part-time", {
  t <- c("Part-Time Crosswalk Guard", "Food Service Worker - Cook (Part -Time)", "Half-Time K-12 Librarian",
         "(3) Head Start PT Bus Drivers", "Art Teacher - Trail Elementary - 1/2 Time",
         "School Psychologist .5 FTE (beginning 26/27)", "Speech and Language Pathologist .5 FTE DW2627_79",
         "Foreign Language Teacher (.33)", ".5 Paraprofessional Position", "0.25 Exceptional Child Specialist/Resource Room",
         "Assistant Cook 20 hrs per week/Burlington", "Food Service- 2 hours per day- Afton Elementary",
         "Bus Driver - Route 4 - 4hrs/day", "Noon Duty - Lewis and Clark Elementary - 2 Days/Week",
         "High School Teacher- 4 Days a Week", "On-Call Security - Missoula County Public Schools")
  expect_equal(k12(t), rep("part-time", length(t)))
  # OPI's part-time role hint, with a subject-only title.
  expect_equal(k12("Music", role_hint = "Part-Time"), "part-time")
})

test_that("full-time stated, whole schedules, split posts and prices are not marked", {
  t <- c("Night Custodian- Full-time or Part Time", "Full-time and Part-time Classroom Aides and SPED Paraprofessionals",
         "HS Math Teacher (1 FTE)", "Paraprofessional General Education SC2627_58  1 FTE",
         "Secretary – $14 per hour, up to 32 hours per week", "Food Service Associate 6 hours/day",
         "Assistant Cook-Starts at $16.50/hr", "0.6 Custodian/0.4 Assistant Cook",
         "Primary .5 Sped Paraprofessional/.5 Academic Coach", "Grades 1/2 Teacher", "Elementary Teacher",
         "Physical Therapist", "Lead Teacher - 21st Century After School and Summer Programs")
  expect_equal(k12(t), rep(NA_character_, length(t)))
})

test_that("pools are not a reason: the title says nothing about the schedule", {
  expect_equal(k12(c("2026-2027 PARAPROFESSIONAL POOL", "Pool Lifeguard", "Full-Time Custodian/Certified Pool Operator")),
               rep(NA_character_, 3))
  expect_equal(college("Custodian I - Full Time (Applicant Pool)"), NA_character_)
})

test_that("student jobs and temporary or seasonal jobs", {
  expect_equal(k12(c("Student Sweeper [ST05]", "Lifeguard - HS Student", "STUDENT WORKER")), rep("student job", 3))
  expect_equal(k12(c("Para for one student", "SCHOOL PSYCHOLOGIST INTERN")), rep(NA_character_, 2))
  expect_equal(k12(c("Seasonal Groundskeeper", "Temporary Custodian (GPS)", "Summer School Teachers",
                     "Full-Time Summer Custodian - Hulett", "SE - SLP TEMP")), rep("temporary or seasonal", 5))
})

test_that("college postings: adjuncts, part-time staff, students, temporary, substitutes", {
  expect_equal(college(c("Adjunct Faculty - Mathematics", "Cook I - Part Time", "Major Gift Officer – Part-time",
                         "Dentist (On-Call), Curry Health Center", "(Student Position) Peer Tutor",
                         "Work Study Posn Student - ATS Student Technician",
                         "Pooled Position Limited Tempor - Data Assistant", "Temp, Sign Language Interpreter",
                         "Children's Center Part-Time Aide (Substitute)")),
               c("part-time", "part-time", "part-time", "part-time", "student job", "student job",
                 "temporary or seasonal", "temporary or seasonal", "substitute"))
  expect_equal(college(c("Assistant Professor of Chemistry", "Director, TRIO Student Support Services",
                         "Student Accounts", "Head Football Coach")), rep(NA_character_, 4))
})

test_that("reasons are the documented set, and missing titles are not marked", {
  r <- k12(c("Substitute Teacher", "Speech Coach", "Part-Time Cook", "Student Custodian", "Summer Grounds", "Teacher"))
  expect_true(all(r[!is.na(r)] %in% WORK_SCHEDULE_REASONS))
  expect_equal(classify_work_schedule(c(NA, ""), he_job_type = c(NA, NA)), c(NA_character_, NA_character_))
})

test_that("jobtype_run_totals() counts every posting and the unmarked ones, per type and run", {
  d <- data.frame(
    title = c("Elementary Teacher", "Part-Time Business Teacher", "Substitute Teacher", "Head Football Coach",
              "Elementary Teacher", "Para", "Para"),
    position = c("Teacher", "Part-Time Teacher", "Substitute", "Athletics", "Teacher", "Paraprofessional", "Paraprofessional"),
    Archive_Date = as.Date(c(rep("2026-11-10", 4), rep("2026-11-17", 3))),
    Posting_ID = c("a", "b", "c", "d", "a", "p", "p"), stringsAsFactors = FALSE)
  rows <- jobtype_run_totals(d, "position", "title", "k12")
  expect_equal(names(rows), c("Job_Type", "Archive_Date", "n", "n_not_marked"))
  expect_equal(nrow(rows), 6)  # no zero rows for a type absent from a run
  sub <- rows[rows$Job_Type == "Substitute", ]
  expect_equal(c(sub$n, sub$n_not_marked), c(1, 0))
  # Rows (WY) vs distinct Posting_ID (MT): the duplicated "Para" row.
  para <- function(r) r[r$Job_Type == "Paraprofessional", c("n", "n_not_marked")]
  expect_equal(unlist(para(rows)), c(n = 2, n_not_marked = 2))
  expect_equal(unlist(para(jobtype_run_totals(d, "position", "title", "k12", id_col = "Posting_ID"))),
               c(n = 1, n_not_marked = 1))
  he <- data.frame(Title = c("Adjunct Faculty - Biology", "Assistant Professor of Biology"),
                   Archive_Date = as.Date("2026-11-17"), stringsAsFactors = FALSE)
  he$Job_Type <- classify_he_job_type(he$Title)
  h <- jobtype_run_totals(he, "Job_Type", "Title", "college")
  expect_equal(h$n_not_marked[match(c("Adjunct/Part-Time Faculty", "Instructor/Teacher/Faculty"), h$Job_Type)], c(0, 1))
})
