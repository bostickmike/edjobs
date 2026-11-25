# Tests for posted-pay (edjobs).
# Moved from the MT and WY repos in phase 2 (2026-11-11); blocks identical in both states appear once,
# and a same-named block whose examples differ appears twice, the second marked with its state.

# extract_posted_pay(): the pay a posting itself states. Unit tests use
# wording copied from real AppliTrack postings (Great Falls, Billings, MCPS,
# Helena, Sweetwater); the parser test uses a real captured Output.asp page.

test_that("extract_posted_pay reads amounts, ranges and units as written", {
  p <- extract_posted_pay(c(
    "LOCATION: Great Falls High School SALARY: $15.14 per hour (25/26 Salary Schedule)",
    "EMPLOYMENT: 2026/2027 SALARY: $48,300-$73,957 (2026/2027 salary schedule)",
    "7.5 hrs/day$21.46 to $22.81 per hour, depending on experience",
    "Starts: August 2026Stipend: $3,548.00Required Coaches Certificates",
    "Long Term Substitutes are paid at $250.00 per day and issued a contract.",
    "Salary: $160.00/Whole Day, $80.00/Half Day",
    "6:00am - 2:30pm $20.60* - $21.95*/hr depending on experience",
    "Part Time, at most 29 hours per weekSalary: P-V, $25.69",
    "BEA Salary Schedule; Full Benefits",
    NA
  ))
  expect_equal(p$Pay_Min, c(15.14, 48300, 21.46, 3548, 250, 160, 20.60, 25.69, NA, NA))
  expect_equal(p$Pay_Max, c(15.14, 73957, 22.81, 3548, 250, 160, 21.95, 25.69, NA, NA))
  expect_equal(p$Pay_Unit, c("hour", "year", "hour", "stipend", "day", "day", "hour", "hour", NA, NA))
  expect_true(all(is.na(p$Pay_Text) == is.na(p$Pay_Min)))
})

test_that("extract_posted_pay splits an amount run into the next number", {
  # Carbon County SD 1 (WY), 2026-11-13 and 11-20: the board drops the space before "12 month work year".
  p <- extract_posted_pay(c(
    "Hide Reports To: Director Full Time: Hourly Rate: $18.85-$23.8512 month work year with salary and fr",
    "Classified hourly rates: $17.820 to $22.00 hourly"
  ))
  expect_equal(p$Pay_Min, c(18.85, 17.82))
  expect_equal(p$Pay_Max, c(23.85, 22))
  expect_equal(p$Pay_Unit, c("hour", "hour"))
})

test_that("extract_posted_pay keeps a lone amount when a $-less number after it is smaller", {
  # Missoula County Public Schools (MT), 2026-11-17 and 11-19: "2/hour minimum" is a two-hour minimum shift, not $2.
  p <- extract_posted_pay(c(
    "tion attached at the bottom right of this posting*$24.00/hour - 2/hour minimum This position covers O",
    "Salary: $45,000 - 60,000 DOE",
    "$18.50 - $16.00 per hour"
  ))
  expect_equal(p$Pay_Min, c(24, 45000, 16))
  expect_equal(p$Pay_Max, c(24, 60000, 18.5))
  expect_equal(p$Pay_Unit, c("hour", "year", "hour"))
})

test_that("extract_posted_pay ignores bonuses, fees and differentials", {
  p <- extract_posted_pay(c(
    "*We offer a one-time $1,000 signing bonus. Starting salary will be a minimum of $46,225 to max of $86,400 DOE",
    "*Includes a $0.50 per hour differential. Pay: $20.60 per hour",
    "Application fee of $25 required."
  ))
  expect_equal(p$Pay_Min, c(46225, 20.60, NA))
  expect_equal(p$Pay_Max, c(86400, 20.60, NA))
})

test_that("extract_posted_pay skips a benefits package value and a $0.00 blank pay field", {
  # Real FVCC (Paycom) and LCCC (NEOGOV) wording, 2026-10-30.
  p <- extract_posted_pay(c(
    "Benefits eligible Unlock Exceptional Benefits at FVCC (Worth ~$25K) Our employees",
    "Adjunct - $0.00 Hourly",
    "Full-time, benefits eligible, $45,000 per year",
    "Salary: $52,000 plus benefits"
  ))
  expect_equal(p$Pay_Min, c(NA, NA, 45000, 52000))
  expect_equal(p$Pay_Unit, c(NA, NA, "year", "year"))
})

test_that("extract_posted_pay skips an amount in millions (a fund or grant), not a salary beside it", {
  # UM's Interfolio Flathead Lake Bio Station director, 2026-11-21 ("$1M" was read as $1).
  p <- extract_posted_pay(c(
    "the successful applicant may be provided access to proceeds from the $1M Jessie Bierman endowment",
    "a $2.5 million NSF award; Salary: $85,000- $98,000 (salary commensurate with experience)",
    "Research funding of $1.2 billion annually",
    "Salary: $45,000 MA degree preferred",
    "$18.50 per hour M-F schedule"
  ))
  expect_equal(p$Pay_Min, c(NA, 85000, NA, 45000, 18.5))
  expect_equal(p$Pay_Max, c(NA, 98000, NA, 45000, 18.5))
})

test_that("ensure_posted_pay_columns pads a zero-row frame", {
  out <- ensure_posted_pay_columns(data.frame(Title = character(0)))
  expect_equal(names(out), c("Title", "Pay_Min", "Pay_Max", "Pay_Unit", "Pay_Text"))
  expect_equal(nrow(out), 0)
  expect_type(out$Pay_Min, "double")
})

test_that("extract_posted_pay skips district housing costs", {
  # Real OPI wording (Richey), 2026-10-31.
  p <- extract_posted_pay(c(
    "Our district owns housing which includes 3 houses and 7 apartments starting at $350 per month! Utilities included.",
    "Teacherage available for $400/month. Salary: $42,000 per year",
    "Starting salary is $45,500; current staff average $52,000"
  ))
  expect_equal(p$Pay_Min, c(NA, 42000, 45500))
})

test_that("extract_posted_pay skips amounts that go toward something else, and benefit ceilings", {
  # Real OPI wording (Turner, Judith Gap, Polson), 2026-10-31.
  p <- extract_posted_pay(c(
    "first contract year at no cost and only $200 monthly for housing/utilities every year",
    "development opportunities, and $800/month toward health, dental, and vision",
    "The program provides for up to $3,000 of state funded loan repayment",
    "Salary: up to $60,000 DOE",
    "Salary: $52,000 per year for the 2026-27 school year"
  ))
  expect_equal(p$Pay_Min, c(NA, NA, NA, 60000, 52000))
})

test_that("extract_posted_pay skips application costs, money paid into benefits, and loan-assistance lists", {
  # Real wording from the 2026-10-31 rehearsal (Billings, Rocky Boy, Billings).
  p <- extract_posted_pay(c(
    "or email lynchla@billingsschools.org. The cost is $60.00) Go to www.billingsschools.org and apply",
    "Our district also pays $11,446 into a flex benefits package (district contribution)",
    "Quality Educator Loan Assistance of up to $3,000, $4,000 & $5,000 over three years.",
    "Loan assistance up to $3,000. Salary: $45,000 per year",
    "Teaching Assistant: $15.50 per hour"
  ))
  expect_equal(p$Pay_Min, c(NA, NA, NA, 45000, 15.5))
})

test_that("extract_posted_pay reads a unit run into the next label, and pay listed after a bonus", {
  # Real posting text whose line breaks the board dropped (2026-11-01
  # rehearsals: WY Converse #1 and Sweetwater #2 AppliTrack, MT Harlem).
  p <- extract_posted_pay(c(
    "LOCATION: Various locationsPAY RATE: $145/dayBENEFITS: None As a substitute,",
    "requirements are listed on the certificate.Salary: $146.09 per dayIf you have any questions",
    "Bus Driver/Custodian with CDL (Part-Time) $21.69/hrThe District is willing to help",
    # The bonus word belongs to the earlier amount, not to the salary.
    "Signing bonus $1,000, $42,000 annual salary",
    "$500 signing bonus, $3,000 relocation"
  ))
  expect_equal(p$Pay_Min, c(145, 146.09, 21.69, 42000, NA))
  expect_equal(p$Pay_Unit, c("day", "day", "hour", "year", NA))
})

test_that("is_implausible_pay flags amounts out of range for their unit", {
  bad <- is_implausible_pay(
    pay_min = c(15, 900, 55000, 145, 4771, 60, 50000, NA, 20, 15),
    pay_max = c(25, 900, 55000, 145, 4771, 60, 40000, NA, 20, 15),
    pay_unit = c("hour", "hour", "hour", "day", "stipend", NA, "year", NA, "fortnight", "hour")
  )
  expect_equal(bad, c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE))
})

test_that("drop_implausible_pay blanks every Pay_* column of a bad row only", {
  df <- data.frame(Pay_Min = c(18, 55000), Pay_Max = c(22, 55000), Pay_Unit = c("hour", "hour"),
                   Pay_Text = c("$18-$22/hr", "$55,000 hourly"), stringsAsFactors = FALSE)
  out <- suppressMessages(drop_implausible_pay(df))
  expect_equal(out$Pay_Min, c(18, NA))
  expect_true(all(is.na(unlist(out[2, POSTED_PAY_COLUMNS]))))
  expect_equal(out[1, ], df[1, ])
})

test_that("a bare $7.25-$60 amount beside a pay word is read as hourly", {
  # Real posting text, 2026-11-03/06 (Stevensville, Three Forks, Converse #1, Park #6).
  p <- extract_posted_pay(c("Monday through Friday, with a starting wage of $16.04 with paid time off",
                            "Activity Drivers: $21.35 starting pay District Paid CDL",
                            "Salary Schedule: Category C; step 1: $13.37",
                            "FLSA Status Approval Date Salary GradeNon Exempt $19.20 - $31.70 Email To A Friend",
                            # Not changed: a stipend, an amount with no pay word, an amount over $60.
                            "Head Coach stipend $45",
                            "Bring $20 for the background check",
                            "Starting salary is $75 for the day"))
  expect_equal(p$Pay_Unit[1:4], rep("hour", 4))
  expect_equal(p$Pay_Min[4], 19.2); expect_equal(p$Pay_Max[4], 31.7)
  expect_equal(p$Pay_Unit[5], "stipend")
  expect_true(is.na(p$Pay_Unit[6]))
  expect_false(p$Pay_Unit[7] %in% "hour")
})

# Moved in phase 3 (2026-11-11) with the platform scrapers.

test_that("a NEOGOV pay field with an implausible unit falls back to the card's description", {
  # Real cards (Gillette College, 2026-11-10): the field says "Adjunct - $900.00
  # Hourly", the description "Grade: $900 per credit hour".
  pay <- parse_neogov_card_pay(paste(readLines(test_path("fixtures", "neogov_gillettecollege_p1_2026-11-10.html"),
                                               warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
  adj <- pay[!is.na(pay$Pay_Min) & pay$Pay_Min == 900, ]
  expect_gte(nrow(adj), 2)
  expect_true(all(adj$Pay_Unit == "credit hour"))
  expect_true(all(grepl("per credit hour", adj$Pay_Text)))
  # A field with a plausible unit is still taken from the field.
  expect_false(any(is.na(pay$Pay_Unit[!is.na(pay$Pay_Min)])))
})


test_that("parse_applitrack_output carries each posting's stated pay (Mo)", {
  path <- test_path("fixtures", "applitrack_output_mcps_pay.bin")
  js <- iconv(readChar(path, file.size(path), useBytes = TRUE), "WINDOWS-1252", "UTF-8")
  out <- parse_applitrack_output(js)
  expect_true(all(c("Pay_Min", "Pay_Max", "Pay_Unit", "Pay_Text") %in% names(out)))
  expect_gt(nrow(out), 0)
  expect_gte(mean(!is.na(out$Pay_Min)), 0.9)
  ok <- !is.na(out$Pay_Min)
  expect_true(all(out$Pay_Min[ok] <= out$Pay_Max[ok]))
  expect_true(all(out$Pay_Unit[ok] %in% c("hour", "day", "month", "year", "credit hour", "stipend", NA)))
  expect_true(all(mapply(function(txt, v) grepl(format(v, big.mark = ",", nsmall = 0, scientific = FALSE), gsub("\\.00\\b", "", txt), fixed = TRUE) || grepl(sprintf("%.2f", v), txt, fixed = TRUE),
                         out$Pay_Text[ok], out$Pay_Min[ok])))
  expect_equal(ncol(parse_applitrack_output("")), ncol(out))
})

# --- Higher ed boards ---------------------------------------------------------

read_fixture <- function(name) paste(readLines(test_path("fixtures", name), warn = FALSE), collapse = "\n")

test_that("parse_neogov_card_pay reads each schooljobs card's pay line by job ID", {
  pay <- parse_neogov_card_pay(read_fixture("schooljobs_ummissoula_joblist_2026-10-30.html"))
  expect_equal(nrow(pay), 10)
  expect_equal(pay$Job_ID[1], "5533670")
  expect_equal(pay$Pay_Text[1], "Hourly Staff Full-time - $22.53 Hourly")
  expect_equal(pay$Pay_Unit[5], "year")
  expect_equal(c(pay$Pay_Min[5], pay$Pay_Max[5]), c(60000, 70000))
  # "Faculty Part-time - See Position Description": no figure stated.
  expect_true(is.na(pay$Pay_Min[8]))
  expect_equal(nrow(parse_neogov_card_pay("<html></html>")), 0)
})

test_that("benefit words belong to their own amount, and a pay word before an amount wins (2026-11-17 review, bug 3)", {
  # Real MT OPI descriptions (archives 2026-10/11): each lost its salary before.
  p <- extract_posted_pay(c("Est. Salary: $43,200-76,995 + $2,000 Signing Bonus + $1,000 Colony Bonus",
                            "Base Salary: $41,615 Full family insurance, including an",
                            "Starting salary $42,900 DOE Health Insurance Stipend",
                            "2026-2027 minimum salary is $42,863. The district contributes to health"))
  expect_equal(p$Pay_Min, c(43200, 41615, 42900, 42863))
  expect_equal(p$Pay_Max, c(76995, 41615, 42900, 42863))
  expect_equal(p$Pay_Unit, rep("year", 4))
  # The bonus itself is still never read as pay.
  expect_equal(extract_posted_pay("Signing bonus $1,000, $42,000 annual salary")$Pay_Min, 42000)
  expect_equal(extract_posted_pay("$45,000, up to $3,000 signing bonus")$Pay_Max, 45000)
})

test_that("K on the high end, a period as thousands separator, and hourly 'up to' ranges", {
  p <- extract_posted_pay(c("Salary $42-48K DOE", "Salary $45.000 per year", "Pay: $16.50 per hour, up to $19.00",
                            "$14 per hour, up to 32 hours per week", "$17.820 to $22.00 hourly"))
  expect_equal(p$Pay_Min, c(42000, 45000, 16.5, 14, 17.82))
  expect_equal(p$Pay_Max, c(48000, 45000, 19, 14, 22))
  expect_equal(p$Pay_Unit, c("year", "year", "hour", "hour", "hour"))
})

test_that("pay words before a semicolon still mark a bare amount as hourly; bonus words there still don't veto", {
  # Browning 2026-11-19 (AppliTrack JobID 2002), the only such case in either state's archives.
  p <- extract_posted_pay(c(
    "Salary: Hourly rate per Classified Salary Schedule, Lane 3/Step 0; $21.16 ($21.75 after successful completion",
    "Hiring bonus available; $18/hour",
    "Salary: $45,000; signing bonus $2,000"))
  expect_equal(p$Pay_Min, c(21.16, 18, 45000))
  expect_equal(p$Pay_Unit, c("hour", "hour", "year"))
})

test_that("a full or half day written before the amount is a day rate", {
  # Melstone 2026-11-17, the only case in either state's K-12 archives.
  p <- extract_posted_pay(c("Certified (teacher) substitute - Full day/$100 and 1/2 day/$50",
                            "Half day / $55", "Stipend tiers: 10-19 days worked/month = $100"))
  expect_equal(p$Pay_Min, c(100, 55, 100))
  expect_equal(p$Pay_Unit, c("day", "day", NA))
})

test_that("a pay grade's low-mid-high steps read as the first to the last amount", {
  # St. Labre's Trakstar board (2026-11-23): every classified and exempt grade is written this way.
  p <- extract_posted_pay(c("Salary Grade: 13A ($100,323-$125,404-$150,812)/yr",
                            "Job Code: 4B ($13.44 -$15.47 -$17.49)/hr.",
                            "Salary Grade: 4B ($13.44–$15.47-$17.49)",
                            "Salary Grade: 3B ($12.00- $13.81-$15.61)/hr."))
  expect_equal(p$Pay_Min, c(100323, 13.44, 13.44, 12))
  expect_equal(p$Pay_Max, c(150812, 17.49, 17.49, 15.61))
  expect_equal(p$Pay_Unit, c("year", "hour", "hour", "hour"))
  expect_equal(p$Pay_Text[1], "Salary Grade: 13A ($100,323-$125,404-$150,812)/yr")
  # Only a rising chain with its top under twice the middle: a third amount that's something else isn't the top.
  expect_equal(c(extract_posted_pay("$20 - $25 - $3,000 signing bonus")$Pay_Min,
                 extract_posted_pay("$20 - $25 - $3,000 signing bonus")$Pay_Max), c(20, 25))
  expect_equal(extract_posted_pay("$45,000-$50,000 - $1,000 stipend")$Pay_Max, 50000)
})

test_that("an hourly range that repeats 'per hour' after its low end is a range", {
  pay <- extract_posted_pay(c("Salary ranging from $16.97 per hour to $20.04 per hour DOE", "$21.46 per hour to $22.81 per hour",
                              "$15 per hour to 40 hours a week", "$15 per hour - $2,000 signing bonus",
                              "$15 per hour to $2,000 a month"))
  expect_equal(pay$Pay_Min, c(16.97, 21.46, 15, 15, 15))
  expect_equal(pay$Pay_Max, c(20.04, 22.81, 15, 15, 15))
  expect_equal(pay$Pay_Unit, rep("hour", 5))
})
