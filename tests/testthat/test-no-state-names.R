# Package code names no state: a state's name, abbreviation, feed or time zone comes in through state_config()
# (vignette("edjobs"); 2026-11-17 package review, finding 5). Reads the string constants of every file in R/ (comments
# and roxygen are free text and may name the states that motivated a rule). Full state names match in any case;
# abbreviations only in capitals ("in", "or", "me" are words).

STATE_NAME_ALLOWED <- list(
  # Stored key prefixes in the committed presence files (presence_sightings.csv): renaming them would split every
  # open stretch. They name the two feed adapters, opi_posting_key() and wsba_vacancy_key().
  "presence_check.R" = c("opi:", "wsba:"),
  # The canary's status words, not Oklahoma.
  "run_checks_canary.R" = c("OK", "[OK]  ")
)

package_code_strings <- function(file) {
  pd <- utils::getParseData(parse(file, keep.source = TRUE), includeText = TRUE)
  s <- pd[pd$token == "STR_CONST", c("line1", "text")]
  s$value <- vapply(s$text, function(x) eval(parse(text = x)), character(1), USE.NAMES = FALSE)
  s
}

test_that("no string constant in R/ names a state, a statewide feed or a state's time zone", {
  r_dir <- test_path("..", "..", "R")
  skip_if_not(dir.exists(r_dir), "package source not available (installed tests)")
  full <- paste0("(?i)\\b(", paste(datasets::state.name, collapse = "|"), ")\\b")
  abbr <- paste0("\\b(", paste(datasets::state.abb, collapse = "|"), ")\\b")
  other <- "(?i)\\b(opi|wsba)\\b|America/(Denver|Boise|Phoenix|Chicago|New_York|Los_Angeles)"
  hits <- character(0)
  for (f in list.files(r_dir, pattern = "\\.R$", full.names = TRUE)) {
    s <- package_code_strings(f)
    if (!nrow(s)) next
    bad <- grepl(full, s$value, perl = TRUE) | grepl(abbr, s$value, perl = TRUE) | grepl(other, s$value, perl = TRUE)
    bad <- bad & !s$value %in% STATE_NAME_ALLOWED[[basename(f)]]
    if (any(bad)) hits <- c(hits, paste0(basename(f), ":", s$line1[bad], " ", substr(s$value[bad], 1, 80)))
  }
  expect(length(hits) == 0, paste0("State names in package code (use state_config()):\n", paste(hits, collapse = "\n")))
})

test_that("the address-line filter covers every state, not only the two dashboards'", {
  expect_true(grepl(US_STATE_ZIP_RX, "townsend, montana 59644", perl = TRUE))
  expect_true(grepl(US_STATE_ZIP_RX, "rawlins, wy 82301", perl = TRUE))
  expect_true(grepl(US_STATE_ZIP_RX, "boise, id 83702", perl = TRUE))
  expect_false(grepl(US_STATE_ZIP_RX, "math teacher", perl = TRUE))
})

test_that("state_timezone() reads the state config, and says so when none is set", {
  withr::defer(set_state_config(NULL))
  set_state_config(NULL)
  expect_error(state_timezone(), "No state time zone")
  set_state_config(state_config("Testland", "99", "app", timezone = "America/Chicago"))
  expect_equal(state_timezone(), "America/Chicago")
  expect_error(state_config("Testland", "99", "app", timezone = "Mountain"), "OlsonNames")
})
