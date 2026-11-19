# The toy state (inst/toystate/): a made-up state with four districts, one per package platform (AppliTrack,
# SchoolSpring, TedK12, Apptegy), run end to end through the package as a third state would run it. HTTP answers
# come from real captured fixtures (the districts' names are made up; the postings are real), and the Apptegy page
# from a stand-in browser session. 2026-11-17 package review, step 3: it measures what a new state still has to
# write (the GAP functions in pipeline.R) and checks each later move against a whole run.

toy_fixture <- function(name) test_path("fixtures", name)

toy_state_env <- function() {
  toy_dir <- system.file("toystate", package = "edjobs")
  # Only the package's exports, as a state attaching library(edjobs) sees them.
  env <- new.env(parent = as.environment("package:edjobs"))
  env$toy_dir <- toy_dir
  sys.source(file.path(toy_dir, "state_config.R"), envir = env)
  sys.source(file.path(toy_dir, "pipeline.R"), envir = env)
  env
}

# Run 1 serves every fixture as captured; run 2 drops SchoolSpring's first posting (filled) and serves an empty
# TedK12 board.
toy_http <- function(run) {
  applitrack <- readBin(toy_fixture("applitrack_output_park1_pay.bin"), "raw", file.size(toy_fixture("applitrack_output_park1_pay.bin")))
  spring <- jsonlite::fromJSON(toy_fixture("schoolspring_lewistown.json"), simplifyVector = FALSE)
  if (run == 2) spring$value$jobsList <- spring$value$jobsList[-1]
  spring_page <- jsonlite::toJSON(spring, auto_unbox = TRUE, null = "null")
  spring_empty <- '{"success":true,"message":"","validationErrors":[],"value":{"page":2,"size":50,"jobsList":[]}}'
  tedk12 <- paste(readLines(toy_fixture("tedk12_goshen.html"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  if (run == 2) tedk12 <- sub("(?s)<table.*</table>", "<table></table>", tedk12, perl = TRUE)
  function(req) {
    u <- req$url
    body <- if (grepl("applitrack\\.com/aldercreek/", u)) applitrack
      else if (grepl("schoolspring\\.com/api/", u)) charToRaw(if (grepl("[?&]page=1(&|$)", u)) spring_page else spring_empty)
      else if (grepl("cedarflats\\.tedk12\\.com", u)) charToRaw(enc2utf8(tedk12))
      else stop("toy state: unexpected request ", u)
    httr2::response(status_code = 200, url = u, body = body)
  }
}

toy_session_factory <- function() {
  text <- paste(readLines(toy_fixture("apptegy_plevna_rendered.txt"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  function() list(Page = list(navigate = function(...) NULL, loadEventFired = function(...) NULL),
                  Runtime = list(evaluate = function(js) list(result = list(value = text))),
                  close = function() NULL)
}

toy_run <- function(env, out, run, run_date) {
  httr2::local_mocked_responses(toy_http(run))
  suppressMessages(env$toy_k12_run(out, as.Date(run_date), session_factory = toy_session_factory()))
}

test_that("the toy state runs end to end: two runs, a same-day re-run, its run expectations", {
  withr::defer(set_state_config(NULL))
  env <- toy_state_env()
  out <- withr::local_tempdir()
  app <- file.path(out, "app")

  c1 <- toy_run(env, out, 1, "2026-11-02")
  expect_named(c1, K12_POSTING_COLUMNS)
  by_district <- table(c1$District)
  expect_setequal(names(by_district), env$toy_registry()$District)
  expect_true(all(by_district > 0))
  methods <- tapply(c1$Posting_Identity_Method, c1$Posting_Source, unique)
  expect_equal(methods[["AppliTrack"]], "link"); expect_equal(methods[["SchoolSpring"]], "link")
  expect_equal(methods[["TedK12"]], "link"); expect_equal(methods[["Apptegy"]], "fallback")
  expect_true(any(c1$position == "Teacher"))
  expect_true(any(!is.na(c1$Pay_Min) & c1$Posting_Source == "AppliTrack"))      # posted pay read and kept
  expect_true(any(!is.na(c1$Est_Pay)))                                          # the state's own OEWS areas used

  c2 <- toy_run(env, out, 2, "2026-11-09")
  expect_false("Cedar Flats School District" %in% c2$District)                  # an empty board: no rows, no error
  spring1 <- c1$Posting_ID[c1$Posting_Source == "SchoolSpring"]; spring2 <- c2$Posting_ID[c2$Posting_Source == "SchoolSpring"]
  expect_equal(length(spring2), length(spring1) - 1)
  expect_true(all(spring2 %in% spring1))                                         # ids stay put across runs
  expect_setequal(c2$Posting_ID[c2$Posting_Source == "Apptegy"], c1$Posting_ID[c1$Posting_Source == "Apptegy"])
  log <- utils::read.csv(file.path(out, "scrape_log.csv"))
  expect_true(any(log$status == "empty"))

  teacher_rows <- nrow(utils::read.csv(file.path(app, "k12jobanalysis.csv")))
  totals <- utils::read.csv(file.path(app, "k12_district_weekly_totals.csv"))
  expect_setequal(unique(totals$Archive_Date), c("2026-11-02", "2026-11-09"))
  toy_run(env, out, 2, "2026-11-09")                                             # same-day re-run replaces
  expect_equal(nrow(utils::read.csv(file.path(app, "k12jobanalysis.csv"))), teacher_rows)

  exp <- read_run_expectations(file.path(env$toy_dir, "run_expectations.csv"))
  expect_equal(validate_run_expectations(exp, root = out, check_files = TRUE), character(0))
  res <- evaluate_run_expectations(exp, "2026-11-09", root = out)
  expect(all(res$Status == "pass"), paste(res$id[res$Status != "pass"], res$Status[res$Status != "pass"], res$Detail[res$Status != "pass"], collapse = "; "))
})

test_that("what a new state still writes: the toy's own functions (shrinks as stages move into the package)", {
  toy_dir <- system.file("toystate", package = "edjobs")
  defined <- function(f) {
    e <- parse(f, keep.source = FALSE)
    unlist(lapply(e, function(x) if (is.call(x) && identical(x[[1]], as.name("<-")) && is.call(x[[3]]) &&
                                       identical(x[[3]][[1]], as.name("function"))) as.character(x[[2]])))
  }
  expect_setequal(c(defined(file.path(toy_dir, "state_config.R")), defined(file.path(toy_dir, "pipeline.R"))),
                  c("toy_registry", "toy_raw_rows", "toy_posting_identity", "toy_k12_run"))
})

test_that("a state's own OEWS area rows come in through state_config()", {
  withr::defer(set_state_config(NULL))
  set_state_config(NULL)
  expect_error(oews_area_definitions("ZZ"), "No OEWS area definitions")
  areas <- system.file("toystate", "oews_areas.csv", package = "edjobs")
  set_state_config(state_config("Toyland", "99", "app", oews_areas = areas))
  expect_equal(oews_state_areas("ZZ")$code[1], "9900000")
  expect_equal(oews_area_for_county("North County", "ZZ"), "9900001")
  expect_true("MT" %in% oews_area_definitions()$state_abbr)                       # the package's own rows stay
  expect_error(state_config("Toyland", "99", "app", oews_areas = 3), "oews_areas")
})
