# Scraper dispatch and the auto-fix issue body (R/scraper_dispatch.R). The
# logic, on made-up sources; each state's own tests check that every one of
# its real registry rows resolves to a real function.

k12_table <- data.frame(
  District = c("Alpha County School District 1", "Beta Schools", "Gamma Schools", "Delta Schools", "Odd Schools"),
  Platform = c("Applitrack", "TylerPortico", "Apptegy", "DeltaHeuristic", "NoSuchPlatform"),
  Slug = c("", "beta", "", "", ""),
  Job_Link = c("https://www.applitrack.com/alpha/onlineapp/", "https://beta.example", "https://gamma.example",
               "https://delta.example", "https://odd.example"),
  stringsAsFactors = FALSE)
misc_table <- data.frame(District = c("Epsilon County School District 2", "Beta Schools"),
                         platform = c("apptegy", "wordpress"),
                         url = c("https://epsilon.example", "https://beta-misc.example"), stringsAsFactors = FALSE)

test_sources <- function() list(
  scraper_registry(function() k12_table, "District", "k12_district_registry.csv",
    specs = list(
      Applitrack = scraper_spec("fetch_applitrack_postings", list(function(row) sub(".*com/([^/]+)/.*", "\\1", row$Job_Link))),
      TylerPortico = scraper_spec("fetch_tylerportico_postings", c("Slug", "District")),
      Apptegy = scraper_spec("DISTRICT_SCRAPERS", "District", session = TRUE, map = TRUE, defined_in = "misc_district_scrapers.R")),
    fallback = function(platform, row) {
      if (grepl("Heuristic$", platform)) scraper_spec(paste0("fetch_", tolower(sub("Heuristic$", "", platform)), "_postings"), "Job_Link")
    }),
  scraper_registry(misc_table, "District", "misc_district_registry", platform_col = "platform", url_col = "url",
    specs = list(apptegy = scraper_spec("fetch_apptegy_postings", "url", session = TRUE),
                 wordpress = scraper_spec("fetch_wordpress_postings", "url"))),
  scraper_calls(list(
    "Zeta College" = list(fn = "fetch_neogov_postings", args = list("https://www.schooljobs.com", "zeta"),
                          url = "https://www.schooljobs.com/careers/zeta"),
    "Eta Charter" = list(fn = "fetch_paylocity_jobs", args = list("id", "eta"))),
    where = "a hardcoded call in the Rmd"))

with_test_state <- function(env = parent.frame(), autofix = list()) {
  old <- current_state_config()
  set_state_config(state_config("Test", "00", "Test_Ed_Jobs",
    feed = list(short_name = "FEED", label = "Statewide feed", url = "https://feed.example", discrepancy_file = "d.csv"),
    scraper_sources = test_sources(), autofix = autofix))
  withr::defer(set_state_config(old), envir = env)
}

test_that("scraper_spec and state_config reject malformed input", {
  expect_error(scraper_spec("f", list(1)), "column names or functions")
  expect_error(state_config("T", "00", "a", scraper_sources = list(list(fn = "x"))), "scraper_registry")
  expect_error(state_config("T", "00", "a", autofix = list(fixtures = "x")), "autofix takes")
  expect_error(state_config("T", "00", "a", autofix = list(fixtures_by = "magic")), "fixtures_by")
})

test_that("resolve_scraper_call reads columns, computes arguments and follows the fallback", {
  s <- test_sources()
  a <- resolve_scraper_call("Alpha County School District 1", s)
  expect_equal(a[c("fn", "args", "session", "map", "platform", "where")],
               list(fn = "fetch_applitrack_postings", args = list("alpha"), session = FALSE, map = FALSE,
                    platform = "Applitrack", where = "k12_district_registry.csv"))
  expect_equal(resolve_scraper_call("Beta Schools", s)$args, list("beta", "Beta Schools"))  # first group wins
  d <- resolve_scraper_call("Delta Schools", s)
  expect_equal(d$fn, "fetch_delta_postings")
  expect_equal(d$args, list("https://delta.example"))
  g <- resolve_scraper_call("Gamma Schools", s)
  expect_true(g$map && g$session)
  expect_equal(describe_scraper_call(g), "the `DISTRICT_SCRAPERS[[\"Gamma Schools\"]]` entry in misc_district_scrapers.R")
  expect_equal(describe_scraper_call(d), "`fetch_delta_postings()`")
})

test_that("resolve_scraper_call covers a second registry and written-out calls, and returns NULL otherwise", {
  s <- test_sources()
  e <- resolve_scraper_call("Epsilon County School District 2", s)
  expect_equal(e[c("fn", "session", "platform", "where")],
               list(fn = "fetch_apptegy_postings", session = TRUE, platform = "apptegy", where = "misc_district_registry"))
  z <- resolve_scraper_call("Zeta College", s)
  expect_equal(z$args, list("https://www.schooljobs.com", "zeta"))
  expect_equal(z$platform, "neogov")
  expect_equal(z$where, "a hardcoded call in the Rmd")
  expect_equal(resolve_scraper_call("Eta Charter", s)$platform, "paylocity")
  expect_null(resolve_scraper_call("Odd Schools", s))       # a platform with no spec
  expect_null(resolve_scraper_call("No Such District", s))
  expect_null(resolve_scraper_call("Zeta College", list()))
})

test_that("resolve_scraper_call defaults to the session's state config and reads a registry from a CSV", {
  with_test_state()
  expect_equal(resolve_scraper_call("Beta Schools")$fn, "fetch_tylerportico_postings")
  csv <- withr::local_tempfile(fileext = ".csv")
  write.csv(k12_table, csv, row.names = FALSE)
  s <- list(scraper_registry(csv, "District", "k12.csv", specs = list(TylerPortico = scraper_spec("f", "Slug"))))
  expect_equal(resolve_scraper_call("Beta Schools", s)$args, list("beta"))
})

test_that("run_scraper_call gives only chromote-backed scrapers a session, and closes it", {
  fake_http <- function(url) data.frame(Title = paste("got", url))
  fake_chromote <- function(chromote_session, url) data.frame(Title = paste(chromote_session$id, url))
  closed <- FALSE
  session <- list(id = "s1", close = function() closed <<- TRUE)
  expect_equal(run_scraper_call(list(fn = "fake_http", args = list("u"), session = FALSE))$Title, "got u")
  expect_equal(run_scraper_call(list(fn = "fake_chromote", args = list("u"), session = TRUE),
                                session_factory = function() session)$Title, "s1 u")
  expect_true(closed)
})

test_that("run_scraper_call hands a map entry the session, by name or as a stub", {
  fake_session <- list(close = function() NULL)
  DISTRICT_SCRAPERS <- list("Gamma Schools" = function(session) {
    data.frame(Title = if (identical(session, fake_session)) "got session" else "no session")
  })
  call <- resolve_scraper_call("Gamma Schools", test_sources())
  expect_equal(run_scraper_call(call, function() fake_session)$Title, "got session")
  stub <- list("Gamma Schools" = function(session) data.frame(Title = "stub"))
  expect_equal(run_scraper_call(call, function() fake_session, map = stub)$Title, "stub")
})

test_that("build_source_url_lookup combines every group and the feed; later groups win", {
  lookup <- build_source_url_lookup(test_sources(), feed = list(label = "Statewide feed", url = "https://feed.example"))
  expect_equal(unname(lookup["Gamma Schools"]), "https://gamma.example")
  expect_equal(unname(lookup["Beta Schools"]), "https://beta-misc.example")
  expect_equal(unname(lookup["Zeta College"]), "https://www.schooljobs.com/careers/zeta")
  expect_false("Eta Charter" %in% names(lookup))   # no url on file
  expect_equal(unname(lookup["Statewide feed"]), "https://feed.example")
  expect_false(anyDuplicated(names(lookup)) > 0)
  expect_false("Statewide feed" %in% names(build_source_url_lookup(test_sources(), feed = NULL)))
})

test_that("collapse_statewide_feed_names buckets every feed row and leaves direct rows alone", {
  with_test_state()
  df <- data.frame(District = c("SCOBEY", "Miles City, MT", "Billings Public Schools", "Yaak, Montana"),
                   Posting_Source = c("Statewide feed", "Statewide feed", "AppliTrack (direct district board)", "Statewide feed"),
                   stringsAsFactors = FALSE)
  expect_equal(collapse_statewide_feed_names(df),
               c("Statewide feed", "Statewide feed", "Billings Public Schools", "Statewide feed"))
  expect_equal(collapse_statewide_feed_names(df[, "District", drop = FALSE]), df$District)
  expect_equal(collapse_statewide_feed_names(df, feed_label = "Other"), df$District)
})

test_that("collapsing the statewide feed keeps a per-location drop from being flagged", {
  with_test_state()
  snap <- function(places) data.frame(District = c(places, "Billings Public Schools"),
                                      Posting_Source = c(rep("Statewide feed", length(places)), "AppliTrack"),
                                      stringsAsFactors = FALSE)
  raw <- list("2026-08-13" = snap(c("SCOBEY", "SCOBEY", "RICHEY")), "2026-08-20" = snap(c("JOLIET", "HALL", "POWER")))
  collapsed <- lapply(raw, function(df) data.frame(name = collapse_statewide_feed_names(df)))
  baseline <- build_historical_counts(collapsed, "name", valid_from = as.Date("2026-08-01"))
  current <- data.frame(name = c("Statewide feed", "Billings Public Schools"), count = c(3, 1))
  expect_equal(nrow(flag_drift(current, baseline, min_mean_count = 0)), 0)
})

# ---- evidence ---------------------------------------------------------------

map_call <- function(d) list(fn = "DISTRICT_SCRAPERS", args = list(d), session = TRUE, map = TRUE)

test_that("scraper_fetch_fn follows a map entry or a wrapper to its fetch_ function", {
  lines <- c('  "Columbia Falls School District 6" = function(session) {',
             '    fetch_redrover_postings(session, "https://jobs.redroverk12.com/org/sd659912",',
             '                            default_location = "Columbia Falls")',
             '  },',
             '  "Huntley Project School District" = fetch_huntley_postings,')
  expect_equal(scraper_fetch_fn(map_call("Columbia Falls School District 6"), lines), "fetch_redrover_postings")
  expect_equal(scraper_fetch_fn(map_call("Huntley Project School District"), lines), "fetch_huntley_postings")
  expect_true(is.na(scraper_fetch_fn(map_call("Nowhere School District"), lines)))
  expect_equal(scraper_fetch_fn(list(fn = "fetch_applitrack_postings", args = list("x"), session = FALSE), lines),
               "fetch_applitrack_postings")
  expect_true(is.na(scraper_fetch_fn(NULL, lines)))
})

test_that("fixtures_used_by_tests finds a scraper's fixtures from its tests, dated newest first", {
  tests <- list("test-a.R" = c(
    'test_that("parse_gamma_postings reads the old layout", {',
    '  x <- readLines(test_path("fixtures", "gamma_rendered.txt"))',
    '})',
    'test_that("fetch_gamma_postings reads the new layout", {',
    '  x <- readLines(test_path("fixtures", "gamma_content_2026-09-30.txt"))',
    '})',
    'test_that("fetch_other_postings", {',
    '  x <- readLines(test_path("fixtures", "other.html"))',
    '})'))
  expect_equal(fixtures_used_by_tests("fetch_gamma_postings", tests), c("gamma_content_2026-09-30.txt", "gamma_rendered.txt"))
  expect_equal(fixtures_used_by_tests(NA_character_, tests), character(0))
})

test_that("fixtures_named_for_source picks this district's fixture, not another's on the same parser", {
  files <- c("misc_districts/apptegy_niobrara_rendered.txt", "misc_districts/apptegy_platte2_rendered.txt",
             "misc_districts/apptegy_weston7_rendered.txt", "misc_districts/apptegy_weston7_rendered_2026-10-01.txt",
             "misc_districts/smartsites_platte1.html", "applitrack_output.txt")
  apptegy <- list(fn = "fetch_apptegy_postings", platform = "apptegy")
  expect_equal(district_fixture_tokens("Niobrara County School District 1"), c(place = "niobrara", number = "1"))
  nio <- fixtures_named_for_source(apptegy, "Niobrara County School District 1", files)
  expect_equal(nio$own, "misc_districts/apptegy_niobrara_rendered.txt")
  expect_length(nio$platform, 4)
  w7 <- fixtures_named_for_source(apptegy, "Weston County School District 7", files)
  expect_equal(w7$own, c("misc_districts/apptegy_weston7_rendered_2026-10-01.txt", "misc_districts/apptegy_weston7_rendered.txt"))
  # Platte 1 on the same parser has no fixture: Platte 2's is not its own.
  expect_equal(fixtures_named_for_source(apptegy, "Platte County School District 1", files)$own, character(0))
  p1 <- fixtures_named_for_source(list(platform = "smartsites"), "Platte County School District 1", files)
  expect_equal(p1$own, "misc_districts/smartsites_platte1.html")
  # The registry says "Applitrack", the fixture "applitrack_".
  expect_length(fixtures_named_for_source(list(platform = "Applitrack"), "Teton County School District 1", files)$platform, 1)
  expect_equal(fixtures_named_for_source(NULL, "x", files), list(platform = character(0), own = character(0)))
})

# A small state repository on disk: one innerText scraper, one HTTP scraper.
fake_repo <- function(env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  writeLines(c("fetch_apptegy_postings <- function(chromote_session, url) {",
               "  text <- chromote_session$Runtime$evaluate('document.body.innerText')",
               "}",
               "fetch_wordpress_postings <- function(url) {",
               "  rvest::read_html(url)",
               "}",
               "fetch_gamma_postings <- function(chromote_session) {",
               "  chromote_session$Runtime$evaluate('document.body.innerText')",
               "}",
               "DISTRICT_SCRAPERS <- list(",
               '  "Gamma Schools" = fetch_gamma_postings,',
               ")"), file.path(root, "misc_district_scrapers.R"))
  fx <- file.path(root, "tests", "testthat", "fixtures", "misc_districts")
  dir.create(fx, recursive = TRUE)
  writeLines(c("Employment", "Elementary Teacher: open", "", "Bus Driver", "Contact us"), file.path(fx, "apptegy_epsilon2_rendered.txt"))
  writeLines(c("Employment", "Cook"), file.path(fx, "apptegy_theta_rendered.txt"))
  writeLines(c("Careers", "Custodian", "Stop here"), file.path(dirname(fx), "gamma_rendered.txt"))
  writeLines(c('test_that("parse_gamma_postings reads the page", {',
               '  readLines(test_path("fixtures", "gamma_rendered.txt"))', "})"),
             file.path(root, "tests", "testthat", "test-gamma.R"))
  root
}
capture <- "Employment\nMusic Teacher: open\nBus Driver\nContact us"
flagged_row <- function(name = "Epsilon County School District 2", titles = "Music Teacher | Bus Driver") {
  data.frame(name = name, type = "K-12", mean_count = 5, count = 0, url = "https://epsilon.example",
             llm_titles = titles, stringsAsFactors = FALSE)
}

test_that("fixtures by name: the CI capture, a same-source diff and the other districts' fixtures", {
  skip_if(!nzchar(Sys.which("diff")), "no system diff")
  with_test_state(autofix = list(scraper_files = "misc_district_scrapers.R",
                                 fixture_example = "tests/testthat/fixtures/misc_districts/{platform}_<district>_rendered_{date}.txt"))
  root <- fake_repo()
  call <- resolve_scraper_call("Epsilon County School District 2")
  ev <- gather_autofix_evidence(call, "Epsilon County School District 2", capture, repo_root = root)
  expect_true(ev$inner_text)
  expect_equal(ev$fixtures_by, "name")
  expect_equal(ev$diff_against, "misc_districts/apptegy_epsilon2_rendered.txt")
  expect_true(any(startsWith(ev$diff, "-Elementary Teacher:")))
  expect_equal(ev$platform_fixtures, c("misc_districts/apptegy_epsilon2_rendered.txt", "misc_districts/apptegy_theta_rendered.txt"))

  body <- paste(build_autofix_issue_body(flagged_row(), call, "https://run", ev, captured_on = "2026-09-30"), collapse = "\n")
  expect_equal(parse_autofix_marker(body), "Epsilon County School District 2")
  expect_equal(parse_autofix_expected_titles(body), c("Music Teacher", "Bus Driver"))
  for (piece in c("- **Platform:** `apptegy` (from misc_district_registry)", "`fetch_apptegy_postings()`", "https://run",
                  "### What CI saw", "Existing fixtures for this source in", "Diff: this source's text fixture",
                  "Other districts' fixtures for the same `fetch_apptegy_postings()` parser (their tests must keep passing): `misc_districts/apptegy_theta_rendered.txt`",
                  "1. Save the **Full page text captured in CI**",
                  "(e.g. `tests/testthat/fixtures/misc_districts/apptegy_<district>_rendered_2026-09-30.txt`)",
                  "what changed since this source's fixture", "rendered this page in CI", "deliberately leaves out",
                  'scripts/repro_scraper.R "Epsilon County School District 2"',
                  "Do **not** edit the registries (k12_district_registry.csv, misc_district_registry), the accumulated data under `Test_Ed_Jobs/`, or archives.")) {
    expect_match(body, piece, fixed = TRUE)
  }
})

test_that("fixtures by name: a shared-parser source with no fixture of its own gets no diff", {
  with_test_state(autofix = list(scraper_files = "misc_district_scrapers.R"))
  root <- fake_repo()
  call <- list(fn = "fetch_apptegy_postings", args = list("u"), session = TRUE, map = FALSE, platform = "apptegy", where = "x")
  ev <- gather_autofix_evidence(call, "Iota County School District 9", capture, repo_root = root)
  expect_true(is.na(ev$diff_against))
  expect_equal(ev$diff, character(0))
  expect_equal(ev$fixtures, character(0))
  expect_length(ev$platform_fixtures, 2)
})

test_that("fixtures by tests: a map entry's parser, its fixtures and a diff against the newest", {
  skip_if(!nzchar(Sys.which("diff")), "no system diff")
  with_test_state(autofix = list(scraper_files = "misc_district_scrapers.R", fixtures_by = "tests"))
  root <- fake_repo()
  call <- resolve_scraper_call("Gamma Schools")
  ev <- gather_autofix_evidence(call, "Gamma Schools", "Careers\nCustodian\nCook\nEnd of list", repo_root = root)
  expect_equal(ev$fetch_fn, "fetch_gamma_postings")
  expect_equal(ev$fixtures, "gamma_rendered.txt")
  expect_equal(ev$platform_fixtures, character(0))
  expect_equal(ev$diff_against, "gamma_rendered.txt")
  body <- paste(build_autofix_issue_body(flagged_row("Gamma Schools"), call, evidence = ev, captured_on = "2026-09-30"), collapse = "\n")
  for (piece in c("Existing fixtures for `fetch_gamma_postings()` in", "Diff: newest text fixture",
                  "the `DISTRICT_SCRAPERS[[\"Gamma Schools\"]]` entry in misc_district_scrapers.R",
                  "(e.g. `tests/testthat/fixtures/<source>_rendered_2026-09-30.txt`)", "what changed since the newest fixture")) {
    expect_match(body, piece, fixed = TRUE)
  }
  expect_no_match(body, "Other districts' fixtures", fixed = TRUE)
})

test_that("an HTTP scraper's issue keeps the capture as context and asks for a raw fixture", {
  with_test_state(autofix = list(scraper_files = "misc_district_scrapers.R"))
  root <- fake_repo()
  call <- resolve_scraper_call("Beta Schools", test_sources()[2])
  ev <- gather_autofix_evidence(call, "Beta Schools", "Custodian\nAdjunct", repo_root = root)
  expect_false(ev$inner_text)
  expect_equal(ev$diff, character(0))
  body <- paste(build_autofix_issue_body(flagged_row("Beta Schools"), call, evidence = ev), collapse = "\n")
  expect_match(body, "1. Fetch the live page", fixed = TRUE)
  expect_match(body, "Full page text captured in CI", fixed = TRUE)
  expect_no_match(body, "Diff:", fixed = TRUE)
  expect_no_match(body, "rendered this page in CI", fixed = TRUE)
})

test_that("build_autofix_issue_body tolerates no LLM titles, no scraper call and no state config", {
  old <- current_state_config(); set_state_config(NULL); withr::defer(set_state_config(old))
  row <- data.frame(name = "Mystery", type = "K-12", mean_count = 4, count = 0, url = NA_character_,
                    llm_titles = NA_character_, stringsAsFactors = FALSE)
  body <- paste(build_autofix_issue_body(row), collapse = "\n")
  expect_equal(parse_autofix_marker(body), "Mystery")
  expect_no_match(body, "An LLM read")
  expect_no_match(body, "repro_scraper")
  expect_match(body, "Do **not** edit the registries, the accumulated data, or archives.", fixed = TRUE)
  expect_match(body, "(none on file)", fixed = TRUE)
  expect_equal(parse_autofix_expected_titles(paste(build_autofix_issue_body(flagged_row()), collapse = "\n")),
               c("Music Teacher", "Bus Driver"))
})

test_that("diff and page-text lines never leak into the live check's expected titles", {
  ev <- list(fetch_fn = "fetch_x_postings", fixtures = "x.txt", platform_fixtures = "x.txt", inner_text = TRUE,
             page_text = "- Not A Title\nreal text", diff_against = "x.txt",
             diff = c("--- x.txt", "+++ ci", "- Also Not A Title", "+new"))
  body <- paste(build_autofix_issue_body(flagged_row(), evidence = ev), collapse = "\n")
  expect_equal(parse_autofix_expected_titles(body), c("Music Teacher", "Bus Driver"))
})

test_that("a long capture is truncated to fit an issue body", {
  ev <- list(fetch_fn = "fetch_x_postings", fixtures = character(0), platform_fixtures = character(0),
             inner_text = TRUE, page_text = strrep("x", AUTOFIX_PAGE_TEXT_MAX_CHARS * 3),
             diff_against = NA_character_, diff = paste0("+", seq_len(AUTOFIX_DIFF_MAX_LINES * 2)))
  md <- autofix_evidence_markdown(ev, "2026-09-30")
  expect_lt(sum(nchar(md)), 60000)  # GitHub caps issue bodies at 65,536 chars
  expect_true(any(grepl("page text truncated", md, fixed = TRUE)))
  expect_true(any(grepl("diff truncated", md, fixed = TRUE)))
  expect_equal(autofix_evidence_markdown(NULL, "2026-09-30"), character(0))
})

test_that("fixtures_named_for_source: district 1's fixture is not district 14's (2026-11-13 review)", {
  files <- c("apptegy_fremont14_2026-10-01.html", "apptegy_fremont1_2026-09-01.html")
  own <- fixtures_named_for_source(list(platform = "apptegy"), "Fremont County School District 1", files)$own
  expect_equal(own, "apptegy_fremont1_2026-09-01.html")
  expect_equal(fixtures_named_for_source(list(platform = "apptegy"), "Fremont County School District 1",
                                         files[1])$own, character(0))
})
