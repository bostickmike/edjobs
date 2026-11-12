# Scraper dispatch and the auto-fix issue body (phase 4, 2026-11-12).
#
# "Which function scrapes this source, with what arguments" as data. Until
# this slice MT kept a spec table keyed by registry platform and WY a switch
# with computed arguments, a second registry and a list of hard-coded calls.
# Here a state describes its sources once, in state_config(scraper_sources = ):
#   - scraper_registry(): a table with one row per source and a platform
#     column, plus the scraper_spec() for each platform;
#   - scraper_calls(): sources that are each one fully written call.
# resolve_scraper_call() looks a source name up in them, in order. The live
# check, the repro script and the auto-fix issue all go through it.
#
# The specs stay in the states for now: the two registries name platforms
# and arguments differently ("AppliTrack" + Slug, "Applitrack" + a path
# computed from Job_Link). Once they agree, the specs for the platforms this
# package scrapes can live here.

#' How one platform's scraper is called
#'
#' @param fn Name of the `fetch_*` function. With `map = TRUE`, the name of a
#'   named list of functions instead, keyed by the first argument (a state's
#'   per-district dispatch map); the entry is called with the session only.
#' @param args What to pass, in order: registry column names, and/or functions
#'   of the registry row (for an argument computed from a column). A single
#'   character vector of column names is the common case.
#' @param session TRUE when the scraper's first argument is a chromote session.
#' @param map See `fn`.
#' @param defined_in For `map = TRUE`, the file holding the map (shown to the
#'   auto-fix agent).
#' @return A list of class `edjobs_scraper_spec`.
#' @export
scraper_spec <- function(fn, args = character(0), session = FALSE, map = FALSE, defined_in = NULL) {
  stopifnot(is.character(fn), length(fn) == 1, nzchar(fn))
  if (!is.list(args)) args <- as.list(args)
  ok <- vapply(args, function(a) is.function(a) || (is.character(a) && length(a) == 1), logical(1))
  if (!all(ok)) stop("scraper_spec(args = ) entries must be column names or functions of the registry row")
  structure(list(fn = fn, args = args, session = isTRUE(session), map = isTRUE(map), defined_in = defined_in),
            class = "edjobs_scraper_spec")
}

#' A registry of sources, one row each
#'
#' @param table The registry: a data frame, the path of a CSV, or a function
#'   returning a data frame (read when a source is looked up, so a state's
#'   `state_config.R` can name an object defined later).
#' @param name_col The column holding the source name the drift check uses.
#' @param where What to call this registry in an issue ("k12_district_registry.csv").
#' @param specs Named list of `scraper_spec()`, one per platform.
#' @param platform_col The column naming the platform.
#' @param url_col The column holding the page a person would visit, or NULL.
#' @param fallback Optional `function(platform, row)` returning a
#'   `scraper_spec()` or NULL, for platforms that follow a naming convention
#'   instead of having an entry in `specs`.
#' @return A list of class `edjobs_scraper_source`.
#' @export
scraper_registry <- function(table, name_col, where, specs = list(), platform_col = "Platform",
                             url_col = "Job_Link", fallback = NULL) {
  stopifnot(is.data.frame(table) || is.function(table) || (is.character(table) && length(table) == 1),
            is.list(specs), all(vapply(specs, inherits, logical(1), "edjobs_scraper_spec")),
            is.null(fallback) || is.function(fallback))
  structure(list(kind = "registry", table = table, name_col = name_col, where = where, specs = specs,
                 platform_col = platform_col, url_col = url_col, fallback = fallback),
            class = "edjobs_scraper_source")
}

#' Sources that are each one written-out call
#'
#' For sources with no registry row: a call the pipeline makes by hand.
#' @param calls Named list (source name -> `list(fn =, args = list(...))`,
#'   optionally `session`, `platform` and `url`). Without `platform`, it is
#'   read off the function name (`fetch_neogov_postings` -> "neogov").
#' @param where What to call this group in an issue ("a hardcoded call in X.Rmd").
#' @return A list of class `edjobs_scraper_source`.
#' @export
scraper_calls <- function(calls, where) {
  stopifnot(is.list(calls), !is.null(names(calls)), all(nzchar(names(calls))),
            all(vapply(calls, function(x) is.character(x$fn) && length(x$fn) == 1, logical(1))))
  structure(list(kind = "calls", calls = calls, where = where), class = "edjobs_scraper_source")
}

# The session's scraper sources (state_config(scraper_sources = )).
state_scraper_sources <- function() {
  s <- current_state_config()$scraper_sources
  if (is.null(s)) list() else s
}

# One field of the session's auto-fix settings (state_config(autofix = )).
state_autofix <- function(field, default = NULL) {
  v <- current_state_config()$autofix[[field]]
  if (is.null(v)) default else v
}

scraper_source_table <- function(source) {
  t <- source$table
  if (is.function(t)) t <- t()
  if (is.character(t)) t <- utils::read.csv(t, stringsAsFactors = FALSE)
  t
}

#' The scraper call for a source
#'
#' Looks `name` up in each source group in order; the first group that lists
#' it decides. Pure lookup: run the result with `run_scraper_call()`.
#' @param name A drift-check source name (district or institution).
#' @param sources List of `scraper_registry()` / `scraper_calls()`; defaults
#'   to the session's `state_config(scraper_sources = )`.
#' @return `list(fn, args, session, map, defined_in, platform, where)` with
#'   `args` as actual values, or NULL when the source has no single-source
#'   scraper call (unknown name, or a platform with no spec).
#' @export
resolve_scraper_call <- function(name, sources = state_scraper_sources()) {
  for (source in sources) {
    if (identical(source$kind, "calls")) {
      spec <- source$calls[[name]]
      if (is.null(spec)) next
      platform <- if (is.null(spec$platform)) sub("^fetch_(.*?)_(postings|atom|jobs)$", "\\1", spec$fn) else spec$platform
      return(list(fn = spec$fn, args = as.list(spec$args), session = isTRUE(spec$session), map = FALSE,
                  defined_in = NULL, platform = platform, where = source$where))
    }
    table <- scraper_source_table(source)
    hit <- which(table[[source$name_col]] == name)
    if (length(hit) == 0) next
    row <- table[hit[1], , drop = FALSE]
    platform <- row[[source$platform_col]]
    spec <- source$specs[[platform]]
    if (is.null(spec) && !is.null(source$fallback)) spec <- source$fallback(platform, row)
    if (is.null(spec)) return(NULL)
    args <- lapply(spec$args, function(a) if (is.function(a)) a(row) else row[[a]])
    return(list(fn = spec$fn, args = args, session = spec$session, map = spec$map, defined_in = spec$defined_in,
                platform = platform, where = source$where))
  }
  NULL
}

#' Describe a scraper call
#'
#' Human-readable pointer to the code a resolved call runs.
#' @param call A `resolve_scraper_call()` result.
#' @export
describe_scraper_call <- function(call) {
  if (isTRUE(call$map)) {
    sprintf("the `%s[[\"%s\"]]` entry%s", call$fn, call$args[[1]],
            if (is.null(call$defined_in)) "" else paste0(" in ", call$defined_in))
  } else {
    sprintf("`%s()`", call$fn)
  }
}

#' Run a scraper call
#'
#' Executes a `resolve_scraper_call()` result against the live source.
#' @param call A `resolve_scraper_call()` result.
#' @param session_factory Called (no arguments) for a chromote session, only
#'   for session-backed scrapers; the session is closed afterwards.
#' @param map For a `map = TRUE` call, the map itself (so tests can pass a
#'   stub); by default looked up by name in `envir`.
#' @param envir Where to find the state's scraper functions.
#' @export
run_scraper_call <- function(call, session_factory = NULL, map = NULL, envir = parent.frame()) {
  if (isTRUE(call$map)) {
    if (is.null(map)) map <- get(call$fn, envir = envir)
    fn <- map[[call$args[[1]]]]
    args <- list()
  } else {
    fn <- get(call$fn, envir = envir, mode = "function")
    args <- call$args
  }
  if (!isTRUE(call$session)) return(do.call(fn, args))
  session <- session_factory()
  on.exit(tryCatch(session$close(), error = function(e) NULL), add = TRUE)
  do.call(fn, c(list(session), args))
}

#' Source name -> public page, for the corroboration step
#'
#' The page a person would visit for each source (not necessarily the
#' endpoint its scraper reads), from the registries' URL columns, the
#' written-out calls' `url`s and the statewide feed. When a name appears in
#' two groups the later group wins.
#' @param sources As for `resolve_scraper_call()`.
#' @param feed The statewide feed (`label`, `url`), or NULL; defaults to the
#'   session's `state_config(feed = )`.
#' @return A named character vector.
#' @export
build_source_url_lookup <- function(sources = state_scraper_sources(), feed = current_state_config()$feed) {
  lookup <- character(0)
  for (source in sources) {
    if (identical(source$kind, "calls")) {
      urls <- vapply(source$calls, function(x) if (is.null(x$url)) NA_character_ else x$url, character(1))
      lookup <- c(lookup, urls[!is.na(urls)])
    } else if (!is.null(source$url_col)) {
      table <- scraper_source_table(source)
      lookup <- c(lookup, stats::setNames(as.character(table[[source$url_col]]), table[[source$name_col]]))
    }
  }
  if (!is.null(feed)) lookup <- c(lookup, stats::setNames(feed$url, feed$label))
  lookup[!duplicated(names(lookup), fromLast = TRUE)]
}

#' One source name for every statewide-feed row
#'
#' A statewide feed gives a free-text location per posting, not a district
#' identity, so counted by name every location looks like its own source and
#' ordinary churn reads as dozens of sources dropping to zero. This relabels
#' every feed row to the feed itself, so the feed is drift-checked as one
#' source. Without `source_col` in `df` the names come back unchanged:
#' callers reading archives from before that column existed should skip them.
#' @param df Postings.
#' @param name_col The source-name column.
#' @param source_col The column saying where a row came from.
#' @param feed_label The feed's value in `source_col`; defaults to the
#'   session's `state_config(feed = )` label.
#' @return The per-row source names.
#' @export
collapse_statewide_feed_names <- function(df, name_col = "District", source_col = "Posting_Source",
                                          feed_label = state_feed("label")) {
  names_out <- as.character(df[[name_col]])
  if (!source_col %in% names(df) || is.null(feed_label)) return(names_out)
  is_feed <- !is.na(df[[source_col]]) & df[[source_col]] == feed_label
  names_out[is_feed] <- feed_label
  names_out
}

# ---- Evidence for the agent ------------------------------------------------
# The drift check renders each flagged page in CI, outside the agent's
# firewall. The agent's own render can't be trusted: on MT issue #7 its
# firewall blocked Apptegy's CDNs, the page came up empty, and it rewrote the
# fetch instead of fixing a renamed stop line. So the issue carries the text
# CI captured, the source's existing fixtures, and a diff between a text
# fixture and that capture. These helpers read the scraper and test files as
# text, since the script filing issues doesn't source the scrapers.

#' The `fetch_*` function a resolved call ends up in
#'
#' For a map call, read off the map's source lines: `"<key>" = fetch_x`, or
#' the first `fetch_` call in a wrapper entry (`"<key>" = function(session) {`).
#' @param call A `resolve_scraper_call()` result, or NULL.
#' @param scraper_lines The state's scraper files, as lines.
#' @return The function name, or NA.
#' @export
scraper_fetch_fn <- function(call, scraper_lines) {
  if (is.null(call)) return(NA_character_)
  if (!isTRUE(call$map)) return(call$fn)
  hit <- grep(sprintf('"%s" = fetch_', call$args[[1]]), scraper_lines, fixed = TRUE, value = TRUE)
  if (length(hit) > 0) return(sub('.*= (fetch_[A-Za-z0-9_]+).*', "\\1", hit[1]))
  start <- grep(sprintf('"%s" = function(', call$args[[1]]), scraper_lines, fixed = TRUE)
  if (length(start) == 0) return(NA_character_)
  body <- scraper_lines[start[1]:min(length(scraper_lines), start[1] + 5)]
  call_line <- grep("fetch_[A-Za-z0-9_]+\\(", body, value = TRUE)
  if (length(call_line) == 0) NA_character_ else sub('.*?(fetch_[A-Za-z0-9_]+)\\(.*', "\\1", call_line[1], perl = TRUE)
}

#' Fixtures a scraper's tests use
#'
#' Fixture files referenced by the tests of `fetch_fn` or its `parse_*` twin,
#' newest first: dated names by date, then undated ones in file order. For
#' fixtures whose names don't carry the source.
#' @param fetch_fn A `fetch_*` function name, or NA.
#' @param test_files Named list of test-file lines (name = file name).
#' @export
fixtures_used_by_tests <- function(fetch_fn, test_files) {
  if (is.na(fetch_fn)) return(character(0))
  fns <- c(fetch_fn, sub("^fetch_", "parse_", fetch_fn))
  found <- character(0)
  for (lines in test_files) {
    starts <- grep("^\\s*test_that\\(", lines)
    ends <- c(starts[-1] - 1, length(lines))
    for (i in seq_along(starts)) {
      if (!any(vapply(fns, grepl, logical(1), x = lines[starts[i]], fixed = TRUE))) next
      block <- lines[starts[i]:ends[i]]
      refs <- unlist(regmatches(block, gregexpr('test_path\\("fixtures", "[^"]+"\\)', block)))
      found <- c(found, sub('.*"([^"]+)"\\)$', "\\1", refs))
    }
  }
  found <- unique(found)
  dates <- ifelse(grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", found),
                  sub(".*([0-9]{4}-[0-9]{2}-[0-9]{2}).*", "\\1", found), "")
  found[order(dates, seq_along(found), decreasing = c(TRUE, FALSE), method = "radix")]
}

#' A district name's fixture tokens
#'
#' "Niobrara County School District 1" -> `c(place = "niobrara", number = "1")`.
#' @param name A source name.
#' @export
district_fixture_tokens <- function(name) {
  words <- tolower(unlist(strsplit(gsub("[^A-Za-z0-9 ]", " ", name), "\\s+")))
  words <- words[nzchar(words)]
  number <- utils::tail(words[grepl("^[0-9]+$", words)], 1)
  c(place = words[1], number = if (length(number)) number else "")
}

#' Fixtures named for a source
#'
#' For fixtures named `<platform>_<district>...` where several districts
#' share one parser: every fixture for the call's platform, and the ones for
#' this source (by the district's place name and number, best match first).
#' A place-only match never takes another numbered district of the same
#' place (Platte 2's `apptegy_platte2_*` is not Platte 1's).
#' @param call A `resolve_scraper_call()` result, or NULL.
#' @param source_name The source's name.
#' @param fixture_files Paths relative to the fixtures folder.
#' @return `list(platform = , own = )`.
#' @export
fixtures_named_for_source <- function(call, source_name, fixture_files) {
  if (is.null(call)) return(list(platform = character(0), own = character(0)))
  prefix <- paste0("^", tolower(call$platform), "_")
  platform <- fixture_files[grepl(prefix, tolower(basename(fixture_files)))]
  tok <- district_fixture_tokens(source_name)
  base <- tolower(basename(platform))
  exact <- nzchar(tok[["number"]]) & grepl(paste0(tok[["place"]], tok[["number"]]), base, fixed = TRUE)
  loose <- grepl(tok[["place"]], base, fixed = TRUE)
  loose <- loose & !grepl(paste0(tok[["place"]], "[0-9]"), base)
  own <- c(platform[exact], platform[loose & !exact])
  dates <- ifelse(grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", own), sub(".*([0-9]{4}-[0-9]{2}-[0-9]{2}).*", "\\1", own), "")
  list(platform = platform, own = own[order(dates, decreasing = TRUE, method = "radix")])
}

#' What the auto-fix issue shows the agent beyond the drift numbers
#'
#' @param call A `resolve_scraper_call()` result, or NULL.
#' @param source_name The source's name.
#' @param page_text The page text CI captured, or NA.
#' @param repo_root The state repository (a parameter so tests can point elsewhere).
#' @param scraper_files The state's scraper files; defaults to
#'   `state_config(autofix = list(scraper_files = ))`.
#' @param fixtures_by How the state's fixtures are found: `"name"`
#'   (`fixtures_named_for_source()`; a diff is only offered against this same
#'   source's fixture) or `"tests"` (`fixtures_used_by_tests()`; the diff is
#'   against the parser's newest text fixture). Defaults to
#'   `state_config(autofix = list(fixtures_by = ))`, else `"name"`.
#' @return A list: `fetch_fn`, `fixtures`, `platform_fixtures`, `fixtures_by`,
#'   `inner_text`, `page_text`, `diff_against`, `diff`.
#' @export
gather_autofix_evidence <- function(call, source_name, page_text, repo_root = ".",
                                    scraper_files = state_autofix("scraper_files", character(0)),
                                    fixtures_by = state_autofix("fixtures_by", "name")) {
  fixtures_by <- match.arg(fixtures_by, c("name", "tests"))
  scraper_lines <- unlist(lapply(file.path(repo_root, scraper_files), readLines, warn = FALSE))
  fixture_dir <- file.path(repo_root, "tests", "testthat", "fixtures")
  fetch_fn <- scraper_fetch_fn(call, scraper_lines)

  if (fixtures_by == "name") {
    fx <- fixtures_named_for_source(call, source_name, list.files(fixture_dir, recursive = TRUE))
  } else {
    test_paths <- list.files(file.path(repo_root, "tests", "testthat"), pattern = "^test-.*[.]R$", full.names = TRUE)
    test_files <- stats::setNames(lapply(test_paths, readLines, warn = FALSE), basename(test_paths))
    fx <- list(platform = character(0), own = fixtures_used_by_tests(fetch_fn, test_files))
  }
  inner_text <- scraper_reads_inner_text(fetch_fn, scraper_lines)
  has_text <- length(page_text) == 1 && !is.na(page_text) && nzchar(trimws(page_text))

  diff_against <- if (inner_text && has_text) utils::head(fx$own[grepl("[.]txt$", fx$own)], 1) else character(0)
  diff <- if (length(diff_against) == 1) {
    old <- paste(readLines(file.path(fixture_dir, diff_against), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    text_diff(old, page_text, diff_against, "page text captured in CI")
  } else character(0)

  list(fetch_fn = fetch_fn, fixtures = fx$own, platform_fixtures = fx$platform, fixtures_by = fixtures_by,
       inner_text = inner_text, page_text = if (has_text) page_text else NA_character_,
       diff_against = if (length(diff_against) == 1) diff_against else NA_character_,
       diff = diff)
}

#' The "What CI saw" section of the auto-fix issue
#'
#' @param evidence A `gather_autofix_evidence()` result, or NULL.
#' @param captured_on The capture date, as text.
#' @return Markdown lines (none when there is nothing to show).
#' @export
autofix_evidence_markdown <- function(evidence, captured_on) {
  if (is.null(evidence)) return(character(0))
  code_list <- function(x) paste0("`", x, "`", collapse = ", ")
  by_tests <- identical(evidence$fixtures_by, "tests")
  out <- c("### What CI saw", "")
  if (length(evidence$fixtures) > 0) {
    whose <- if (by_tests) sprintf("`%s()`", evidence$fetch_fn) else "this source"
    out <- c(out, sprintf("Existing fixtures for %s in `tests/testthat/fixtures/`, newest first: %s",
                          whose, code_list(evidence$fixtures)), "")
  }
  others <- setdiff(evidence$platform_fixtures, evidence$fixtures)
  if (length(others) > 0) {
    out <- c(out, sprintf("Other districts' fixtures for the same `%s()` parser (their tests must keep passing): %s",
                          evidence$fetch_fn, code_list(others)), "")
  }
  if (length(evidence$diff) > 0) {
    d <- evidence$diff
    cut <- length(d) > AUTOFIX_DIFF_MAX_LINES
    out <- c(out,
             sprintf("<details><summary>Diff: %s text fixture (<code>%s</code>) &rarr; page text captured in CI</summary>",
                     if (by_tests) "newest" else "this source's", evidence$diff_against),
             "", "````diff", utils::head(d, AUTOFIX_DIFF_MAX_LINES),
             if (cut) sprintf("... diff truncated (%d more lines)", length(d) - AUTOFIX_DIFF_MAX_LINES),
             "````", "", "</details>", "")
  }
  if (!is.na(evidence$page_text)) {
    txt <- evidence$page_text
    cut <- nchar(txt) > AUTOFIX_PAGE_TEXT_MAX_CHARS
    if (cut) txt <- substr(txt, 1, AUTOFIX_PAGE_TEXT_MAX_CHARS)
    out <- c(out,
             sprintf("<details><summary>Full page text captured in CI (<code>document.body.innerText</code>, %s)</summary>", captured_on),
             "", "````text", txt, if (cut) "... page text truncated", "````", "", "</details>", "")
  }
  if (length(out) == 2) character(0) else out
}

#' The body of a per-source auto-fix issue
#'
#' Written as the task prompt the coding agent works from. Carries an HTML
#' comment naming the source (`parse_autofix_marker()`), so the PR live check
#' can find which scraper a PR claims to fix.
#' @param row One `likely_broken` row of the corroboration results (`name`,
#'   `type`, `mean_count`, `count`, `url`, `llm_titles`).
#' @param call `resolve_scraper_call()`'s result for it, or NULL.
#' @param run_url The drift-check run, or NULL.
#' @param evidence `gather_autofix_evidence()`'s result, or NULL.
#' @param captured_on The capture date, as text.
#' @param config The state's `state_config()`: its `app_dir`, its registries'
#'   names and `autofix$fixture_example` (a path pattern for the new fixture,
#'   with `{platform}` and `{date}` filled in) go into the instructions.
#' @return Markdown lines.
#' @export
build_autofix_issue_body <- function(row, call = NULL, run_url = NULL, evidence = NULL,
                                     captured_on = as.character(Sys.Date()), config = current_state_config()) {
  titles <- if (is.na(row$llm_titles) || !nzchar(row$llm_titles)) character(0)
            else strsplit(row$llm_titles, " | ", fixed = TRUE)[[1]]
  # The CI capture is the new fixture verbatim only when the parser reads
  # innerText; otherwise it's context and the agent captures raw HTML.
  use_capture <- !is.null(evidence) && isTRUE(evidence$inner_text) && !is.na(evidence$page_text)
  by_tests <- identical(evidence$fixtures_by, "tests")
  repro <- sprintf("`Rscript scripts/repro_scraper.R \"%s\"`", row$name)

  example <- config$autofix$fixture_example
  if (is.null(example)) example <- "tests/testthat/fixtures/<source>_rendered_{date}.txt"
  example <- gsub("{date}", captured_on, example, fixed = TRUE)
  example <- gsub("{platform}", if (is.null(call)) "<platform>" else call$platform, example, fixed = TRUE)

  registries <- unlist(lapply(config$scraper_sources, function(s) if (identical(s$kind, "registry")) s$where))
  keep_out <- c(if (length(registries) > 0) sprintf("the registries (%s)", paste(registries, collapse = ", ")) else "the registries",
                if (is.null(config$app_dir)) "the accumulated data" else sprintf("the accumulated data under `%s/`", config$app_dir))

  c(
    sprintf("<!-- autofix-source: %s -->", row$name),
    "",
    sprintf("The weekly drift check found that **%s** (%s) averaged %.1f postings/week but the scraper returned **%d** this run, while the live page still lists real postings. The page markup most likely changed and the parser no longer matches it.",
            row$name, row$type, row$mean_count, as.integer(row$count)),
    "",
    sprintf("- **Live page:** %s", if (is.na(row$url)) "(none on file)" else row$url),
    if (!is.null(call)) sprintf("- **Platform:** `%s` (from %s)", call$platform, call$where),
    if (!is.null(call)) sprintf("- **Scraper entry point:** %s (and the `parse_*` function it calls, if any)", describe_scraper_call(call)),
    if (!is.null(call)) sprintf("- **Reproduce:** %s runs this scraper against its fixture tests and the live site", repro),
    if (!is.null(run_url)) sprintf("- **Drift-check run:** %s", run_url),
    "",
    if (length(titles) > 0) c(
      "An LLM read these postings off the live page (a hint, not ground truth -- verify against the page itself):",
      "",
      paste0("- ", titles),
      "",
      "The list covers the whole page, so it can include things this source's parser deliberately leaves out (e.g. contact info or a standing substitute-recruiting list). Restore what the parser used to find; see the comment above its `parse_*` function.",
      ""
    ),
    if (isTRUE(call$session)) c(
      "The drift check rendered this page in CI with chromote (`document.body.innerText`), so it renders normally outside your sandbox. If your render shows no postings, check the firewall's blocked hosts before changing how the scraper fetches the page -- see `.github/copilot-instructions.md`.",
      ""
    ),
    autofix_evidence_markdown(evidence, captured_on),
    "### Task",
    "",
    if (use_capture) {
      sprintf("1. Save the **Full page text captured in CI** above, verbatim, as a **new, dated fixture** next to this source's existing ones (e.g. `%s`). It is real captured data -- the exact text this scraper parses -- so prefer it over your own render. Keep the existing fixtures; the old layout must keep parsing.", example)
    } else {
      "1. Fetch the live page and save it as a **new, dated real fixture** in `tests/testthat/fixtures/` (keep the existing fixture -- the old layout must keep parsing)."
    },
    if (length(evidence$diff) > 0) {
      sprintf("2. Start from the diff above: it shows what changed since %s fixture. Fix the parser so it extracts the real postings from the new fixture. Keep the change minimal and in the existing style.",
              if (by_tests) "the newest" else "this source's")
    } else {
      "2. Fix the parser so it extracts the real postings from the new fixture. Keep the change minimal and in the existing style."
    },
    "3. Add a regression test against the new fixture asserting the exact titles found.",
    "4. Run `testthat::test_dir(\"tests/testthat\")` and make sure everything passes.",
    "",
    sprintf("Do **not** edit %s, or archives. If the source has moved to a different platform or URL, or the page genuinely has no postings, don't force a parser change -- say so in the PR description and stop.",
            paste(keep_out, collapse = ", ")),
    "",
    sprintf("The PR must reference this issue (`Fixes #<n>`) so the live check can find the source. Label: `%s`.", AUTOFIX_LABEL)
  )
}
