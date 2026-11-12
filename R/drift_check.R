# Shared parts of the weekly drift and data-quality checks: page-text job
# signal scoring and LLM verdicts, title lint, keyword conflicts, Other-share
# and pay-coverage checks, salary value checks, and the scraper auto-fix
# helpers (issue markers, engine choice, escalation to Claude). Moved from the
# MT and WY repos' drift_check.R in phase 2 (2026-11-11), where these
# definitions were identical. Each state's drift_check.R keeps its source
# dispatch, baselines, statewide feed and issue body code.

#' Pay coverage counts
#'
#' One row per source: postings (n) and how many state pay (with_pay).
#' @param names See the description.
#' @param has_pay See the description.
#' @export
pay_coverage_counts <- function(names, has_pay) {
  if (length(names) == 0) {
    return(data.frame(name = character(0), n = integer(0), with_pay = integer(0),
                      stringsAsFactors = FALSE))
  }
  n <- tapply(rep(1L, length(names)), names, sum)
  with_pay <- tapply(as.integer(has_pay), names, sum)
  data.frame(name = names(n), n = as.integer(n), with_pay = as.integer(with_pay[names(n)]),
             stringsAsFactors = FALSE)
}

#' Flag pay coverage drop
#'
#' current: pay_coverage_counts() for this run. history: a list of the same,
#' one per archived week that had Pay_*. A source is flagged when it has at
#' least min_weeks weeks with min_postings or more postings, its average
#' share with pay over those weeks is at least min_share, and this run --
#' with min_postings or more postings, so a source that lost its postings is
#' left to flag_drift() -- has a share at or below drop_threshold times that
#' average.
#' @param current See the description.
#' @param history See the description.
#' @param min_weeks See the description.
#' @param min_postings See the description.
#' @param min_share See the description.
#' @param drop_threshold See the description.
#' @export
flag_pay_coverage_drop <- function(current, history, min_weeks = 2, min_postings = 3,
                                   min_share = 0.4, drop_threshold = 0.2) {
  empty <- data.frame(name = character(0), mean_share = numeric(0), n_weeks = integer(0),
                      n = integer(0), with_pay = integer(0), stringsAsFactors = FALSE)
  weeks <- do.call(rbind, history)
  if (is.null(weeks) || nrow(weeks) == 0 || nrow(current) == 0) return(empty)
  weeks <- weeks[weeks$n >= min_postings, , drop = FALSE]
  if (nrow(weeks) == 0) return(empty)
  weeks$share <- weeks$with_pay / weeks$n
  baseline <- data.frame(
    name = sort(unique(weeks$name)),
    stringsAsFactors = FALSE
  )
  baseline$n_weeks <- as.integer(table(weeks$name)[baseline$name])
  baseline$mean_share <- as.numeric(tapply(weeks$share, weeks$name, mean)[baseline$name])

  merged <- merge(baseline, current, by = "name")
  merged <- merged[merged$n >= min_postings & merged$n_weeks >= min_weeks &
                     merged$mean_share >= min_share, , drop = FALSE]
  flagged <- merged[merged$with_pay / merged$n <= merged$mean_share * drop_threshold, , drop = FALSE]
  if (nrow(flagged) == 0) return(empty)
  flagged[order(-flagged$mean_share), c("name", "mean_share", "n_weeks", "n", "with_pay")]
}

#' Build pay coverage report markdown
#' @param flagged See the description.
#' @param date See the description.
#' @export
build_pay_coverage_report_markdown <- function(flagged, date = Sys.Date()) {
  if (nrow(flagged) == 0) return(character(0))
  c(
    paste0("### Posted pay went missing (", date, ")"),
    "",
    paste("These sources still return postings, but far fewer of them carry the pay the posting states",
          "than usual -- the pay field or detail request the scraper reads has likely changed.",
          "Postings and counts are unaffected; the Posted Pay column is blank for them."),
    "",
    sprintf("- **%s** (%s): usually %.0f%% of postings state pay, now %d of %d",
            flagged$name, flagged$type, 100 * flagged$mean_share, flagged$with_pay, flagged$n),
    ""
  )
}

#' Attach scrape log errors
#'
#' A source dropping to (near) zero is ambiguous on count alone: a genuinely
#' quiet week looks identical to a scraper that started erroring. But
#' safe_scrape() (scrape_helpers.R) already logs which one happened, to
#' scrape_log.csv, in the very same pipeline run that produced this week's
#' drift-flagged counts -- so check there first, before spending a live
#' chromote render on a guess. A source whose most recent logged attempt
#' this run was a real "error" (not "empty") is a much stronger and cheaper
#' signal: the registered URL itself is broken (a dead ATS tenant, a DNS
#' failure, a migrated platform, an HTTP error perform_with_retry() couldn't
#' recover from), not just "no visible postings right now". scrape_log's
#' `source` strings aren't always an exact match for a flagged `name` (a
#' platform-prefixed source name, e.g. "Apptegy/chromote: <District>",
#' would still need this even though nothing in this project currently logs
#' that way), so match by substring containment rather than requiring
#' equality.
#' @param flagged See the description.
#' @param scrape_log See the description.
#' @export
attach_scrape_log_errors <- function(flagged, scrape_log) {
  flagged$scrape_error <- rep(NA_character_, nrow(flagged))
  if (nrow(flagged) == 0 || nrow(scrape_log) == 0) return(flagged)

  # Keep only each source's single most recent logged attempt -- a source
  # that errored earlier in the run but succeeded on a later retry/re-run
  # must NOT be reported as currently broken, so status is checked on the
  # latest attempt, not on "was there ever an error this run".
  latest <- scrape_log[order(scrape_log$timestamp), ]
  latest <- latest[!duplicated(latest$source, fromLast = TRUE), ]
  errors <- latest[!is.na(latest$status) & latest$status == "error", ]
  if (nrow(errors) == 0) return(flagged)

  for (i in seq_len(nrow(flagged))) {
    hits <- which(vapply(errors$source, function(s) grepl(flagged$name[i], s, fixed = TRUE), logical(1)))
    if (length(hits) > 0) flagged$scrape_error[i] <- errors$error_message[hits[1]]
  }
  flagged
}

#' Build implausible pay report markdown
#'
#' Report section for this week's posted amounts that drop_implausible_pay()
#' blanked (rows of `bad`: source, title, Pay_Min, Pay_Max, Pay_Unit,
#' Pay_Text). A misread -- a fee, a benefit's value -- usually means the
#' extractor needs a new skip rule; a real but unusual figure means
#' PAY_PLAUSIBLE_RANGE needs widening.
#' @param bad See the description.
#' @param date See the description.
#' @param max_rows See the description.
#' @export
build_implausible_pay_report_markdown <- function(bad, date = Sys.Date(), max_rows = 25) {
  if (nrow(bad) == 0) return(character(0))
  shown <- utils::head(bad, max_rows)
  amount <- ifelse(shown$Pay_Min == shown$Pay_Max | is.na(shown$Pay_Max),
                   sprintf("$%s", format(shown$Pay_Min, big.mark = ",")),
                   sprintf("$%s-$%s", format(shown$Pay_Min, big.mark = ","), format(shown$Pay_Max, big.mark = ",")))
  c(
    paste0("### Posted pay blanked as implausible (", date, ")"),
    "",
    paste("These amounts were read from the postings but fall outside PAY_PLAUSIBLE_RANGE",
          "(edjobs, R/k12_he_classification.R) for their unit, so the dashboard shows no posted pay for them.",
          "The raw archive keeps them. A misread needs an extractor fix; a real figure needs the range widened."),
    "",
    sprintf("- **%s** (%s) \"%s\": %s per %s -- \"%s\"",
            shown$source, shown$type, shown$title, amount, ifelse(is.na(shown$Pay_Unit), "unknown unit", shown$Pay_Unit),
            substr(gsub("[\r\n]+", " ", shown$Pay_Text), 1, 120)),
    if (nrow(bad) > max_rows) sprintf("- ...and %d more", nrow(bad) - max_rows),
    ""
  )
}

#' Check salary coverage
#'
#' Salary data (WSBA for K-12, IPEDS for Higher Ed -- see salary_scrapers.R
#' and ipeds_salary_scraper.R) has a small, essentially fixed universe (48
#' WY school districts, 9 WY public HE institutions) and changes far less
#' often than job postings (once a year, not weekly), so a trailing
#' statistical baseline like flag_drift() doesn't fit. Instead this is a
#' hard assertion against that known universe size: a parser silently
#' extracting fewer matched records than the known-fixed count is the same
#' failure signature as the Applitrack encoding bug -- the scrape "succeeds"
#' (safe_scrape logs status "ok", n_rows > 0) while quietly returning much
#' less real data than before, because the source changed its page/PDF/API
#' layout out from under the parser.
#' @param name See the description.
#' @param actual See the description.
#' @param expected See the description.
#' @param min_ok See the description.
#' @export
check_salary_coverage <- function(name, actual, expected, min_ok = expected) {
  if (actual >= min_ok) return(NULL)
  data.frame(name = name, expected = expected, actual = actual, stringsAsFactors = FALSE)
}

#' Check salary value bounds
#'
#' actual: named numeric vector (name = district/institution, value = salary).
#' @param name See the description.
#' @param actual See the description.
#' @param min_ok See the description.
#' @param max_ok See the description.
#' @export
check_salary_value_bounds <- function(name, actual, min_ok, max_ok) {
  bad <- actual < min_ok | actual > max_ok
  bad[is.na(bad)] <- FALSE
  if (!any(bad)) return(NULL)
  data.frame(
    name = name, entity = names(actual)[bad], value = unname(actual[bad]),
    min_ok = min_ok, max_ok = max_ok, stringsAsFactors = FALSE
  )
}

#' Check salary yoy plausibility
#'
#' current/prior: named numeric vectors (name = district/institution),
#' compared pairwise by name. Flags an entity whose |\% change| both (a)
#' exceeds hard_ceiling outright (a settlement essentially never moves this
#' much in one real year) and (b) is a real statistical outlier against
#' every OTHER entity's change this same run (median absolute deviation,
#' robust to one or two entities genuinely having an unusual year) --
#' requiring both avoids flagging a single district with a real large
#' settlement while every other district also moved a lot (a real
#' statewide event, not a parser bug), and avoids flagging a small
#' percentage move that's just normal variation.
#' @param current See the description.
#' @param prior See the description.
#' @param hard_ceiling See the description.
#' @param mad_multiplier See the description.
#' @export
check_salary_yoy_plausibility <- function(current, prior, hard_ceiling = 0.25, mad_multiplier = 5) {
  common <- intersect(names(current), names(prior))
  cur <- current[common]
  pri <- prior[common]
  valid <- !is.na(cur) & !is.na(pri) & pri != 0
  if (sum(valid) < 3) return(NULL)  # too few points for a cross-sectional outlier check to mean anything

  pct_change <- (cur[valid] - pri[valid]) / pri[valid]
  center <- stats::median(pct_change)
  spread <- stats::mad(pct_change)

  is_outlier <- if (spread > 0) {
    abs(pct_change - center) > mad_multiplier * spread & abs(pct_change) > hard_ceiling
  } else {
    # A zero spread means every peer moved by (near) the exact same amount
    # -- a MAD-based threshold would then be 0, and "greater than 0" would
    # flag ordinary tiny floating-point variation between otherwise-equal
    # changes. Fall back to the hard ceiling alone: still lets a genuine
    # uniform statewide move of, say, 15% through untouched (below the
    # ceiling), while still catching one district at 47% against five
    # peers that didn't move at all (this function's whole reason to
    # exist), which a spread-based threshold of 0 could never do since
    # nothing can exceed a threshold of Inf.
    abs(pct_change) > hard_ceiling
  }
  if (!any(is_outlier)) return(NULL)

  data.frame(
    name = names(pct_change)[is_outlier],
    prior = unname(pri[valid][is_outlier]),
    current = unname(cur[valid][is_outlier]),
    pct_change = unname(pct_change[is_outlier]),
    stringsAsFactors = FALSE
  )
}

#' Score page text for job signal
#'
#' page_text: visible rendered text of a live page (document.body.innerText
#' via chromote, same technique used for the 2026-08-03 manual spot-check).
#' Returns one of "likely_broken", "looks_genuinely_empty", or
#' "inconclusive" -- deliberately three-valued rather than a boolean, since
#' a page that's neither clearly job-signal-positive nor clearly says "no
#' openings" (e.g. a fetch error, a redirect to an unrelated page) shouldn't
#' be silently folded into either bucket.
#' @param page_text See the description.
#' @export
score_page_text_for_job_signal <- function(page_text) {
  if (is.na(page_text) || nchar(trimws(page_text)) == 0) {
    return("inconclusive")
  }

  negative_signal <- grepl(
    "no (open |current )?(job|position|vacan)|no openings|not currently (hiring|accepting)|there are (currently )?no",
    page_text, ignore.case = TRUE
  )

  positive_hits <- lengths(regmatches(
    page_text,
    gregexpr("apply now|view details|job title|posted:|closing date|date posted|JobID", page_text, ignore.case = TRUE)
  ))

  if (negative_signal && positive_hits < 3) {
    "looks_genuinely_empty"
  } else if (positive_hits >= 3) {
    "likely_broken"
  } else {
    "inconclusive"
  }
}

#' `STANDING_POOL_PATTERN`
#'
#' verdict: from score_page_text_for_job_signal() ("likely_broken" /
#'   "looks_genuinely_empty" / "inconclusive"), or "confirmed_broken" /
#'   "no_url_available" set upstream in corroborate_drift.R.
#' llm_titles: from llm_titles_from_page_text() -- character(0) when the LLM
#'   step was skipped (no key) or found nothing.
#' 
#' Returns list(verdict, note):
#'   - LLM found real postings and we weren't already at confirmed_broken ->
#'     promote to "likely_broken" and say what it found (the scraper returned
#'     ~0 but these are demonstrably on the page).
#'   - LLM found nothing AND the text signal was only "inconclusive" ->
#'     downgrade to "looks_genuinely_empty" (conservative: both weak signals
#'     now agree there's nothing there).
#'   - otherwise: unchanged.
#' Standing substitute-recruiting lists ("We are always looking for
#' substitutes: bus driver substitutes, kitchen substitutes, ...") read like
#' postings to an LLM, but most boards that run them aren't posting jobs there.
#' Lone Rock (MT issue #13, 2026-11-10): its two real openings came down, the
#' scraper rightly returned 0, and the LLM's six substitute lines made it
#' "likely_broken". Whether a substitute line is a missed posting depends on
#' the source: 127 of 183 MT and 49 of 59 WY direct sources list substitute
#' postings themselves (last 8 weeks, 2026-11-10). So the lines are set aside
#' only when the source's own recent titles have none.
#' @export
STANDING_POOL_PATTERN <- "(?i)\\bsub(stitute)?s?\\b"

#' Drop standing pool titles
#'
#' llm_titles minus substitute lines when history_titles (the source's own
#' recent titles) has no substitute title; NULL history keeps everything.
#' @param llm_titles See the description.
#' @param history_titles See the description.
#' @export
drop_standing_pool_titles <- function(llm_titles, history_titles = NULL) {
  if (is.null(history_titles) || !length(llm_titles)) return(llm_titles)
  if (any(grepl(STANDING_POOL_PATTERN, history_titles, perl = TRUE))) return(llm_titles)
  llm_titles[!grepl(STANDING_POOL_PATTERN, llm_titles, perl = TRUE)]
}

#' Recent source titles
#'
#' The titles one source returned over its last `weeks` archived runs (K-12:
#' Archivek12_Data District/title; "Higher Ed": Archived_HE_Data Institution/Title), or
#' NULL when there are no archives to read.
#' @param name See the description.
#' @param type See the description.
#' @param weeks See the description.
#' @param k12_dir See the description.
#' @param he_dir See the description.
#' @export
recent_source_titles <- function(name, type = "K-12", weeks = 8, k12_dir = "Archivek12_Data", he_dir = "Archived_HE_Data") {
  he <- type %in% c("Higher Ed", "HE")   # check_drift.R labels HE sources "Higher Ed"
  files <- utils::tail(sort(list.files(if (he) he_dir else k12_dir,
                                       pattern = if (he) "^hedata_\\d{4}-\\d{2}-\\d{2}\\.xlsx$" else "^combined_\\d{4}-\\d{2}-\\d{2}\\.csv$",
                                       full.names = TRUE)), weeks)
  if (!length(files)) return(NULL)
  unlist(lapply(files, function(f) tryCatch({
    x <- if (he) as.data.frame(readxl::read_excel(f, col_types = "text")) else utils::read.csv(f, colClasses = "character")
    if (he) x$Title[x$Institution %in% name] else x$title[x$District %in% name]
  }, error = function(e) character(0))))
}

#' Combine verdict with llm
#' @param verdict See the description.
#' @param llm_titles See the description.
#' @param history_titles See the description.
#' @export
combine_verdict_with_llm <- function(verdict, llm_titles, history_titles = NULL) {
  kept <- drop_standing_pool_titles(llm_titles, history_titles)
  pooled <- length(llm_titles) - length(kept)
  llm_titles <- kept
  n <- length(llm_titles)

  if (n > 0 && !identical(verdict, "confirmed_broken")) {
    shown <- paste(utils::head(llm_titles, 8L), collapse = "; ")
    return(list(
      verdict = "likely_broken",
      note = paste0("an LLM read ", n, " posting(s) off the live page: ",
                    shown, if (n > 8L) ", ..." else "")
    ))
  }

  if (n == 0 && identical(verdict, "inconclusive")) {
    return(list(verdict = "looks_genuinely_empty",
                note = if (pooled > 0) paste0("an LLM read only a standing substitute list (", pooled,
                                              " line(s)) off the live page, and this source never posts substitute jobs")
                       else "an LLM read no postings off the live page either"))
  }

  list(verdict = verdict, note = NA_character_)
}

#' `AUTOFIX_LABEL`
#' @export
AUTOFIX_LABEL <- "scraper-autofix"

#' `AUTOFIX_ELIGIBLE_VERDICTS`
#' @export
AUTOFIX_ELIGIBLE_VERDICTS <- "likely_broken"

#' `AUTOFIX_MARKER_RE`
#' @export
AUTOFIX_MARKER_RE <- "<!--\\s*autofix-source:\\s*(.+?)\\s*-->"

#' Autofix issue title
#' @param name See the description.
#' @export
autofix_issue_title <- function(name) paste0("Scraper auto-fix: ", name)

#' `AUTOFIX_ENGINES`
#'
#' Who works a new auto-fix issue, from the AUTOFIX_ENGINE repo variable:
#' "claude" (default) dispatches .github/workflows/claude-autofix.yml,
#' "copilot" assigns the issue to the Copilot coding agent via COPILOT_TOKEN,
#' "none" just files it. Both agents on one issue would race to open
#' competing PRs, so it's one or the other. A typo fails the step loudly
#' rather than quietly filing issues nobody works.
#' @export
AUTOFIX_ENGINES <- c("claude", "copilot", "none")

#' `AUTOFIX_CLAUDE_WORKFLOW`
#' @export
AUTOFIX_CLAUDE_WORKFLOW <- "claude-autofix.yml"

#' Autofix engine
#' @param value See the description.
#' @export
autofix_engine <- function(value) {
  v <- tolower(trimws(if (length(value) == 0 || is.na(value)) "" else value))
  if (!nzchar(v)) return("claude")
  if (!v %in% AUTOFIX_ENGINES) {
    stop("AUTOFIX_ENGINE must be one of ", paste(AUTOFIX_ENGINES, collapse = ", "), ", not '", value, "'")
  }
  v
}

#' `AUTOFIX_ESCALATION_MARKER`
#'
#' Claude as the fallback engine (2026-11-11). With AUTOFIX_ENGINE = "copilot",
#' an issue Copilot hasn't fixed by a later weekly run goes to the Claude
#' workflow once: still flagged, at least AUTOFIX_ESCALATION_MIN_DAYS old, not
#' already handed over (the marker in a comment), no open PR with changes
#' waiting for review, and no merged fix (merged but still flagged is a human
#' problem). A week's wait lets Copilot finish, and lets a board that was
#' briefly empty recover on its own, before a second agent is paid for.
#' `prs` is every PR whose body says "Fixes #<issue>": columns state
#' ("OPEN"/"CLOSED"/"MERGED") and changed_files. Returns the decision as a
#' word, "escalate" or the reason not to.
#' @export
AUTOFIX_ESCALATION_MARKER <- "<!-- autofix-escalated: claude -->"

#' `AUTOFIX_ESCALATION_MIN_DAYS`
#' @export
AUTOFIX_ESCALATION_MIN_DAYS <- 5

#' Autofix escalation decision
#' @param issue_created See the description.
#' @param today See the description.
#' @param comment_bodies See the description.
#' @param prs See the description.
#' @export
autofix_escalation_decision <- function(issue_created, today, comment_bodies, prs) {
  age <- as.numeric(as.Date(today) - as.Date(substr(issue_created, 1, 10)))
  if (is.na(age) || age < AUTOFIX_ESCALATION_MIN_DAYS) return("too_new")
  if (any(grepl(AUTOFIX_ESCALATION_MARKER, comment_bodies, fixed = TRUE))) return("already_escalated")
  if (NROW(prs) > 0) {
    if (any(prs$state == "MERGED")) return("fix_merged")
    if (any(prs$state == "OPEN" & prs$changed_files > 0)) return("pr_awaiting_review")
  }
  "escalate"
}

#' Autofix linked prs
#'
#' PRs (from `gh pr list --json number,state,changedFiles,body`) that say
#' "Fixes/Closes/Resolves #<issue>", as autofix_escalation_decision() wants them.
#' @param prs See the description.
#' @param issue_number See the description.
#' @export
autofix_linked_prs <- function(prs, issue_number) {
  empty <- data.frame(number = integer(0), state = character(0), changed_files = integer(0))
  if (NROW(prs) == 0) return(empty)
  pat <- sprintf("(?i)\\b(fix(es|ed)?|close[sd]?|resolve[sd]?)\\s+#%s\\b", issue_number)
  hit <- grepl(pat, ifelse(is.na(prs$body), "", prs$body), perl = TRUE)
  if (!any(hit)) return(empty)
  data.frame(number = prs$number[hit], state = prs$state[hit], changed_files = prs$changedFiles[hit])
}

#' Parse autofix marker
#' @param body See the description.
#' @export
parse_autofix_marker <- function(body) {
  if (length(body) == 0 || is.na(body)) return(NA_character_)
  m <- regmatches(body, regexec(AUTOFIX_MARKER_RE, body, perl = TRUE))[[1]]
  if (length(m) < 2) NA_character_ else m[2]
}

#' `AUTOFIX_PAGE_TEXT_MAX_CHARS`
#' @export
AUTOFIX_PAGE_TEXT_MAX_CHARS <- 20000

#' `AUTOFIX_DIFF_MAX_LINES`
#' @export
AUTOFIX_DIFF_MAX_LINES <- 200

#' Scraper reads inner text
#'
#' Whether fetch_fn's parser consumes document.body.innerText -- the form the
#' drift check captured, so the capture can be the new fixture verbatim.
#' @param fetch_fn See the description.
#' @param scraper_lines See the description.
#' @export
scraper_reads_inner_text <- function(fetch_fn, scraper_lines) {
  if (is.na(fetch_fn)) return(FALSE)
  start <- grep(sprintf("^%s <- function", fetch_fn), scraper_lines)
  if (length(start) == 0) return(FALSE)
  later <- grep("^[A-Za-z_.][A-Za-z0-9_.]* <- ", scraper_lines)
  end <- c(later[later > start[1]], length(scraper_lines) + 1)[1] - 1
  any(grepl("document.body.innerText", scraper_lines[start[1]:end], fixed = TRUE))
}

#' Text diff
#'
#' Unified diff of two texts after trimming lines and dropping blanks
#' (innerText and html_text2() disagree on blank lines, which is noise).
#' Uses the system `diff`; returns character(0) when identical or absent.
#' @param old_text See the description.
#' @param new_text See the description.
#' @param old_label See the description.
#' @param new_label See the description.
#' @export
text_diff <- function(old_text, new_text, old_label = "old", new_label = "new") {
  norm <- function(x) { x <- trimws(unlist(strsplit(x, "\n", fixed = TRUE))); x[nzchar(x)] }
  if (!nzchar(Sys.which("diff"))) return(character(0))
  f_old <- tempfile(); f_new <- tempfile()
  on.exit(unlink(c(f_old, f_new)))
  writeLines(norm(old_text), f_old, useBytes = TRUE)
  writeLines(norm(new_text), f_new, useBytes = TRUE)
  out <- suppressWarnings(system2("diff", c("-u", "--label", shQuote(old_label), "--label", shQuote(new_label),
                                            shQuote(f_old), shQuote(f_new)), stdout = TRUE, stderr = FALSE))
  as.character(out)
}

#' Parse autofix expected titles
#'
#' The LLM-read titles build_autofix_issue_body() listed -- the live check's
#' (hint-quality) expectation for what the fixed scraper should now return.
#' @param body See the description.
#' @export
parse_autofix_expected_titles <- function(body) {
  lines <- strsplit(body, "\n", fixed = TRUE)[[1]]
  start <- grep("^An LLM read these postings", lines)
  # First heading after the list -- "### What CI saw" (whose diff and page
  # text can hold "- " lines) or "### Task".
  end <- grep("^### ", lines)
  end <- end[end > start[1]]
  if (length(start) == 0 || length(end) == 0) return(character(0))
  block <- lines[(start[1] + 1):(end[1] - 1)]
  sub("^- ", "", block[startsWith(block, "- ")])
}

#' `TITLE_ABBREVIATION_PATTERN`
#'
#' Abbreviations whose trailing period isn't the end of a sentence.
#' @export
TITLE_ABBREVIATION_PATTERN <- paste0(
  "(?i)\\b(Jr|Sr|Asst|Assist|Assoc|Coord|Dept|Ed|Sp|St|Supt|Admin|Tech|Spec|Mgr|Dir|Fed|Govt|",
  "Mr|Mrs|Ms|Dr|No|Vo|Ag|Phys|Res|Prof|Approx|Hrs|P\\.E|Ft)\\."
)

#' Lint title shape
#' @param title See the description.
#' @param max_chars See the description.
#' @export
lint_title_shape <- function(title, max_chars = 200) {
  t <- as.character(title)
  t[is.na(t)] <- ""
  t <- trimws(t)
  no_abbrev <- gsub(TITLE_ABBREVIATION_PATTERN, "", t, perl = TRUE)
  level_words <- "(?i)\\b(high school|middle school|junior high|jr\\.? high|elementary|hs|ms|head|assistant|asst\\.?|varsity|junior varsity|jv)\\b"
  rules <- list(
    "blank title" = !nzchar(t),
    "no job named" = grepl("^(other|none|n/?a|tbd|tba|positions?|jobs?|vacanc(y|ies)|openings?|see (job )?description|untitled)$",
                           t, ignore.case = TRUE),
    "level and rank only, no role" = nzchar(t) &
      grepl("(?i)\\b(head|assistant|asst)\\b", t, perl = TRUE) &
      !grepl("[A-Za-z0-9]", gsub(level_words, "", t, perl = TRUE)),
    "schedule or detail sentence" = grepl("\\.\\s*$", no_abbrev) &
      (grepl("[A-Za-z]\\.\\s+[A-Za-z]", no_abbrev) |
         grepl("\\b(must|required|requires|hiring|open until|hours?|hrs|per (day|hour|week)|full[- ]time|part[- ]time|year[- ]round|apply|mon|fri)\\b",
               no_abbrev, ignore.case = TRUE)),
    "field label" = grepl("^(summary|description|job description|job summary|position summary|position|job title|title|location|department|salary|pay|wage|posted|closing date|status|requirements|qualifications)\\s*:",
                          t, ignore.case = TRUE),
    "markup left in" = grepl("StartFragment|EndFragment|&[a-z]+;|&#[0-9]+;|<[a-z/][^>]*>|\u200b", t, ignore.case = TRUE, perl = TRUE),
    "file name" = grepl("[A-Za-z]{2,}_[A-Za-z]{2,}|\\s-\\sCopy$", t),
    "too long" = nchar(t) > max_chars
  )
  reasons <- do.call(cbind, lapply(names(rules), function(r) ifelse(rules[[r]], r, NA_character_)))
  if (length(t) == 0) return(character(0))
  out <- apply(matrix(reasons, nrow = length(t)), 1, function(r) paste(r[!is.na(r)], collapse = "; "))
  out[!nzchar(out)] <- NA_character_
  out
}

#' Title pay unrecorded
#'
#' Titles that state pay ("Secretary - $14 per hour", "Substitute Teachers
#' $150/day") on postings whose Posted Pay is blank: no scraper reads pay from
#' the title, so the posting's own pay isn't shown. Uses extract_posted_pay()
#' (edjobs), which ignores amounts that aren't pay ("$10,000
#' Hiring Incentive").
#' @param title See the description.
#' @param pay_min See the description.
#' @export
title_pay_unrecorded <- function(title, pay_min) {
  t <- as.character(title)
  has_dollar <- !is.na(t) & grepl("\\$\\s*[0-9]", t)
  out <- rep(FALSE, length(t))
  if (!any(has_dollar & is.na(pay_min))) return(out)
  idx <- which(has_dollar & is.na(pay_min))
  out[idx] <- !is.na(extract_posted_pay(t[idx])$Pay_Min)
  out
}

#' Build title lint report markdown
#'
#' hits: data.frame(source, type, title, reason). At most max_per_source
#' titles are listed per source.
#' @param hits See the description.
#' @param date See the description.
#' @param max_per_source See the description.
#' @export
build_title_lint_report_markdown <- function(hits, date = Sys.Date(), max_per_source = 5) {
  if (nrow(hits) == 0) return(character(0))
  key <- paste(hits$type, hits$source, sep = "\r")
  lines <- unlist(lapply(unique(key), function(k) {
    h <- hits[key == k, , drop = FALSE]
    n_postings <- nrow(h)
    h <- h[!duplicated(h$title), , drop = FALSE]
    shown <- utils::head(h, max_per_source)
    c(sprintf("- **%s** (%s), %d posting(s):", h$source[1], h$type[1], n_postings),
      sprintf("  - \"%s\" -- %s", substr(gsub("[\r\n]+", " ", shown$title), 1, 120), shown$reason),
      if (nrow(h) > max_per_source) sprintf("  - ...and %d more", nrow(h) - max_per_source))
  }))
  c(
    paste0("### Titles that don't look like job titles (", date, ")"),
    "",
    paste("This week's postings whose title looks like something other than a job title: page text, a",
          "summary, a heading, or pay the Posted Pay column doesn't carry. Usually the scraper is reading",
          "the wrong element. These titles are classified as they are, so they can land in the wrong bucket."),
    "",
    lines,
    ""
  )
}

#' Other share counts
#'
#' One row per source: postings (n) and how many the classifier puts in Other
#' (other).
#' @param names See the description.
#' @param is_other See the description.
#' @export
other_share_counts <- function(names, is_other) {
  out <- pay_coverage_counts(names, is_other)
  names(out)[names(out) == "with_pay"] <- "other"
  out
}

#' Flag other share jump
#'
#' current: other_share_counts() for this run. history: a list of the same,
#' one per recent archived week (the caller picks the window). A source is
#' flagged when it has at least min_weeks weeks with min_postings or more
#' postings, and this run has min_postings or more postings, at least
#' min_other of them in Other, and an Other share at least `jump` above its
#' average and at least `ratio` times it. Replayed over every archived week
#' (8-week window, 2026-11-05) these defaults flag 2 MT and 2 WY source-weeks,
#' all real scraper problems (Florence-Carlton schedule lines, Albany section
#' headings); stricter settings flag none of them.
#' @param current See the description.
#' @param history See the description.
#' @param min_weeks See the description.
#' @param min_postings See the description.
#' @param min_other See the description.
#' @param jump See the description.
#' @param ratio See the description.
#' @export
flag_other_share_jump <- function(current, history, min_weeks = 2, min_postings = 3, min_other = 2,
                                  jump = 0.15, ratio = 1.5) {
  empty <- data.frame(name = character(0), mean_share = numeric(0), n_weeks = integer(0),
                      n = integer(0), other = integer(0), stringsAsFactors = FALSE)
  weeks <- do.call(rbind, history)
  if (is.null(weeks) || nrow(weeks) == 0 || nrow(current) == 0) return(empty)
  weeks <- weeks[weeks$n >= min_postings, , drop = FALSE]
  if (nrow(weeks) == 0) return(empty)
  weeks$share <- weeks$other / weeks$n
  baseline <- data.frame(name = sort(unique(weeks$name)), stringsAsFactors = FALSE)
  baseline$n_weeks <- as.integer(table(weeks$name)[baseline$name])
  baseline$mean_share <- as.numeric(tapply(weeks$share, weeks$name, mean)[baseline$name])
  merged <- merge(baseline, current, by = "name")
  share <- merged$other / merged$n
  flagged <- merged[merged$n_weeks >= min_weeks & merged$n >= min_postings & merged$other >= min_other &
                      share >= merged$mean_share + jump & share >= ratio * merged$mean_share, , drop = FALSE]
  if (nrow(flagged) == 0) return(empty)
  flagged[order(-(flagged$other / flagged$n)), c("name", "mean_share", "n_weeks", "n", "other")]
}

#' Build other share report markdown
#'
#' flagged: flag_other_share_jump() rows plus type and examples (a few of the
#' source's Other titles this run).
#' @param flagged See the description.
#' @param date See the description.
#' @export
build_other_share_report_markdown <- function(flagged, date = Sys.Date()) {
  if (nrow(flagged) == 0) return(character(0))
  c(
    paste0("### More postings than usual land in Other (", date, ")"),
    "",
    paste("These sources' share of postings the classifier can't place jumped against their own recent",
          "weeks. Usually the scraper started picking up page text instead of titles; otherwise the",
          "classifier needs a rule for a new kind of title."),
    "",
    sprintf("- **%s** (%s): usually %.0f%% Other, now %d of %d -- e.g. %s",
            flagged$name, flagged$type, 100 * flagged$mean_share, flagged$other, flagged$n, flagged$examples),
    ""
  )
}

#' K12 keyword conflict rules
#'
#' Keyword conflicts (classification safeguards item 9)
#' A title carrying a word that names one bucket while the classifier put it
#' in another: the hand audit of 2026-11-01, as rules. Each rule is a signal
#' (regex, case-insensitive), the buckets that are fine for it, and an
#' optional `unless` regex for titles where the word is legitimately
#' secondary (a nutrition-services secretary, a "Secretary/Para" dual role).
#' Non-blocking: listed in the drift issue so a person decides whether the
#' classifier needs a rule. Tuned 2026-11-08 against every archived title
#' since 2025-10-01 in both states, where only real misroutes remained.
#' @export
k12_keyword_conflict_rules <- list(
  list(id = "coach aide outside Athletics",
       signal = "\\bAides?\\b.*(Football|Basketball|Volleyball|Wrestling|Soccer|\\bSwim|\\bGolf\\b|\\bTrack\\b|Cross Country|Baseball|Softball|Tennis)|(Football|Basketball|Volleyball|Wrestling|Soccer|\\bSwim|\\bGolf\\b|\\bTrack\\b|Cross Country|Baseball|Softball|Tennis).*\\bAides?\\b",
       ok = c("Athletics", "Substitute", "Activities/Extracurricular")),
  list(id = "food-service word outside Food Services",
       signal = "\\b(Kitchen|Cooks?|Nutrition|Cafeteria|Food Services?|Dishwasher)\\b",
       ok = c("Food Services", "Substitute"),
       unless = "Teacher|Instructor|Culinary|Director|Supervisor|Manager|Coordinator|Secretary|Bookkeeper|Clerk|Administrative Assistant|Driver|Warehouse|Transport|Custodian|Custodial|/"),
  list(id = "bus aide or monitor outside Transportation",
       signal = "\\bBus\\b.*\\b(Aides?|Monitors?|Para\\w*)\\b|\\b(Aides?|Monitors?)\\b.*\\bBus\\b",
       ok = c("Transportation", "Substitute"), unless = "/|\\bor\\b|&|\\band\\b"),
  list(id = "bus driver outside Transportation", signal = "\\bBus Drivers?\\b",
       ok = c("Transportation", "Substitute"), unless = "/|\\bor\\b|&|\\band\\b|Driver'?s?\\s*Ed"),
  list(id = "custodian outside Custodial/Maintenance", signal = "Custodian|Custodial|Janitor",
       ok = c("Custodial/Maintenance", "Substitute"), unless = "/|\\bor\\b|&|\\band\\b|Secretary|Supervisor|Director|Manager"),
  list(id = "teacher title in Paraprofessional", signal = "\\bTeacher\\b",
       ok = setdiff(c("Substitute", "Paraprofessional", "Activities/Extracurricular", "Transportation", "Food Services",
                      "Athletics", "Support Services", "Staff", "Custodial/Maintenance", "Administration",
                      "Part-Time Teacher", "Teacher", "Child Care", "Other"), "Paraprofessional"),
       unless = "Assistant|Aides?\\b|Associate|Para|Helper"),
  list(id = "principal or superintendent outside Administration", signal = "\\b(Principal|Superintendent)\\b",
       ok = c("Administration", "Substitute"), unless = "Secretary|Assistant to|Clerk|Administrative Assistant|Office|/"),
  list(id = "secretary or clerk outside Staff", signal = "\\b(Secretary|Clerk)\\b",
       ok = c("Staff", "Substitute"), unless = "/")
)

#' He keyword conflict rules
#' @export
he_keyword_conflict_rules <- list(
  list(id = "custodian outside Maintenance & Service", signal = "Custodian|Custodial|Janitor",
       ok = c("Maintenance & Service", "Student Positions", "Temporary Position"),
       unless = "Supervisor|Director|Manager|Dish|Cook|Kitchen"),
  list(id = "postdoc outside Research & Science", signal = "Post-?doc", ok = "Research & Science"),
  list(id = "adjunct outside Adjunct/Part-Time Faculty", signal = "\\bAdjunct\\b", ok = "Adjunct/Part-Time Faculty")
)

#' Find keyword conflicts
#'
#' The first rule each title trips, or NA.
#' @param title See the description.
#' @param bucket See the description.
#' @param rules See the description.
#' @export
find_keyword_conflicts <- function(title, bucket, rules) {
  title <- as.character(title)
  out <- rep(NA_character_, length(title))
  for (r in rules) {
    hit <- grepl(r$signal, title, ignore.case = TRUE, perl = TRUE) & !(bucket %in% r$ok)
    if (!is.null(r$unless)) hit <- hit & !grepl(r$unless, title, ignore.case = TRUE, perl = TRUE)
    out[hit & is.na(out)] <- r$id
  }
  out
}

#' Build keyword conflict report markdown
#'
#' hits: data.frame(source, type, title, bucket, rule).
#' @param hits See the description.
#' @param date See the description.
#' @param max_rows See the description.
#' @export
build_keyword_conflict_report_markdown <- function(hits, date = Sys.Date(), max_rows = 25) {
  if (nrow(hits) == 0) return(character(0))
  h <- hits[!duplicated(paste(hits$type, hits$title)), , drop = FALSE]
  shown <- utils::head(h, max_rows)
  c(
    paste0("### Titles whose words point to a different bucket (", date, ")"),
    "",
    paste("This week's postings whose title names one kind of job while the classifier put it in another",
          "(a custodian outside Custodial, a bus aide outside Transportation). Either the classifier needs",
          "a rule, or the title is a real exception; add it to the golden set either way."),
    "",
    sprintf("- \"%s\" (%s, %s): %s -- %s", substr(gsub("[\r\n]+", " ", shown$title), 1, 120), shown$source,
            shown$type, shown$bucket, shown$rule),
    if (nrow(h) > max_rows) sprintf("- ...and %d more", nrow(h) - max_rows),
    ""
  )
}

# ---------------------------------------------------------------------------
# Drift checks that differed between MT and WY until phase 4 (2026-11-11):
# the statewide-feed labels come from state_config(feed = ), and flag_drift()'s
# floor is an argument.
# ---------------------------------------------------------------------------

# One field of the session's statewide feed (state_config(feed = )).
state_feed <- function(field, default = NULL) {
  v <- current_state_config()$feed[[field]]
  if (is.null(v)) default else v
}

#' Weekly posting counts per source, from archive snapshots
#'
#' `archive_snapshots`: a named list (run date -> data frame), one per archived run (a plain argument,
#' not a directory scan, so it is testable on in-memory frames). Returns one row per source: `name`,
#' `mean_count`, `n_weeks`.
#' @param archive_snapshots Named list of run date -> data frame.
#' @param name_col The column naming the source.
#' @param valid_from Only snapshots from this date on count (the first run of
#'   the pipeline as it now is); defaults to the session's
#'   `state_config(drift_baseline_from = )`, else every snapshot.
#' @export
build_historical_counts <- function(archive_snapshots, name_col,
                                    valid_from = current_state_config()$drift_baseline_from) {
  if (is.null(valid_from)) valid_from <- as.Date("1900-01-01")
  # archive_snapshots: named list of data.frames, names are "YYYY-MM-DD"
  # dates, each data.frame has a `name_col` column of source names (one row
  # per posting, same shape as combinedclean.csv/hedata.xlsx).
  valid_dates <- names(archive_snapshots)[as.Date(names(archive_snapshots)) >= as.Date(valid_from)]

  if (length(valid_dates) == 0) {
    return(data.frame(name = character(0), n_weeks = integer(0), mean_count = numeric(0)))
  }

  counts_by_week <- lapply(valid_dates, function(d) {
    df <- archive_snapshots[[d]]
    as.data.frame(table(df[[name_col]]), stringsAsFactors = FALSE)
  })

  all_counts <- do.call(rbind, counts_by_week)
  names(all_counts) <- c("name", "count")
  all_counts$count <- as.numeric(all_counts$count)

  stats::aggregate(count ~ name, data = all_counts, FUN = function(x) c(n = length(x), mean = mean(x))) -> agg
  data.frame(
    name = agg$name,
    n_weeks = agg$count[, "n"],
    mean_count = agg$count[, "mean"],
    stringsAsFactors = FALSE
  )
}

#' Sources whose count collapsed against their own history
#'
#' `baseline`: `build_historical_counts()`. A source needs `min_weeks` weeks of history and an average
#' above 0 and at least `min_mean_count` before it can be flagged: a district averaging 1-2 postings that
#' now has 0 is ordinary churn, not the silent-parser-failure signature this check exists to catch. Flags a
#' source when this week's count is at most `drop_threshold` times its average. WY uses the default
#' (3); MT passes 0 (any average above 0) until it decides.
#' @param current_counts This run's counts: `name`, `count`.
#' @param baseline `build_historical_counts()` output.
#' @param min_weeks Weeks of history needed.
#' @param min_mean_count Smallest historical average that can be flagged.
#' @param drop_threshold Flag at or below this share of the average.
#' @export
flag_drift <- function(current_counts, baseline, min_weeks = 2, min_mean_count = 3,
                       drop_threshold = 0.2) {
  merged <- merge(baseline, current_counts, by = "name", all.x = TRUE)
  merged$count[is.na(merged$count)] <- 0

  eligible <- merged[merged$n_weeks >= min_weeks & merged$mean_count > 0 & merged$mean_count >= min_mean_count, ]
  flagged <- eligible[eligible$count <= eligible$mean_count * drop_threshold, ]
  flagged[order(-flagged$mean_count), c("name", "mean_count", "n_weeks", "count")]
}

#' The statewide feed's part in this run, for the drift report
#'
#' How many vacancies the statewide feed listed, how many it supplied at districts we scrape directly
#' (openings their own board doesn't list), and the discrepancy report's unmatched rows. The feed's labels
#' and the discrepancy file default to the session's `state_config(feed = )`. Returns a list:
#' total_feed_vacancies, feed_name, fallback_districts, total_fallback_vacancies, discrepancies, markdown.
#' @param combined_df This run's combined K-12 rows.
#' @param registry The K-12 registry.
#' @param discrepancy_report The discrepancy report, or NULL to read `discrepancy_path`.
#' @param scrape_log scrape_log.csv rows.
#' @param feed_label The feed's Posting_Source label.
#' @param feed_short_name The feed's short name, e.g. "OPI".
#' @param discrepancy_path The discrepancy report file.
#' @export
summarize_central_safety_net <- function(
  combined_df = NULL,
  registry = NULL,
  discrepancy_report = NULL,
  scrape_log = NULL,
  feed_label = state_feed("label", "statewide feed"),
  feed_short_name = state_feed("short_name", "statewide feed"),
  discrepancy_path = state_feed("discrepancy_file")
) {
  # 1. Total vacancies on the statewide feed
  feed_total <- 0L
  if (!is.null(scrape_log) && nrow(scrape_log) > 0 && "source" %in% names(scrape_log)) {
    feed_log <- scrape_log[grepl(feed_short_name, scrape_log$source, ignore.case = TRUE) |
                           grepl(feed_label, scrape_log$source, fixed = TRUE), , drop = FALSE]
    if (nrow(feed_log) > 0 && "n_rows" %in% names(feed_log)) {
      latest_rows <- feed_log$n_rows[nrow(feed_log)]
      if (!is.na(latest_rows) && is.numeric(latest_rows)) {
        feed_total <- as.integer(latest_rows)
      }
    }
  }

  if (feed_total == 0L && !is.null(combined_df) && nrow(combined_df) > 0) {
    if ("Posting_Source" %in% names(combined_df)) {
      feed_total <- sum(!is.na(combined_df$Posting_Source) & combined_df$Posting_Source == feed_label)
    } else {
      is_feed <- (!is.na(combined_df$url) & grepl(feed_short_name, combined_df$url, ignore.case = TRUE)) |
                 (!is.na(combined_df$posting_id) & grepl(feed_short_name, combined_df$posting_id, ignore.case = TRUE))
      feed_total <- sum(is_feed)
    }
  }

  # 2. Registered districts receiving fallback postings
  registered_districts <- if (!is.null(registry) && "District" %in% names(registry)) {
    unique(registry$District[!is.na(registry$District) & nzchar(registry$District)])
  } else {
    character(0)
  }

  fallback_districts <- list()
  if (!is.null(combined_df) && nrow(combined_df) > 0 && length(registered_districts) > 0) {
    is_feed_row <- if ("Posting_Source" %in% names(combined_df)) {
      !is.na(combined_df$Posting_Source) & combined_df$Posting_Source == feed_label
    } else {
      (!is.na(combined_df$url) & grepl(feed_short_name, combined_df$url, ignore.case = TRUE)) |
      (!is.na(combined_df$posting_id) & grepl(feed_short_name, combined_df$posting_id, ignore.case = TRUE))
    }
    feed_reg_rows <- combined_df[is_feed_row & combined_df$District %in% registered_districts, , drop = FALSE]
    if (nrow(feed_reg_rows) > 0) {
      tbl <- table(feed_reg_rows$District)
      fallback_districts <- as.list(as.integer(tbl))
      names(fallback_districts) <- names(tbl)
    }
  }

  # 3. Discrepancy report
  if (is.null(discrepancy_report) && !is.null(discrepancy_path) && file.exists(discrepancy_path)) {
    discrepancy_report <- tryCatch(
      utils::read.csv(discrepancy_path, stringsAsFactors = FALSE),
      error = function(e) NULL
    )
  }

  disc_unmatched <- if (!is.null(discrepancy_report) && nrow(discrepancy_report) > 0 && "Discrepancy_Type" %in% names(discrepancy_report)) {
    discrepancy_report[!grepl("ghost", discrepancy_report$Discrepancy_Type, ignore.case = TRUE), , drop = FALSE]
  } else {
    data.frame()
  }

  # If fallback_districts is empty but discrepancy_report had matching fallback districts
  if (length(fallback_districts) == 0 && nrow(disc_unmatched) > 0 && "District" %in% names(disc_unmatched)) {
    reg_disc <- disc_unmatched[disc_unmatched$District %in% registered_districts, , drop = FALSE]
    if (nrow(reg_disc) > 0) {
      tbl <- table(reg_disc$District)
      fallback_districts <- as.list(as.integer(tbl))
      names(fallback_districts) <- names(tbl)
    }
  }

  total_fallback_vacancies <- sum(unlist(fallback_districts), 0L)

  # 4. Generate Markdown
  md <- c(
    sprintf("### %s postings at districts we scrape directly", feed_short_name),
    "",
    paste("Openings the statewide feed lists for a district we scrape directly, counted because the district's own",
          "board doesn't list them: the district posts some jobs only on the feed, or our scrape misses them.",
          "A district with many is worth a look."),
    "",
    sprintf("- **Statewide Feed (%s)**: %d active vacancies captured.", feed_short_name, feed_total)
  )

  if (length(fallback_districts) == 0) {
    md <- c(md, sprintf("- **Counted from %s at directly scraped districts**: none (every %s opening at those districts is also on the district's own board).", feed_short_name, feed_short_name))
  } else {
    md <- c(
      md,
      sprintf("- **Counted from %s at directly scraped districts**: %d posting(s) at %d district(s), not on the district's own board:",
              feed_short_name, total_fallback_vacancies, length(fallback_districts))
    )
    for (d_name in names(fallback_districts)) {
      md <- c(md, sprintf("  - **%s**: %d", d_name, fallback_districts[[d_name]]))
    }
  }

  if (nrow(disc_unmatched) == 0) {
    md <- c(md, "- **Central Feed Discrepancies**: 0 unmatched vacancies detected for active direct scrapers.")
  } else {
    md <- c(
      md,
      sprintf("- **Central Feed Discrepancies**: %d unmatched vacancy/vacancies detected on central feed for direct-scraped districts (investigate potential ATS drift):", nrow(disc_unmatched))
    )
    shown_n <- min(nrow(disc_unmatched), 5L)
    for (i in seq_len(shown_n)) {
      r <- disc_unmatched[i, ]
      dp <- if ("Date_Posted" %in% names(r) && !is.na(r$Date_Posted) && nzchar(r$Date_Posted)) sprintf(" (posted %s)", r$Date_Posted) else ""
      t_title <- if ("Title" %in% names(r)) r$Title else ""
      d_dist <- if ("District" %in% names(r)) r$District else ""
      md <- c(md, sprintf("  - **%s**: \"%s\"%s", d_dist, t_title, dp))
    }
    if (nrow(disc_unmatched) > shown_n) {
      md <- c(md, sprintf("  - ... and %d more discrepancies", nrow(disc_unmatched) - shown_n))
    }
  }
  md <- c(md, "")

  list(
    total_feed_vacancies = feed_total,
    feed_name = feed_label,
    fallback_districts = fallback_districts,
    total_fallback_vacancies = total_fallback_vacancies,
    discrepancies = disc_unmatched,
    markdown = md
  )
}

#' The drift issue's markdown
#'
#' Flagged sources grouped by verdict, each with what the statewide feed says about it, then the feed
#' summary. `intro_explanation` replaces the default explanation (a state can add its own history).
#' @param actionable Flagged sources with their verdicts.
#' @param safety_net_summary `summarize_central_safety_net()` output.
#' @param date Report date.
#' @param feed_short_name The feed's short name.
#' @param intro_explanation Text to use instead of the default explanation.
#' @export
build_drift_report_markdown <- function(
  actionable,
  safety_net_summary = NULL,
  date = Sys.Date(),
  feed_short_name = state_feed("short_name", "statewide feed"),
  intro_explanation = NULL
) {
  if (nrow(actionable) == 0) return(character(0))

  default_intro <- if (is.null(intro_explanation)) {
    "A source is flagged when it has at least 2 weeks of history averaging real postings, and this week's count dropped to 20% or less of that average. These entries are corroborated against the actual live page but still need a human look."
  } else {
    intro_explanation
  }

  lines <- c(
    paste0("Automated drift check flagged ", nrow(actionable), " source(s) as of ", date, "."),
    "",
    default_intro,
    ""
  )

  for (verdict_group in c("confirmed_broken", "likely_broken", "inconclusive", "no_url_available")) {
    subset_rows <- actionable[actionable$verdict == verdict_group, ]
    if (nrow(subset_rows) == 0) next

    label <- switch(verdict_group,
      confirmed_broken = "### Confirmed broken -- the scraper itself errored this run (not just a low count)",
      likely_broken = "### Likely broken -- live page has real job-posting content, scraper reported little/none",
      inconclusive = "### Inconclusive -- drift detected, live page didn't clearly confirm either way",
      no_url_available = "### No URL on file -- couldn't corroborate automatically"
    )
    lines <- c(lines, label, "")
    for (i in seq_len(nrow(subset_rows))) {
      r <- subset_rows[i, ]
      url_part <- if (is.na(r$url)) "" else sprintf(" -- %s", r$url)
      error_part <- if (verdict_group == "confirmed_broken" && !is.na(r$error_message) && nzchar(r$error_message)) sprintf(" (%s)", r$error_message) else ""
      lines <- c(lines, sprintf("- **%s** (%s): averaged %.1f/week, now %d%s%s", r$name, r$type, r$mean_count, r$count, url_part, error_part))
      if ("llm_note" %in% names(r) && !is.na(r$llm_note) && nzchar(r$llm_note)) lines <- c(lines, sprintf("  - %s", r$llm_note))

      if (r$type == "K-12" && !is.null(safety_net_summary)) {
        sn_count <- if ("safety_net_vacancies" %in% names(r) && !is.na(r$safety_net_vacancies)) {
          as.integer(r$safety_net_vacancies)
        } else if (!is.null(safety_net_summary$fallback_districts[[r$name]])) {
          as.integer(safety_net_summary$fallback_districts[[r$name]])
        } else {
          0L
        }

        if (sn_count > 0) {
          lines <- c(lines, sprintf("  - **On %s**: %d posting%s for this district that its own board doesn't list (counted from %s)",
                                    feed_short_name, sn_count, if (sn_count == 1) "" else "s", feed_short_name))
        } else {
          lines <- c(lines, sprintf("  - **On %s**: nothing for this district beyond what its own board lists", feed_short_name))
        }
      }
    }
    lines <- c(lines, "")
  }

  if (!is.null(safety_net_summary) && !is.null(safety_net_summary$markdown)) {
    lines <- c(lines, safety_net_summary$markdown)
  }

  lines
}

#' Titles from a scraper's result
#'
#' Scrapers don't agree on case: some return `title`, most `Title`.
#' @param result A scraper's data frame.
#' @export
scraper_titles <- function(result) {
  col <- intersect(c("Title", "title"), names(result))
  if (length(col) == 0) character(0) else as.character(result[[col[1]]])
}

#' Live check of an auto-fix PR's scraper
#'
#' `result`: the scraper's data frame, or a condition if it errored. Fails only on the unambiguous cases
#' (an error, or zero rows: the symptom the issue was filed for). Fewer rows than the LLM read, or titles it
#' didn't match, are reported for the reviewer but don't fail: the LLM list is a hint.
#' @param source_name The source.
#' @param result The scraper's result or error.
#' @param expected_titles Titles the drift check's LLM read off the page.
#' @return list(pass, markdown).
#' @export
summarize_live_check <- function(source_name, result, expected_titles = character(0)) {
  header <- sprintf("### Live scraper check: %s", source_name)
  if (inherits(result, "condition")) {
    return(list(pass = FALSE, markdown = c(header, "", sprintf(":x: The scraper **errored** against the live site: `%s`", conditionMessage(result)))))
  }
  titles <- scraper_titles(result)
  if (length(titles) == 0) {
    return(list(pass = FALSE, markdown = c(header, "", ":x: The scraper still returns **0 postings** from the live site.")))
  }

  norm <- function(x) tolower(trimws(x))
  matched <- vapply(expected_titles, function(t) any(grepl(norm(t), norm(titles), fixed = TRUE) |
                                                     vapply(norm(titles), grepl, logical(1), x = norm(t), fixed = TRUE)),
                    logical(1))
  out <- c(header, "",
           sprintf(":white_check_mark: The scraper returned **%d posting(s)** from the live site:", length(titles)),
           "", paste0("- ", utils::head(titles, 25)),
           if (length(titles) > 25) sprintf("- ... and %d more", length(titles) - 25), "")
  if (length(expected_titles) > 0) {
    out <- c(out, sprintf("Matched %d of %d title(s) the drift check's LLM read off the page.", sum(matched), length(expected_titles)))
    if (any(!matched)) out <- c(out, "", "Not matched (check by hand -- the LLM list is a hint, not ground truth):", "",
                                paste0("- ", expected_titles[!matched]))
  }
  list(pass = TRUE, markdown = out)
}
