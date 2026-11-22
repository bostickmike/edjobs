# New vs still unresolved in the weekly drift issue (test-kit plan item 3; Mike chose option B, 2026-11-22).
#
# Every check appends to one report, posted as a comment on one rolling issue per state, and the same items came back
# every run with nothing to say which were new: MT's Ulm notice was flagged in 8 comments and Frontier's empty board
# in 9 before anyone acted (claude-notes/2026-11-21_step8-review/, section G). This puts a short block at the top of
# each run's comment: the items that weren't in the last report, and the items flagged in `min_runs` or more reports
# in a row, with how many and since when. Items a person has looked at and accepts go in the state's
# `drift_acknowledged.csv` and drop out of both lists (they stay in the full report below).

GLANCE_START <- "<!-- drift-glance-start -->"
GLANCE_END <- "<!-- drift-glance-end -->"

# Sections that repeat every run by design (feed totals, the safety-net summary), not flags.
DRIFT_SUMMARY_SECTIONS <- "postings at districts we scrape directly|Safety Net Status Summary"

#' The flagged items in a drift report
#'
#' Items are the top-level bullets that start with a bold name (`- **Name** ...`), keyed by their section heading
#' (the nearest `###`/`####` heading, without a trailing run date, or for a part with no heading its "Automated
#' <what> check flagged" line) and the name. A parenthesis right after the name
#' is part of the key when it names something ("(K-12)", "(Swan River School District #4)", "(Hill County)"), with
#' counts taken out ("(5)", ", 7 postings"), so the same item matches from run to run while its numbers change.
#' Summary sections (`DRIFT_SUMMARY_SECTIONS`) and the at-a-glance block itself are skipped.
#' @param md The report: a character vector of lines, or one string.
#' @return Data frame: `section`, `item`, `key`, one row per distinct key.
#' @export
drift_report_items <- function(md) {
  empty <- data.frame(section = character(0), item = character(0), key = character(0), stringsAsFactors = FALSE)
  if (!length(md)) return(empty)
  lines <- unlist(strsplit(paste(md, collapse = "\n"), "\n", fixed = TRUE))
  # Drop an earlier at-a-glance block.
  start <- which(lines == GLANCE_START); end <- which(lines == GLANCE_END)
  if (length(start) && length(end)) {
    drop <- unlist(Map(seq, start, vapply(start, function(s) min(c(end[end >= s], length(lines))), numeric(1))))
    lines <- lines[-drop]
  }
  section <- ""
  out <- list()
  for (l in lines) {
    if (grepl("^#{2,4} ", l)) {
      section <- trimws(sub("\\s*\\(\\d{4}-\\d{2}-\\d{2}\\)\\s*$", "", sub("^#+ ", "", l)))
      next
    }
    # A part with no heading opens with "Automated <what> check flagged ..." (salary value check) after a rule.
    if (grepl("^---\\s*$", l)) { section <- ""; next }
    if (!nzchar(section) && grepl("^Automated .+ flagged", l)) {
      section <- sub("^Automated (.+?)( check)? flagged.*$", "\\1 check", l)
      next
    }
    if (!grepl("^- \\*\\*[^*]+\\*\\*", l) || grepl(DRIFT_SUMMARY_SECTIONS, section)) next
    item <- sub("^- \\*\\*([^*]+)\\*\\*.*$", "\\1", l)
    rest <- sub("^- \\*\\*[^*]+\\*\\*", "", l)
    paren <- if (grepl("^ ?\\(([^)]*)\\)", rest)) sub("^ ?\\(([^)]*)\\).*$", "\\1", rest) else ""
    paren <- trimws(gsub(",?\\s*\\d+ (postings?|vacanc(y|ies))", "", paren))
    if (grepl("^[0-9.,% ]*$", paren)) paren <- ""
    name <- if (nzchar(paren)) paste0(item, " (", paren, ")") else item
    out[[length(out) + 1]] <- data.frame(section = section, item = name, key = paste(section, name, sep = " | "),
                                         stringsAsFactors = FALSE)
  }
  if (!length(out)) return(empty)
  r <- do.call(rbind, out)
  r[!duplicated(r$key), , drop = FALSE]
}

#' Which of this run's drift items are new and which keep coming back
#'
#' @param current This run's report (lines).
#' @param previous Earlier reports on the issue, oldest first: a list of `list(created = "<ISO time>", body = "...")`.
#' @param acknowledged Data frame with `Item` and optionally `Section` (empty or NA = any section): items a person has
#'   looked at; matched on the item name as [drift_report_items()] writes it.
#' @param min_runs An item is "still unresolved" once it is in this many reports in a row, this one included.
#' @return Data frame: `section`, `item`, `runs` (reports in a row, this one included), `since` (date of the earliest of
#'   those), `acknowledged`, `status` ("new", "unresolved" or "repeat").
#' @export
classify_drift_repeats <- function(current, previous = list(), acknowledged = NULL, min_runs = 3) {
  cur <- drift_report_items(current)
  if (!nrow(cur)) return(cbind(cur[, c("section", "item")], runs = integer(0), since = character(0),
                               acknowledged = logical(0), status = character(0)))
  prev_keys <- lapply(previous, function(p) drift_report_items(p$body)$key)
  prev_dates <- vapply(previous, function(p) substr(as.character(p$created %||% NA_character_), 1, 10), character(1))
  runs <- integer(nrow(cur)); since <- rep(NA_character_, nrow(cur))
  for (i in seq_len(nrow(cur))) {
    n <- 0L
    for (j in rev(seq_along(prev_keys))) { if (cur$key[i] %in% prev_keys[[j]]) n <- n + 1L else break }
    runs[i] <- n + 1L
    if (n > 0) since[i] <- prev_dates[length(prev_keys) - n + 1L]
  }
  ack <- rep(FALSE, nrow(cur))
  if (!is.null(acknowledged) && nrow(acknowledged)) {
    sec <- if ("Section" %in% names(acknowledged)) acknowledged$Section else rep(NA_character_, nrow(acknowledged))
    for (k in seq_len(nrow(acknowledged))) {
      any_sec <- is.na(sec[k]) || !nzchar(sec[k])
      ack <- ack | (cur$item == acknowledged$Item[k] & (any_sec | cur$section == sec[k]))
    }
  }
  status <- ifelse(runs == 1L, "new", ifelse(runs >= min_runs, "unresolved", "repeat"))
  data.frame(section = cur$section, item = cur$item, runs = runs, since = since, acknowledged = ack,
             status = status, stringsAsFactors = FALSE)
}

#' The at-a-glance block for the top of a drift comment
#'
#' @param repeats [classify_drift_repeats()] output.
#' @param min_runs As there.
#' @param ack_file The acknowledgement file named in the text.
#' @param max_list Most items listed under each heading.
#' @return Markdown lines, wrapped in the `GLANCE_START`/`GLANCE_END` markers; none when there are no items.
#' @export
build_drift_glance_markdown <- function(repeats, min_runs = 3, ack_file = "drift_acknowledged.csv", max_list = 40) {
  if (!nrow(repeats)) return(character(0))
  shown <- repeats[!repeats$acknowledged, , drop = FALSE]
  new <- shown[shown$status == "new", , drop = FALSE]
  unres <- shown[shown$status == "unresolved", , drop = FALSE]
  unres <- unres[order(-unres$runs, unres$section, unres$item), , drop = FALSE]
  n_rep <- sum(shown$status == "repeat"); n_ack <- sum(repeats$acknowledged)
  bullets <- function(d, f) {
    l <- f(d)
    if (length(l) > max_list) c(utils::head(l, max_list), sprintf("- ... and %d more (full report below)", length(l) - max_list)) else l
  }
  c(GLANCE_START,
    "### This run at a glance",
    "",
    sprintf("%d new, %d still unresolved (in %d or more reports in a row), %d seen in the last report too, %d acknowledged (`%s`).",
            nrow(new), nrow(unres), min_runs, n_rep, n_ack, ack_file),
    "",
    if (nrow(new)) c("#### New this run", "", bullets(new, function(d) sprintf("- %s: %s", d$section, d$item)), ""),
    if (nrow(unres)) c(sprintf("#### Still unresolved (%d+ reports in a row)", min_runs), "",
                       bullets(unres, function(d) sprintf("- %s: %s -- %d reports, since %s", d$section, d$item, d$runs,
                                                         ifelse(is.na(d$since), "?", d$since))), ""),
    sprintf("An item a person has looked at and accepts (a district that posts only on the statewide feed, say) goes in `%s` (Section, Item, Reason, Acknowledged) and leaves these lists.", ack_file),
    "",
    "---",
    GLANCE_END,
    "")
}

#' Put the at-a-glance block at the top of this run's drift report
#'
#' Run by the weekly workflow just before it comments on the drift issue. `previous_path` is the issue's earlier
#' reports as JSON (`[{"created": ..., "body": ...}, ...]`, oldest first; the workflow writes it with `gh`). A missing
#' or unreadable file means no history: every item is new. Rewrites `report_path` in place; does nothing when there is
#' no report.
#' @param report_path This run's report.
#' @param previous_path The earlier reports.
#' @param ack_file The state's acknowledgement file (columns `Section`, `Item`, `Reason`, `Acknowledged`).
#' @param min_runs See [classify_drift_repeats()].
#' @return The repeats table, invisibly.
#' @export
mark_drift_repeats_step <- function(report_path = "/tmp/drift_report.md", previous_path = "/tmp/drift_previous.json",
                                    ack_file = "drift_acknowledged.csv", min_runs = 3) {
  if (!file.exists(report_path)) { cat("No drift report this run.\n"); return(invisible(NULL)) }
  current <- readLines(report_path, warn = FALSE)
  previous <- tryCatch(jsonlite::fromJSON(previous_path, simplifyVector = FALSE), error = function(e) list())
  ack <- if (file.exists(ack_file)) utils::read.csv(ack_file, stringsAsFactors = FALSE, colClasses = "character") else NULL
  rep <- classify_drift_repeats(current, previous, ack, min_runs)
  glance <- build_drift_glance_markdown(rep, min_runs, ack_file)
  cat(sprintf("Drift items: %d new, %d still unresolved, %d acknowledged, over %d earlier report(s).\n",
              sum(rep$status == "new" & !rep$acknowledged), sum(rep$status == "unresolved" & !rep$acknowledged),
              sum(rep$acknowledged), length(previous)))
  if (length(glance)) writeLines(c(glance, current), report_path)
  invisible(rep)
}
