# New vs still unresolved in the weekly drift issue (test-kit plan item 3; Mike chose option B, 2026-11-22).
#
# Every check appends to one report, posted as a comment on one rolling issue per state, and the same items came back
# every run with nothing to say which were new: MT's Ulm notice was flagged in 8 comments and Frontier's empty board
# in 9 before anyone acted (claude-notes/2026-11-21_step8-review/, section G). This puts a short block at the top of
# each run's comment: the items that weren't there on the last run day, and the items flagged on `min_runs` or more
# run days in a row, with how many and since when; sections that flag something without per-source items are named. Items a person has looked at and accepts go in the state's
# `drift_acknowledged.csv` and drop out of both lists (they stay in the full report below).

GLANCE_START <- "<!-- drift-glance-start -->"
GLANCE_END <- "<!-- drift-glance-end -->"

# Sections that repeat every run by design (feed totals, the safety-net summary), not flags.
DRIFT_SUMMARY_SECTIONS <- "postings at districts we scrape directly|Safety Net Status Summary"

#' Read a drift report: its flagged items, its sections, and which sections were cut short or have no items
#'
#' The report is parts separated by `---` lines (one per check, `append_drift_report()`). A section is the nearest
#' `##`-`####` heading (without a trailing run date), or, for a part with no heading, its label: an "Automated <what>
#' check flagged" line (any such line starts a new section: the salary check writes coverage, value and reference
#' blocks in one part), "Reference tables behind the pay estimates", a leading bold label (`**Monitoring self-check:**`),
#' or the part's first words. Items are the top-level bullets that start with a bold name (`- **Name** ...`), keyed by
#' section and name; a parenthesis right after the name is part of the key when it names something ("(K-12)",
#' "(Swan River School District #4)", "(Hill County)"), with counts taken out ("(5)", ", 7 postings"), so the same item
#' matches from run to run while its numbers change. Summary sections (`DRIFT_SUMMARY_SECTIONS`) and an earlier
#' at-a-glance block are skipped.
#' @param md The report: a character vector of lines, or one string.
#' @return A list: `items` (data frame `section`, `item`, `key`, `details`: the bullet's text after its name, one row
#'   per key), `sections` (every section seen),
#'   `truncated` (sections with a "- ... and N more" line), `unitemized` (sections with content but no items).
#' @export
parse_drift_report <- function(md) {
  items <- data.frame(section = character(0), item = character(0), key = character(0), details = character(0),
                      stringsAsFactors = FALSE)
  res <- list(items = items, sections = character(0), truncated = character(0), unitemized = character(0))
  if (!length(md)) return(res)
  lines <- unlist(strsplit(gsub("\r", "", paste(md, collapse = "\n")), "\n", fixed = TRUE))
  start <- which(lines == GLANCE_START); end <- which(lines == GLANCE_END)
  if (length(start) && length(end)) {
    drop <- unlist(Map(seq, start, vapply(start, function(s) min(c(end[end >= s], length(lines))), numeric(1))))
    lines <- lines[-drop]
  }
  section <- ""; part_start <- TRUE
  out <- list(); content <- list(); trunc <- character(0)
  set_section <- function(x) { section <<- trimws(x); content[[section]] <<- content[[section]] %||% 0L }
  for (l in lines) {
    if (grepl("^---\\s*$", l)) { section <- ""; part_start <- TRUE; next }
    if (!nzchar(trimws(l))) next
    if (grepl("^#{2,4} ", l)) {
      set_section(sub("\\s*\\(\\d{4}-\\d{2}-\\d{2}\\)\\s*$", "", sub("^#+ ", "", l))); part_start <- FALSE; next
    }
    if (grepl("^Automated .+ flagged", l)) {
      set_section(sub("^Automated (.+?)( check)? flagged.*$", "\\1 check", l)); part_start <- FALSE; next
    }
    if (grepl("^Reference tables behind the pay estimates", l)) {
      set_section("Reference tables behind the pay estimates"); part_start <- FALSE; next
    }
    if (part_start && !grepl("^- ", l)) {
      lab <- if (grepl("^\\*\\*[^*]+\\*\\*", l)) sub(":$", "", sub("^\\*\\*([^*]+)\\*\\*.*$", "\\1", l)) else substr(l, 1, 60)
      set_section(lab); part_start <- FALSE
    }
    part_start <- FALSE
    if (!nzchar(section)) set_section("(no heading)")
    content[[section]] <- content[[section]] + 1L
    if (grepl("^- \\.\\.\\. and [0-9]+ more", l)) trunc <- union(trunc, section)
    if (!grepl("^- \\*\\*[^*]+\\*\\*", l) || grepl(DRIFT_SUMMARY_SECTIONS, section)) next
    item <- sub("^- \\*\\*([^*]+)\\*\\*.*$", "\\1", l)
    rest <- sub("^- \\*\\*[^*]+\\*\\*", "", l)
    paren <- if (grepl("^ ?\\(([^)]*)\\)", rest)) sub("^ ?\\(([^)]*)\\).*$", "\\1", rest) else ""
    paren <- trimws(gsub(",?\\s*\\d+ (postings?|vacanc(y|ies))", "", paren))
    if (grepl("^[0-9.,% ]*$", paren)) paren <- ""
    name <- if (nzchar(paren)) paste0(item, " (", paren, ")") else item
    # What the bullet says after its name: the postings or the figure flagged ("(4): English (posted 2026-11-16);
    # Music (posted 2026-10-01)", ": $16,900 is outside ..."), which an acknowledgement's Details are checked against.
    det <- trimws(sub("^\\s*[:\u2013\u2014-]+\\s*", "", sub("^ ?\\([^)]*\\)", "", rest)))
    out[[length(out) + 1]] <- data.frame(section = section, item = name, key = paste(section, name, sep = " | "),
                                         details = det, stringsAsFactors = FALSE)
  }
  if (length(out)) { r <- do.call(rbind, out); res$items <- r[!duplicated(r$key), , drop = FALSE] }
  res$sections <- names(content)
  res$truncated <- trunc
  has_items <- unique(res$items$section)
  res$unitemized <- names(content)[unlist(content) > 0 & !names(content) %in% has_items &
                                     !grepl(DRIFT_SUMMARY_SECTIONS, names(content))]
  res
}

#' The flagged items in a drift report
#'
#' [parse_drift_report()]'s items.
#' @param md The report: a character vector of lines, or one string.
#' @return Data frame: `section`, `item`, `key`, `details`, one row per distinct key.
#' @export
drift_report_items <- function(md) parse_drift_report(md)$items

#' Which of this run's drift items are new and which keep coming back
#'
#' Counts run days, not reports: of the earlier reports, only the last on each date counts, and those dated `today`
#' (an earlier re-run or test dispatch the same day) are left out. A report that doesn't show the item's section at
#' all (that check wrote nothing, or crashed) or cut the section short ("... and N more", MT's 15-district OPI list)
#' says nothing about the item: it neither extends nor breaks the run.
#' @param current This run's report (lines).
#' @param previous Earlier reports on the issue, oldest first: a list of `list(created = "<ISO time>", body = "...")`.
#' @param acknowledged Data frame with `Item` and optionally `Section` (empty or NA = any section) and `Details`: items a
#'   person has looked at; matched on the item name as [drift_report_items()] writes it. Without an `Item` column it is
#'   ignored. `Details` (empty or NA = the whole item, whatever it lists) names what was checked, as the report writes
#'   it, separated by "; " (the postings at a district, the figure flagged): the item is acknowledged only while every
#'   "; "-separated part of its details is one of them. Anything else (a new posting at an acknowledged district) makes
#'   it "new" again, listed in `unchecked`; so does a line with no details at all. A report that repeats a posting
#'   should say how many (MT: "Physical Ed (posted 2026-10-25, 2 postings)"), so a third copy is a new part. A row
#'   with empty `Details` for the same item covers it whole and wins.
#' @param min_runs An item is "still unresolved" once it is in this many run days in a row, this one included.
#' @param today This run's date ("YYYY-MM-DD"), or NA.
#' @return Data frame: `section`, `item`, `runs` (run days in a row, this one included), `since` (date of the earliest
#'   of those), `acknowledged`, `status` ("new", "unresolved" or "repeat"), `unchecked` (details an acknowledgement with
#'   `Details` doesn't cover, "; "-separated; "" otherwise).
#' @export
classify_drift_repeats <- function(current, previous = list(), acknowledged = NULL, min_runs = 3, today = NA) {
  cur <- drift_report_items(current)
  if (!nrow(cur)) return(cbind(cur[, c("section", "item")], runs = integer(0), since = character(0),
                               acknowledged = logical(0), status = character(0), unchecked = character(0)))
  dates <- vapply(previous, function(p) substr(as.character(p$created %||% NA_character_), 1, 10), character(1))
  keep <- if (length(previous)) !duplicated(dates, fromLast = TRUE) | is.na(dates) else logical(0)
  if (!is.na(today)) keep <- keep & (is.na(dates) | dates != today)
  previous <- previous[keep]; dates <- dates[keep]
  parsed <- lapply(previous, function(p) parse_drift_report(p$body))
  runs <- integer(nrow(cur)); since <- rep(NA_character_, nrow(cur))
  for (i in seq_len(nrow(cur))) {
    n <- 0L; first <- NA_character_
    for (j in rev(seq_along(parsed))) {
      pj <- parsed[[j]]
      if (cur$key[i] %in% pj$items$key) { n <- n + 1L; first <- dates[j]; next }
      if (!cur$section[i] %in% pj$sections || cur$section[i] %in% pj$truncated) next
      break
    }
    runs[i] <- n + 1L
    since[i] <- first
  }
  ack <- rep(FALSE, nrow(cur)); unchecked <- rep("", nrow(cur))
  if (!is.null(acknowledged) && nrow(acknowledged) && "Item" %in% names(acknowledged)) {
    sec <- if ("Section" %in% names(acknowledged)) acknowledged$Section else rep(NA_character_, nrow(acknowledged))
    det <- if ("Details" %in% names(acknowledged)) as.character(acknowledged$Details) else rep(NA_character_, nrow(acknowledged))
    parts <- function(x) { p <- trimws(unlist(strsplit(x, "; ", fixed = TRUE))); p[nzchar(p)] }
    for (i in seq_len(nrow(cur))) {
      hit <- which(acknowledged$Item == cur$item[i] & (is.na(sec) | !nzchar(sec) | sec == cur$section[i]))
      if (!length(hit)) next
      if (any(is.na(det[hit]) | !nzchar(trimws(det[hit])))) { ack[i] <- TRUE; next }   # the whole item
      now <- parts(cur$details[i])
      # Nothing on the bullet's own line to check (its postings on nested lines, or none listed): not acknowledged,
      # since what was checked can't be compared with what is there now (review, 2026-11-22).
      if (!length(now)) { unchecked[i] <- "(this run's line lists nothing to check against)"; next }
      left <- setdiff(now, unlist(lapply(det[hit], parts)))
      if (length(left)) unchecked[i] <- paste(left, collapse = "; ") else ack[i] <- TRUE
    }
  }
  status <- ifelse(runs == 1L | nzchar(unchecked), "new", ifelse(runs >= min_runs, "unresolved", "repeat"))
  data.frame(section = cur$section, item = cur$item, runs = runs, since = since, acknowledged = ack,
             status = status, unchecked = unchecked, stringsAsFactors = FALSE)
}

#' The at-a-glance block for the top of a drift comment
#'
#' @param repeats [classify_drift_repeats()] output.
#' @param min_runs As there.
#' @param ack_file The acknowledgement file named in the text.
#' @param max_list Most items listed under each heading.
#' @param unitemized Sections in this report that flag something without a per-source item (a table, a paragraph:
#'   rebuild parity, run expectations, a crashed monitoring step), named so "0 new" can't hide them.
#' @return Markdown lines, wrapped in the `GLANCE_START`/`GLANCE_END` markers; none when there are no items.
#' @export
build_drift_glance_markdown <- function(repeats, min_runs = 3, ack_file = "drift_acknowledged.csv", max_list = 40,
                                        unitemized = character(0)) {
  if (!nrow(repeats) && !length(unitemized)) return(character(0))
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
    sprintf("%d new, %d still unresolved (in %d or more run days in a row), %d seen on the last run day too, %d acknowledged (`%s`).",
            nrow(new), nrow(unres), min_runs, n_rep, n_ack, ack_file),
    "",
    if (length(unitemized)) c(paste0("Also in this report, not counted above (read them below): ",
                                     paste(unitemized, collapse = "; "), "."), ""),
    if (nrow(new)) c("#### New this run", "", bullets(new, function(d) {
      uc <- if ("unchecked" %in% names(d)) d$unchecked else rep("", nrow(d))
      sprintf("- %s: %s%s", d$section, d$item, ifelse(nzchar(uc), paste0(" -- not yet checked: ", uc), ""))
    }), ""),
    if (nrow(unres)) c(sprintf("#### Still unresolved (%d+ run days in a row)", min_runs), "",
                       bullets(unres, function(d) sprintf("- %s: %s -- %d run days, since %s", d$section, d$item, d$runs,
                                                         ifelse(is.na(d$since), "?", d$since))), ""),
    sprintf("An item a person has looked at and accepts goes in `%s` (Section, Item, Reason, Acknowledged; Details, to accept only what was checked: the postings or figure exactly as the full report below lists them after the name, \"; \"-separated) and leaves these lists. With Details, anything not checked brings the item back as new.", ack_file),
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
#' @param min_runs,today See [classify_drift_repeats()].
#' @return The repeats table, invisibly.
#' @export
mark_drift_repeats_step <- function(report_path = "/tmp/drift_report.md", previous_path = "/tmp/drift_previous.json",
                                    ack_file = "drift_acknowledged.csv", min_runs = 3,
                                    today = format(Sys.Date(), "%Y-%m-%d")) {
  if (!file.exists(report_path)) { cat("No drift report this run.\n"); return(invisible(NULL)) }
  current <- readLines(report_path, warn = FALSE)
  previous <- tryCatch(jsonlite::fromJSON(previous_path, simplifyVector = FALSE), error = function(e) list())
  ack <- if (file.exists(ack_file)) utils::read.csv(ack_file, stringsAsFactors = FALSE, colClasses = "character") else NULL
  if (!is.null(ack) && !"Item" %in% names(ack)) { cat(ack_file, "has no Item column; ignored.\n"); ack <- NULL }
  rep <- classify_drift_repeats(current, previous, ack, min_runs, today = today)
  glance <- build_drift_glance_markdown(rep, min_runs, ack_file, unitemized = parse_drift_report(current)$unitemized)
  cat(sprintf("Drift items: %d new, %d still unresolved, %d acknowledged, over %d earlier report(s).\n",
              sum(rep$status == "new" & !rep$acknowledged), sum(rep$status == "unresolved" & !rep$acknowledged),
              sum(rep$acknowledged), length(previous)))
  # GitHub's comment limit is 65,536 characters: a report already near it is filed without the block.
  if (length(glance) && sum(nchar(c(glance, current))) + length(c(glance, current)) > 65000) {
    cat("Report too long for the at-a-glance block; filed without it.\n"); glance <- character(0)
  }
  if (length(glance)) writeLines(c(glance, current), report_path)
  invisible(rep)
}
