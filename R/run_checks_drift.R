# The weekly run's drift-family checks as package functions (2026-11-17 package review, step 6, slice 2): the drift
# check, posted-pay coverage and title quality. What differed between the states' scripts is now state_config()
# data (k12_source_names, drift_min_mean, direct_registry, feed). Output and files are the scripts' own.

#' The source name of each K-12 row, as the state's weekly checks count it
#'
#' `state_config(k12_source_names = )` applied to `df`; `District` when the state sets none.
#' @param df An archive or `combinedclean.csv` data frame.
#' @param config A `state_config()`; defaults to the session's.
#' @return One name per row, or NULL when the state's function leaves this archive out.
#' @export
state_k12_source_names <- function(df, config = current_state_config()) {
  f <- config$k12_source_names
  if (is.null(f)) df$District else f(df)
}

append_drift_report <- function(lines, report_path) {
  if (file.exists(report_path)) {
    write(c("", "---", "", lines), file = report_path, append = TRUE)
  } else {
    writeLines(lines, report_path)
  }
}

#' The weekly drift check
#'
#' Tier 1 of the drift-alert system. Non-blocking and informational: compares each source's posting count this run
#' with its own baseline from the archives (one run a week, `build_historical_counts()`; weeks its scrape failed
#' left out), flags drops (`flag_drift()`, floor `state_config(drift_min_mean = )`), attaches this run's scrape
#' errors and the statewide feed's fallback counts, and writes `out_csv` (header only when nothing is flagged).
#' @param app_dir The app folder; defaults to the session's `state_config()`.
#' @param today The run date; that day's archives are left out of the baseline.
#' @param out_csv Where the flagged sources go.
#' @return The flagged sources, invisibly.
#' @export
check_drift_step <- function(app_dir = NULL, today = format(Sys.Date(), "%Y-%m-%d"), out_csv = "/tmp/drift_flagged.csv") {
  app_dir <- run_app_dir(app_dir)
  config <- current_state_config()
  read_k12_archive <- function(path) {
    nm <- state_k12_source_names(utils::read.csv(path, stringsAsFactors = FALSE), config)
    if (is.null(nm)) character(0) else nm
  }
  read_he_archive <- function(path) readxl::read_xlsx(path)$Institution
  load_snapshots <- function(dir, pattern, date_regex, reader, exclude_date) {
    files <- list.files(dir, pattern = pattern, full.names = TRUE)
    dates <- regmatches(files, regexpr(date_regex, files))
    keep <- dates != exclude_date
    files <- files[keep]
    dates <- dates[keep]
    snapshots <- lapply(files, function(f) data.frame(name = reader(f)))
    names(snapshots) <- dates
    snapshots
  }
  date_regex <- "[0-9]{4}-[0-9]{2}-[0-9]{2}"
  k12_snapshots <- load_snapshots("Archivek12_Data", "^combined_.*\\.csv$", date_regex, read_k12_archive, today)
  he_snapshots <- load_snapshots("Archived_HE_Data", "^hedata_.*\\.xlsx$", date_regex, read_he_archive, today)

  failed_weeks <- failed_source_weeks(if (file.exists("coverage_calendar.csv")) utils::read.csv("coverage_calendar.csv", stringsAsFactors = FALSE))
  k12_baseline <- build_historical_counts(k12_snapshots, "name", failed = failed_weeks)
  he_baseline <- build_historical_counts(he_snapshots, "name", failed = failed_weeks)

  combined_raw <- utils::read.csv(file.path(app_dir, "combinedclean.csv"), stringsAsFactors = FALSE)
  registry <- if (file.exists("k12_district_registry.csv")) {
    if (is.null(config$direct_registry)) utils::read.csv("k12_district_registry.csv", stringsAsFactors = FALSE) else config$direct_registry()
  } else NULL

  current_k12 <- as.data.frame(table(state_k12_source_names(combined_raw, config)))
  names(current_k12) <- c("name", "count")
  current_he <- as.data.frame(table(readxl::read_xlsx(file.path(app_dir, "hedata.xlsx"))$Institution))
  names(current_he) <- c("name", "count")

  tag_type <- function(df, label) {
    if (nrow(df) > 0) df$type <- label
    df
  }
  min_mean <- if (is.null(config$drift_min_mean)) 3 else config$drift_min_mean
  flagged_k12 <- tag_type(flag_drift(current_k12, k12_baseline, min_mean_count = min_mean), "K-12")
  flagged_he <- tag_type(flag_drift(current_he, he_baseline, min_mean_count = min_mean), "Higher Ed")
  flagged <- if (nrow(flagged_k12) == 0 && nrow(flagged_he) == 0) {
    data.frame(name = character(0), mean_count = numeric(0), n_weeks = integer(0), count = numeric(0), type = character(0))
  } else {
    rbind(flagged_k12, flagged_he)
  }

  # scrape_log.csv: this run's own record of what errored vs. came back empty (attach_scrape_log_errors()).
  scrape_log <- if (file.exists("scrape_log.csv")) {
    utils::read.csv("scrape_log.csv", stringsAsFactors = FALSE)
  } else {
    data.frame(timestamp = character(0), source = character(0), status = character(0),
               n_rows = integer(0), error_message = character(0))
  }
  flagged <- attach_scrape_log_errors(flagged, scrape_log)

  safety_net_summary <- summarize_central_safety_net(
    combined_df = combined_raw,
    registry = registry,
    scrape_log = scrape_log,
    feed_label = config$feed$label,
    feed_short_name = config$feed$short_name,
    discrepancy_path = config$feed$discrepancy_file
  )
  flagged$safety_net_vacancies <- if (nrow(flagged) == 0) {
    integer(0)
  } else {
    vapply(seq_len(nrow(flagged)), function(i) {
      if (flagged$type[i] == "K-12") {
        val <- safety_net_summary$fallback_districts[[flagged$name[i]]]
        if (!is.null(val)) as.integer(val) else 0L
      } else {
        NA_integer_
      }
    }, integer(1))
  }

  cat(sprintf(
    "Drift check: %d K-12 baseline sources, %d HE baseline sources, %d flagged (%d confirmed by scrape_log as errored).\n",
    nrow(k12_baseline), nrow(he_baseline), nrow(flagged), sum(!is.na(flagged$scrape_error))
  ))
  cat(sprintf(
    "Statewide feed: %d postings; %d district(s) we scrape directly have %d posting(s) counted from it that their own board doesn't list.\n",
    safety_net_summary$total_feed_vacancies, length(safety_net_summary$fallback_districts), safety_net_summary$total_fallback_vacancies
  ))
  if (nrow(flagged) > 0) print(flagged)
  utils::write.csv(flagged, out_csv, row.names = FALSE)
  invisible(flagged)
}

#' The weekly posted-pay coverage check
#'
#' Non-blocking and informational: a source whose postings still arrive but have lost the pay they used to carry
#' (`flag_pay_coverage_drop()`, against its own archived weeks, one run a week) goes into the drift report; so do
#' the posted amounts `drop_implausible_pay()` blanked in today's raw archives.
#' @param app_dir The app folder; defaults to the session's `state_config()`.
#' @param today The run date: left out of the history; its archives are where the blanked amounts are read.
#' @param out_csv,report_path Where the flagged sources and the drift-report sections go.
#' @return The flagged sources, invisibly.
#' @export
check_pay_coverage_step <- function(app_dir = NULL, today = format(Sys.Date(), "%Y-%m-%d"),
                                    out_csv = "/tmp/pay_coverage_flagged.csv", report_path = "/tmp/drift_report.md") {
  app_dir <- run_app_dir(app_dir)
  config <- current_state_config()
  k12_pay_counts <- function(df) {
    if (!"Pay_Min" %in% names(df)) return(NULL)
    nm <- state_k12_source_names(df, config)
    if (is.null(nm)) return(NULL)
    pay_coverage_counts(nm, !is.na(df$Pay_Min))
  }
  he_pay_counts <- function(df) {
    if (!"Pay_Min" %in% names(df)) return(NULL)
    pay_coverage_counts(df$Institution, !is.na(df$Pay_Min))
  }
  archive_history <- function(dir, pattern, reader, counter, exclude_date) {
    files <- list.files(dir, pattern = pattern, full.names = TRUE)
    files <- files[!grepl(exclude_date, basename(files), fixed = TRUE)]
    files <- last_run_each_week(files)   # the averages and min_weeks were measured on weekly runs (0.39.0)
    Filter(Negate(is.null), lapply(files, function(f) counter(reader(f))))
  }
  read_csv <- function(f) utils::read.csv(f, stringsAsFactors = FALSE)
  read_xlsx <- function(f) as.data.frame(readxl::read_xlsx(f))

  k12_history <- archive_history("Archivek12_Data", "^combined_.*\\.csv$", read_csv, k12_pay_counts, today)
  he_history <- archive_history("Archived_HE_Data", "^hedata_.*\\.xlsx$", read_xlsx, he_pay_counts, today)
  k12_current <- k12_pay_counts(read_csv(file.path(app_dir, "combinedclean.csv")))
  he_current <- he_pay_counts(read_xlsx(file.path(app_dir, "hedata.xlsx")))

  flag <- function(current, history, label) {
    if (is.null(current)) return(NULL)
    out <- flag_pay_coverage_drop(current, history)
    out$type <- rep(label, nrow(out))
    out
  }
  flagged <- rbind(flag(k12_current, k12_history, "K-12"), flag(he_current, he_history, "Higher Ed"))
  if (is.null(flagged)) {
    flagged <- data.frame(name = character(0), mean_share = numeric(0), n_weeks = integer(0),
                          n = integer(0), with_pay = integer(0), type = character(0))
  }
  cat(sprintf("Pay coverage: %d K-12 and %d HE archived weeks with pay, %d source(s) flagged.\n",
              length(k12_history), length(he_history), nrow(flagged)))
  utils::write.csv(flagged, out_csv, row.names = FALSE)

  report_lines <- build_pay_coverage_report_markdown(flagged)
  if (length(report_lines) > 0) {
    print(flagged)
    append_drift_report(report_lines, report_path)
  }

  implausible <- function(df, source_col, title_col, label) {
    if (is.null(df) || !"Pay_Min" %in% names(df)) return(NULL)
    bad <- is_implausible_pay(df$Pay_Min, df$Pay_Max, df$Pay_Unit)
    if (!any(bad)) return(NULL)
    data.frame(source = df[[source_col]][bad], title = df[[title_col]][bad], df[bad, POSTED_PAY_COLUMNS],
               type = label, stringsAsFactors = FALSE, row.names = NULL)
  }
  todays <- function(dir, pattern, reader) {
    f <- list.files(dir, pattern = pattern, full.names = TRUE)
    f <- f[grepl(today, basename(f), fixed = TRUE)]
    if (length(f) == 0) NULL else reader(f[1])
  }
  bad_pay <- rbind(implausible(todays("Archivek12_Data", "^combined_.*\\.csv$", read_csv), "District", "title", "K-12"),
                   implausible(todays("Archived_HE_Data", "^hedata_.*\\.xlsx$", read_xlsx), "Institution", "Title", "Higher Ed"))
  cat(sprintf("Implausible posted pay blanked this run: %d\n", if (is.null(bad_pay)) 0L else nrow(bad_pay)))
  if (!is.null(bad_pay)) {
    print(bad_pay[, c("source", "title", "Pay_Min", "Pay_Max", "Pay_Unit")])
    append_drift_report(build_implausible_pay_report_markdown(bad_pay), report_path)
  }
  invisible(flagged)
}

#' The weekly title-quality check
#'
#' Non-blocking and informational (classification safeguards items 5, 6 and 9): titles that don't look like job
#' titles (`lint_title_shape()`, pay the Posted Pay column doesn't carry), sources whose share of Other jumped
#' against their own recent weeks (`flag_other_share_jump()`), and titles whose words name a different bucket
#' (`find_keyword_conflicts()`). All go into the drift report; the three tables are written either way.
#' @param app_dir The app folder; defaults to the session's `state_config()`.
#' @param today The run date, left out of the history.
#' @param lookback_weeks How many recent weeks the Other share is compared with (each week's last run).
#' @param out_dir Where `title_lint.csv`, `other_share_flagged.csv` and `keyword_conflicts.csv` go.
#' @param report_path The drift report.
#' @return The three tables, invisibly.
#' @export
check_title_quality_step <- function(app_dir = NULL, today = format(Sys.Date(), "%Y-%m-%d"), lookback_weeks = 8,
                                     out_dir = "/tmp", report_path = "/tmp/drift_report.md") {
  app_dir <- run_app_dir(app_dir)
  config <- current_state_config()
  read_csv <- function(f) utils::read.csv(f, stringsAsFactors = FALSE)
  read_xlsx <- function(f) as.data.frame(readxl::read_xlsx(f))
  k12_sources <- function(df) state_k12_source_names(df, config)

  k12 <- read_csv(file.path(app_dir, "combinedclean.csv"))
  he <- read_xlsx(file.path(app_dir, "hedata.xlsx"))
  k12$source <- k12_sources(k12)
  he$source <- he$Institution

  # ---- 5. title-shape lint ----
  lint <- function(df, title_col, label) {
    reason <- lint_title_shape(df[[title_col]])
    pay_in_title <- if ("Pay_Min" %in% names(df)) title_pay_unrecorded(df[[title_col]], df$Pay_Min) else FALSE
    reason <- ifelse(pay_in_title, ifelse(is.na(reason), "pay in the title, Posted Pay blank",
                                          paste0(reason, "; pay in the title, Posted Pay blank")), reason)
    hit <- !is.na(reason)
    data.frame(source = df$source[hit], type = rep(label, sum(hit)), title = df[[title_col]][hit],
               reason = reason[hit], stringsAsFactors = FALSE)
  }
  hits <- rbind(lint(k12, "title", "K-12"), lint(he, "Title", "Higher Ed"))
  hits <- hits[order(hits$type != "K-12", hits$source), , drop = FALSE]
  cat(sprintf("Title lint: %d K-12 and %d HE posting(s) with a title that doesn't look like a job title, at %d source(s).\n",
              sum(hits$type == "K-12"), sum(hits$type == "Higher Ed"), length(unique(paste(hits$type, hits$source)))))
  utils::write.csv(hits, file.path(out_dir, "title_lint.csv"), row.names = FALSE)

  # ---- 6. per-source Other share against its own recent weeks (days and weeks, not files: 0.39.0) ----
  recent_archives <- function(dir, pattern) {
    last_run_each_week(recent_archive_files(dir, pattern, days = 7 * lookback_weeks, exclude_date = today))
  }
  k12_other <- function(df) {
    src <- k12_sources(df)
    if (is.null(src)) return(NULL)
    other_share_counts(src, classify_k12_position(df$title) == "Other")
  }
  he_other <- function(df) other_share_counts(df$Institution, classify_he_job_type(df$Title) == "Other")
  k12_history <- Filter(Negate(is.null), lapply(recent_archives("Archivek12_Data", "^combined_.*\\.csv$"),
                                                function(f) k12_other(read_csv(f))))
  he_history <- Filter(Negate(is.null), lapply(recent_archives("Archived_HE_Data", "^hedata_.*\\.xlsx$"),
                                               function(f) he_other(read_xlsx(f))))
  k12$is_other <- classify_k12_position(k12$title) == "Other"
  he$is_other <- classify_he_job_type(he$Title) == "Other"
  flag <- function(df, history, title_col, label) {
    out <- flag_other_share_jump(other_share_counts(df$source, df$is_other), history)
    out$type <- rep(label, nrow(out))
    out$examples <- vapply(out$name, function(s) {
      t <- unique(df[[title_col]][df$source == s & df$is_other])
      paste(sprintf("\"%s\"", substr(utils::head(t, 3), 1, 60)), collapse = ", ")
    }, character(1))
    out
  }
  other_flagged <- rbind(flag(k12, k12_history, "title", "K-12"), flag(he, he_history, "Title", "Higher Ed"))
  cat(sprintf("Other share: %.1f%% of K-12 and %.1f%% of HE postings in Other; %d source(s) flagged against their last %d K-12 / %d HE archived weeks.\n",
              100 * mean(k12$is_other), 100 * mean(he$is_other), nrow(other_flagged),
              length(k12_history), length(he_history)))
  utils::write.csv(other_flagged, file.path(out_dir, "other_share_flagged.csv"), row.names = FALSE)

  # ---- 9. keyword conflicts ----
  conflicts <- function(df, title_col, bucket, rules, label) {
    rule <- find_keyword_conflicts(df[[title_col]], bucket, rules)
    hit <- !is.na(rule)
    data.frame(source = df$source[hit], type = rep(label, sum(hit)), title = df[[title_col]][hit],
               bucket = bucket[hit], rule = rule[hit], stringsAsFactors = FALSE)
  }
  conflict_hits <- rbind(conflicts(k12, "title", classify_k12_position(k12$title), k12_keyword_conflict_rules, "K-12"),
                         conflicts(he, "Title", classify_he_job_type(he$Title), he_keyword_conflict_rules, "Higher Ed"))
  cat(sprintf("Keyword conflicts: %d K-12 and %d HE posting(s) whose title names a different bucket.\n",
              sum(conflict_hits$type == "K-12"), sum(conflict_hits$type == "Higher Ed")))
  utils::write.csv(conflict_hits, file.path(out_dir, "keyword_conflicts.csv"), row.names = FALSE)

  report_lines <- c(build_other_share_report_markdown(other_flagged), build_title_lint_report_markdown(hits),
                    build_keyword_conflict_report_markdown(conflict_hits))
  if (length(report_lines) > 0) {
    if (nrow(other_flagged) > 0) print(other_flagged, row.names = FALSE)
    if (nrow(hits) > 0) print(hits[, c("source", "type", "reason")], row.names = FALSE)
    if (nrow(conflict_hits) > 0) print(conflict_hits[, c("source", "type", "bucket", "rule")], row.names = FALSE)
    append_drift_report(report_lines, report_path)
  }
  invisible(list(title_lint = hits, other_share = other_flagged, keyword_conflicts = conflict_hits))
}

#' The weekly check for sources that produce no rows
#'
#' Non-blocking and informational: registry sources (K-12 districts, HE institutions) with no rows in any of the
#' last `n_runs` weekly runs (`find_silent_sources()`) go into the drift report, apart from those listed in
#' `ack_file` (columns `Type` "K-12"/"Higher Ed", `Source`, `Reason`, `Acknowledged`) whose latest scrape this run
#' didn't error: an acknowledged source that errored is listed with its error. An acknowledgement with an
#' `Empty_Text` (optional column) holds only while the board's page still matches it (`check_ack_empty_text()`,
#' fetched each run); otherwise the source is listed as needing a look. K-12 counts direct-board rows
#' only (the statewide feed's rows are not the board's); an archive without `Posting_Source` counts every row under
#' its district, so it can only make a source look less silent. Today's archives are included: the check is on
#' what the run just collected.
#' @param app_dir Unused today (the archives are read from the repo root); kept for the steps' common signature.
#' @param today The run date.
#' @param n_runs How many of the latest weekly runs a source must be silent in.
#' @param skip_platforms K-12 registry Platforms with no board to scrape (MT's "OPIOnly").
#' @param ack_file The acknowledgement file.
#' @param out_csv,report_path Where the silent sources and the drift-report section go.
#' @param scrape_log_path This run's scrape log: a silent source whose latest attempt errored is listed with the
#'   error, acknowledged or not.
#' @param fetch Fetches a board page for the `Empty_Text` check (a function of a URL returning HTML).
#' @return The silent sources, invisibly.
#' @export
check_silent_sources_step <- function(app_dir = NULL, today = format(Sys.Date(), "%Y-%m-%d"), n_runs = 4,
                                      skip_platforms = character(0), ack_file = "silent_sources_acknowledged.csv",
                                      out_csv = "/tmp/silent_sources.csv", report_path = "/tmp/drift_report.md",
                                      scrape_log_path = "scrape_log.csv", fetch = fetch_board_html) {
  config <- current_state_config()
  feed_label <- config$feed$label
  runs <- function(dir, pattern) {
    f <- sort(list.files(dir, pattern = pattern, full.names = TRUE))
    f <- f[regmatches(basename(f), regexpr("\\d{4}-\\d{2}-\\d{2}", basename(f))) <= today]
    f <- last_run_each_week(f)
    stats::setNames(f, regmatches(basename(f), regexpr("\\d{4}-\\d{2}-\\d{2}", basename(f))))
  }
  tally <- function(x) { t <- table(x); stats::setNames(as.integer(t), names(t)) }
  k12_counts <- lapply(runs("Archivek12_Data", "^combined_.*\\.csv$"), function(f) {
    d <- utils::read.csv(f, stringsAsFactors = FALSE)
    if (!nrow(d)) return(integer(0))
    if (!"Posting_Source" %in% names(d)) d$Posting_Source <- ""   # no source column: every row counts under its district
    nm <- if (is.null(config$k12_source_names)) d$District else config$k12_source_names(d)
    if (is.null(nm)) nm <- d$District
    # No statewide feed: every row is the board's (`x != NULL` is logical(0), which dropped every row).
    direct <- if (is.null(feed_label)) rep(TRUE, nrow(d)) else is.na(d$Posting_Source) | d$Posting_Source != feed_label
    tally(nm[direct])
  })
  he_counts <- lapply(runs("Archived_HE_Data", "^hedata_.*\\.xlsx$"), function(f) tally(readxl::read_xlsx(f)$Institution))

  k12_reg <- if (file.exists("k12_district_registry.csv")) utils::read.csv("k12_district_registry.csv", stringsAsFactors = FALSE) else NULL
  k12_sources <- if (is.null(k12_reg)) character(0) else unique(k12_reg$District[!k12_reg$Platform %in% skip_platforms])
  he_sources <- if (file.exists("he_institution_registry.csv")) unique(utils::read.csv("he_institution_registry.csv", stringsAsFactors = FALSE)$Institution) else character(0)
  ack <- if (file.exists(ack_file)) utils::read.csv(ack_file, stringsAsFactors = FALSE) else data.frame(Type = character(0), Source = character(0))
  ack_for <- function(type) ack$Source[ack$Type == type]

  silent <- rbind(
    transform(find_silent_sources(k12_counts, k12_sources, n_runs, ack_for("K-12")), type = rep("K-12", length(source))),
    transform(find_silent_sources(he_counts, he_sources, n_runs, ack_for("Higher Ed")), type = rep("Higher Ed", length(source)))
  )
  # This run's scrape errors (a CyberSchool page with no jobs list, a dead link): an acknowledgement says a board is
  # really empty, not that it may break, so a silent source whose latest attempt errored is listed either way.
  scrape_log <- if (file.exists(scrape_log_path)) utils::read.csv(scrape_log_path, stringsAsFactors = FALSE) else data.frame()
  silent$scrape_error <- attach_scrape_log_errors(data.frame(name = silent$source, stringsAsFactors = FALSE),
                                                  scrape_log)$scrape_error
  # Acknowledged silent sources with an Empty_Text: their board must still read as empty (check_ack_empty_text()).
  silent$empty_check <- rep(NA_character_, nrow(silent))
  check <- silent$acknowledged & (is.na(silent$scrape_error) | !nzchar(silent$scrape_error))
  if (any(check) && "Empty_Text" %in% names(ack)) {
    he_reg <- if (file.exists("he_institution_registry.csv")) utils::read.csv("he_institution_registry.csv", stringsAsFactors = FALSE) else NULL
    links <- c(if (!is.null(k12_reg)) stats::setNames(k12_reg$Job_Link, paste("K-12", k12_reg$District, sep = "|")),
               if (!is.null(he_reg) && "Job_Link" %in% names(he_reg)) stats::setNames(he_reg$Job_Link, paste("Higher Ed", he_reg$Institution, sep = "|")))
    keys <- paste(silent$type, silent$source, sep = "|")
    res <- check_ack_empty_text(ack[paste(ack$Type, ack$Source, sep = "|") %in% keys[check], , drop = FALSE], links, fetch = fetch)
    silent$empty_check[check] <- unname(res[keys[check]])
  }
  stale_ack <- setdiff(paste(ack$Type, ack$Source), paste(silent$type, silent$source))
  cat(sprintf("Silent sources: %d K-12 and %d HE registry source(s) with no rows in the last %d weekly runs (%d acknowledged, not erroring, still reading as empty); %d K-12 and %d HE weekly runs on hand.\n",
              sum(silent$type == "K-12"), sum(silent$type == "Higher Ed"), n_runs,
              sum(silent$acknowledged & (is.na(silent$scrape_error) | !nzchar(silent$scrape_error)) &
                    (is.na(silent$empty_check) | !nzchar(silent$empty_check))),
              length(k12_counts), length(he_counts)))
  if (length(stale_ack)) cat("Acknowledged but producing rows again (remove from ", ack_file, "): ",
                             paste(stale_ack, collapse = ", "), "\n", sep = "")
  if (nrow(silent) > 0) print(silent, row.names = FALSE)
  utils::write.csv(silent, out_csv, row.names = FALSE)
  report_lines <- build_silent_sources_report_markdown(silent, n_runs, date = today, ack_file = ack_file)
  if (length(report_lines) > 0) append_drift_report(report_lines, report_path)
  invisible(silent)
}
