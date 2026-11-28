# Toyland's weekly K-12 run: the part of a state's pipeline the package doesn't hold yet. A real state's Rmd does the
# same steps with more sources (WY's K-12 chunks are about 450 lines). Everything defined here is something a third
# state would have to write or copy; README.md lists it, and test-toy-state.R fails when that list changes, so moving
# a step into the package shows up as a shorter list.
#
# Needs: library(edjobs), and state_config.R sourced with `toy_dir` set.

# GAP 1: each fetcher returns its own columns ("title"/"Title", "url"/"Link", "job_id"/"Posting_ID"); a state maps
# them to one raw row before archiving. WY's per-platform switch does this inline.
toy_raw_rows <- function(df, platform, district) {
  if (is.null(df) || !nrow(df)) return(NULL)
  pick <- function(...) {
    for (n in c(...)) if (n %in% names(df)) return(as.character(df[[n]]))
    rep(NA_character_, nrow(df))
  }
  url <- pick("url", "Link")
  if (platform == "AppliTrack") {
    registry <- read_k12_registry(toy_registry_csv)
    tenant <- registry$Platform_Key[registry$District == district]
    url <- dplyr::coalesce(applitrack_posting_url(tenant, pick("job_id")), url)
  }
  out <- data.frame(title = pick("title", "Title"), date_posted = pick("date_posted", "Posted_Date"),
                    position = pick("position"), location = pick("location", "Location"), url = url,
                    District = district, Posting_Source = platform, Close_Date = pick("closing_date", "Close_Date"),
                    stringsAsFactors = FALSE)
  cbind(out, ensure_posted_pay_columns(df)[POSTED_PAY_COLUMNS])
}

# GAP 2: posting identity. The package reads a stable ID from a posting's own link (k12_link_posting_id()); a posting
# with none (a district page like Apptegy's) needs a fallback, and each state writes its own (MT's
# build_k12_posting_id(), WY's assign_posting_identity()). Here: district + title + location + posted date, with a
# count-preserving suffix when two rows share it.
toy_posting_identity <- function(rows) {
  link_id <- k12_link_posting_id(rows$url)
  fallback <- paste("fallback", rows$District, tolower(rows$title), tolower(rows$location), rows$date_posted, sep = "|")
  rows$Posting_ID <- dplyr::coalesce(link_id, make.unique(fallback, sep = "#"))
  rows$Posting_Identity_Method <- ifelse(is.na(link_id), "fallback", "link")
  rows
}

# GAP 3: the run itself, one stage after another, writing the app's files. Steps 5-7 of the package review move
# these stages into package functions; until then every state's Rmd holds them.
toy_k12_run <- function(out_dir, run_date, session_factory = NULL) {
  app <- file.path(out_dir, "app"); archive <- file.path(out_dir, "Archivek12_Data")
  dir.create(app, recursive = TRUE, showWarnings = FALSE); dir.create(archive, showWarnings = FALSE)
  registry <- read_k12_registry(toy_registry_csv)

  # Scrape: every registry row through the package's dispatch.
  raw <- lapply(seq_len(nrow(registry)), function(i) {
    row <- registry[i, ]
    call <- resolve_scraper_call(row$District)
    df <- safe_scrape(row$District, function() run_scraper_call(call, session_factory, envir = asNamespace("edjobs")),
                      expected_cols = "title", log_path = file.path(out_dir, "scrape_log.csv"))
    toy_raw_rows(df, row$Platform, row$District)
  })
  rows <- dplyr::bind_rows(raw)
  rows$date_posted <- standardize_date(rows$date_posted)
  rows$title <- fix_title_encoding(rows$title)
  rows <- toy_posting_identity(move_title_detail(rows))
  rows$Archive_Date <- as.character(run_date)
  # Board ads from before the previous school year are flagged, kept in the archive and not counted
  # (flag_stale_board_postings(); the toy has no statewide feed, so every row is a board row).
  rows <- flag_stale_board_postings(rows, run_date)
  utils::write.csv(rows, file.path(archive, paste0("combined_", run_date, ".csv")), row.names = FALSE)

  # Build this run's files and append them to the accumulated ones.
  oews <- utils::read.csv(file.path(toy_dir, "oews_role_wages.csv"), colClasses = c(Area_Code = "character", SOC = "character", Year = "character"))
  combinedclean <- build_k12_combinedclean(dplyr::distinct(drop_stale_rows(rows)), oews, registry[c("District", "County")], state = "ZZ")
  utils::write.csv(combinedclean, file.path(app, "combinedclean.csv"), row.names = FALSE)
  write_k12_history(combinedclean, app)
  invisible(combinedclean)
}
