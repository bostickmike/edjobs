# The accumulated K-12 history files, built one way for every state (phase 4,
# 2026-11-12). The weekly pipeline classifies only the current run's rows and
# appends them (append_weekly_rows()), instead of re-reading every archive;
# the same builders serve the from-archive rebuild, so the two cannot drift.
#
# One output schema: until 0.10.0 MT wrote Posting_ID / Posting_Source /
# Posting_Identity_Method / OPI_District / Also_On_OPI / Role_Hint and WY
# wrote posting_id. K12_POSTING_COLUMNS is what every state's
# combinedclean.csv holds, so the states' files stack. A state's raw rows and
# archives keep their own column names; build_k12_combinedclean(columns = )
# maps them. Posting ids are never recomputed here: the spell history
# bridges on their values.

#' The K-12 output columns every state writes
#'
#' `K12_POSTING_COLUMNS`: `combinedclean.csv`, one row per counted posting.
#' `K12_TEACHER_COLUMNS`: `k12jobanalysis.csv`, the teacher postings with
#' subject and broad category. `Feed_District` and `Also_On_Feed` describe the
#' statewide feed (the district a feed posting names; whether a direct
#' posting is also on the feed); `Role_Hint` is the role read from a feed
#' posting's description. A state that does not record one of them writes NA.
#' @export
K12_POSTING_COLUMNS <- c("title", "Archive_Date", "date_posted", "position", "location", "url", "District",
                         "Posting_Source", "Posting_Identity_Method", "Posting_ID", "Feed_District", "Also_On_Feed",
                         "Close_Date", POSTED_PAY_COLUMNS, OEWS_ESTIMATE_COLUMNS, "Role_Hint")

#' @rdname K12_POSTING_COLUMNS
#' @export
K12_TEACHER_COLUMNS <- c("title", "Archive_Date", "position", "location", "url", "District", "Posting_Source",
                         "Posting_Identity_Method", "Posting_ID", "Category", "Broad_Category", "Role_Hint")

#' Append one run's rows to an accumulated file
#'
#' Reads the accumulated CSV at `existing_path` and returns it with
#' `new_rows` appended. `new_rows` must hold exactly one `date_col` value; if
#' the file already has rows for that date (a same-day re-run), they are
#' replaced, so running twice in a day cannot double-count it. The columns
#' must match the file's exactly: a mismatch means a classifier or schema
#' changed, which is a decision (update the file's schema, or rebuild it from
#' the archive), not something to pad with NA. With no file yet, `new_rows`
#' is returned; with no new rows, the file as it is.
#' @param existing_path The accumulated CSV.
#' @param new_rows This run's rows.
#' @param date_col The run-date column.
#' @param read_fn The reader (a parameter so tests can pass synthetic data).
#' @return The rows to write back.
#' @export
append_weekly_rows <- function(existing_path, new_rows, date_col = "Archive_Date",
                                read_fn = utils::read.csv) {
  if (nrow(new_rows) == 0) {
    if (!file.exists(existing_path)) return(new_rows)
    existing <- read_fn(existing_path, stringsAsFactors = FALSE)
    existing[[date_col]] <- as.character(existing[[date_col]])
    return(existing)
  }

  new_rows[[date_col]] <- as.character(new_rows[[date_col]])
  new_dates <- unique(new_rows[[date_col]])
  if (length(new_dates) != 1) {
    stop("append_weekly_rows(): new_rows must contain exactly one ", date_col,
         ", got ", length(new_dates), ": ", paste(new_dates, collapse = ", "))
  }

  if (!file.exists(existing_path)) {
    return(new_rows)
  }

  existing <- read_fn(existing_path, stringsAsFactors = FALSE)
  existing[[date_col]] <- as.character(existing[[date_col]])

  if (!setequal(names(existing), names(new_rows))) {
    stop(
      "append_weekly_rows(): column mismatch between existing ", existing_path,
      " (", paste(sort(names(existing)), collapse = ", "), ") and this week's new rows (",
      paste(sort(names(new_rows)), collapse = ", "), "). A classifier or schema likely ",
      "changed -- fix the mismatch, or rebuild the file from the raw archive, rather than ",
      "letting bind_rows() silently pad the difference with NA."
    )
  }

  # Same-day re-run guard -- drop this date's rows from the existing file
  # before appending, so a re-run replaces rather than duplicates.
  existing <- existing[!(existing[[date_col]] %in% new_dates), , drop = FALSE]   # `!=` made an NA date an all-NA row

  dplyr::bind_rows(existing, new_rows[names(existing)])
}

#' Count K-12 teacher postings per category, run and district
#'
#' Counts distinct posting ids, not title and location text: two vacancies
#' can share their display text and still be two postings (SchoolSpring).
#' @param k12jobs Teacher rows (`build_k12_teacher_rows()`).
#' @param id_col The posting-id column.
#' @return `Broad_Category`, `Archive_Date`, `District`, `sum`.
#' @export
summarize_k12_posting_counts <- function(k12jobs, id_col = "Posting_ID") {
  required <- c("Broad_Category", "Archive_Date", "District", id_col)
  missing <- setdiff(required, names(k12jobs))
  if (length(missing) > 0) {
    stop("summarize_k12_posting_counts(): missing required column(s): ", paste(missing, collapse = ", "))
  }
  k12jobs %>%
    dplyr::group_by(Broad_Category, Archive_Date, District) %>%
    dplyr::summarize(sum = dplyr::n_distinct(.data[[id_col]]), .groups = "drop")
}

#' `combinedclean.csv`'s rows from one run's counted K-12 postings
#'
#' Placeholder board entries out, the current classifier, posted pay guarded
#' (`drop_implausible_pay()`), and the OEWS pay estimate for the posting's
#' role in its district's county: the registry's county, else the county of
#' the district a feed posting names, statewide where neither is known. A
#' feed posting's title can be only a subject, so its description's opening
#' refines the bucket (`k12_role_hint()`).
#'
#' Callers dedupe first. `Posting_ID` must already be on the rows (under the
#' state's own name, see `columns`); it is carried through, never rebuilt.
#' @param rows One run's counted postings, with the state's raw column names.
#' @param oews_wages The OEWS wage table (`attach_oews_estimates()`).
#' @param district_county Data frame `District`, `County` (the registry's).
#' @param state The state's postal code, for the statewide OEWS fallback.
#' @param columns The state's raw names for schema columns, as
#'   `c(<schema name> = "<raw name>")`: any of `Posting_ID`, `Posting_Source`,
#'   `Posting_Identity_Method`, `Feed_District`, `Also_On_Feed`, and
#'   `Feed_Description` (a feed posting's description, used for `Role_Hint`
#'   and not written).
#' @param feed_counties Named character vector, county by feed district name,
#'   or NULL.
#' @param canonicalize_district The state's district-name corrections.
#' @param feed_label The feed's `Posting_Source` value; defaults to the
#'   session's `state_config(feed = )` label. Role hints are read only for
#'   these rows.
#' @return A data frame with `K12_POSTING_COLUMNS`.
#' @export
build_k12_combinedclean <- function(rows, oews_wages, district_county, state, columns = character(0),
                                    feed_counties = NULL, canonicalize_district = function(x) x,
                                    feed_label = state_feed("label")) {
  absent <- names(columns)[!columns %in% names(rows) & !names(columns) %in% names(rows)]
  if (length(absent) > 0 && nrow(rows) > 0) {
    message("build_k12_combinedclean(): no ", paste0(columns[absent], collapse = ", "), " column (mapped to ",
            paste(absent, collapse = ", "), "); left NA")
  }
  if (is.null(feed_label) && is.null(current_state_config())) {
    warning("build_k12_combinedclean(): no state_config() loaded and no feed_label, so no feed rows get a Role_Hint")
  }
  for (to in names(columns)) {
    from <- columns[[to]]
    if (from %in% names(rows) && from != to) {
      if (to %in% names(rows)) stop("build_k12_combinedclean(): rows hold both ", from, " and ", to)
      names(rows)[names(rows) == from] <- to
    }
  }
  if (!"Posting_ID" %in% names(rows)) stop("build_k12_combinedclean(): rows need a posting id (Posting_ID, or name it in `columns`)")
  for (col in c("Posting_Source", "Posting_Identity_Method", "Feed_District", "Close_Date", "Feed_Description")) {
    if (!col %in% names(rows)) rows[[col]] <- NA_character_
  }
  if (!"Also_On_Feed" %in% names(rows)) rows$Also_On_Feed <- NA
  rows$Close_Date <- as.character(rows$Close_Date)
  is_feed <- if (is.null(feed_label)) rep(FALSE, nrow(rows)) else !is.na(rows$Posting_Source) & rows$Posting_Source == feed_label
  rows$Role_Hint <- NA_character_
  hinted <- is_feed & !is.na(rows$Feed_Description) & nzchar(rows$Feed_Description)
  if (any(hinted)) rows$Role_Hint[hinted] <- k12_role_hint(rows$Feed_Description[hinted])

  out <- rows %>%
    dplyr::filter(!is_placeholder_posting(title)) %>%
    dplyr::mutate(position = classify_k12_position(title, Role_Hint),
                  District = canonicalize_district(District)) %>%
    ensure_posted_pay_columns() %>%
    drop_implausible_pay()
  county <- district_county$County[match(out$District, district_county$District)]
  if (!is.null(feed_counties)) county <- dplyr::coalesce(county, unname(feed_counties[as.character(out$Feed_District)]))
  out <- attach_oews_estimates(out, oews_wages, county, state)
  out %>% dplyr::select(dplyr::all_of(K12_POSTING_COLUMNS))
}

#' `k12jobanalysis.csv`'s rows: the teacher postings, with subject and broad category
#'
#' @param combinedclean `build_k12_combinedclean()`'s result.
#' @return A data frame with `K12_TEACHER_COLUMNS`.
#' @export
build_k12_teacher_rows <- function(combinedclean) {
  combinedclean %>%
    dplyr::filter(position == "Teacher") %>%
    dplyr::mutate(Category = classify_k12_subject(title),
                  Broad_Category = classify_k12_broad_category(Category)) %>%
    dplyr::select(dplyr::all_of(K12_TEACHER_COLUMNS))
}
