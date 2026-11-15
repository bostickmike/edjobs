# Google Doc job lists: a CMS-family routine (2026-11-14).
#
# Some districts keep their openings in a published Google Doc that their own
# site links (MT's Victor, Superior, Bainville, Creston;
# claude-notes/2026-11-14_googledocs/). One routine reads each doc's text
# through the shared title line filter, with the same optional per-district
# rules. A native Google Doc is read from its plain-text export; a document
# uploaded to Drive (a .docx, as Victor's is) has no export, so its preview
# page's text is read instead.

#' Fetch the job postings listed in one or more Google Docs
#'
#' @param doc_ids The docs' ids (the part after `/document/d/`), as a character
#'   vector or one string separated by ";" (registry `Slug`).
#' @param location The posting location to record (registry `City`).
#' @param link The page a person would visit (registry `Job_Link`), each
#'   posting's link.
#' @param start,stop,skip_section Optional per-district rules (see
#'   [apptegy_title_lines()]), applied to each doc.
#' @return Data frame: `Title`, `Location`, `Posted_Date` (the doc's "updated
#'   M/D/YYYY" date as ISO, as Victor's old scraper read it; else NA), `Link`,
#'   and the posted pay columns.
#' @export
fetch_googledoc_postings <- function(doc_ids, location, link, start = NA, stop = NA, skip_section = NA) {
  ids <- googledoc_ids(doc_ids)
  if (!length(ids)) stop("fetch_googledoc_postings: no doc id")
  texts <- vapply(ids, googledoc_text, "")
  parse_googledoc_postings(texts, location, link, start, stop, skip_section)
}

#' Parse Google Doc job lists
#'
#' @param texts Each doc's text (one element per doc).
#' @inheritParams fetch_googledoc_postings
#' @return See [fetch_googledoc_postings()].
#' @export
parse_googledoc_postings <- function(texts, location, link, start = NA, stop = NA, skip_section = NA) {
  per_doc <- lapply(texts, function(t) {
    if (is.na(t) || !nzchar(t)) return(cbind(apptegy_title_rows(NULL), Updated = character(0)))
    rows <- apptegy_title_rows(t, na_rule(start), na_rule(stop), na_rule(skip_section))
    cbind(rows, Updated = rep(googledoc_updated(t), nrow(rows)))
  })
  rows <- do.call(rbind, c(list(cbind(apptegy_title_rows(NULL), Updated = character(0))), per_doc))
  rows <- rows[!duplicated(rows$Title), , drop = FALSE]
  out <- title_rows_postings(rows[, c("Title", "Line")], link, location)
  out$Posted_Date <- as.character(rows$Updated)
  out
}

# A doc's own "updated 10/1/2026" line, as an ISO date (NA when none).
googledoc_updated <- function(text) {
  m <- regmatches(text, regexpr("(?i)\\bupdated:?\\s+[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}", text, perl = TRUE))
  if (!length(m)) return(NA_character_)
  d <- as.Date(sub("(?i)^updated:?\\s+", "", m, perl = TRUE), format = "%m/%d/%Y")
  if (is.na(d)) NA_character_ else format(d)
}

#' A Google Doc preview page's text
#'
#' The preview page (`/document/d/<id>/preview`) carries the document's text
#' as `"s":"..."` strings in its script data; these are joined in order.
#' @param html_text The preview page's HTML.
#' @return One string ("" when none is found).
#' @export
googledoc_preview_text <- function(html_text) {
  if (is.null(html_text) || !nzchar(trimws(html_text))) return("")
  strings <- regmatches(html_text, gregexpr("\"s\":\"([^\"]+)\"", html_text))[[1]]
  if (length(strings) == 0) return("")
  parts <- vapply(strings, function(s) {
    txt <- sub("^\"s\":\"", "", sub("\"$", "", s))
    txt <- gsub("\\\\n", "\n", txt)
    txt <- gsub("\\\\u0027", "'", txt)
    gsub("\\\\u0010|\\\\u0011|\\\\u0012|\\\\u001c", "", txt)
  }, character(1))
  paste(parts, collapse = "\n")
}

googledoc_ids <- function(doc_ids) {
  ids <- trimws(unlist(strsplit(as.character(doc_ids), ";", fixed = TRUE)))
  ids[!is.na(ids) & nzchar(ids)]
}

# One doc's text: the plain-text export, else (an uploaded file) the preview page's text.
googledoc_text <- function(id) {
  get <- function(u) {
    resp <- httr2::request(u) %>%
      httr2::req_user_agent(POSTING_STATUS_UA) %>%
      httr2::req_error(is_error = function(resp) FALSE) %>%
      perform_with_retry(is_transient = is_transient_gateway_error)
    if (httr2::resp_status(resp) >= 400) NA_character_ else httr2::resp_body_string(resp)
  }
  txt <- get(sprintf("https://docs.google.com/document/d/%s/export?format=txt", id))
  if (!is.na(txt) && nzchar(trimws(txt)) && !grepl("^\\s*<", txt)) return(txt)
  prev <- get(sprintf("https://docs.google.com/document/d/%s/preview", id))
  if (is.na(prev)) stop("fetch_googledoc_postings: doc ", id, " has neither a text export nor a preview")
  out <- googledoc_preview_text(prev)
  if (!nzchar(out)) stop("fetch_googledoc_postings: doc ", id, "'s preview has no text")
  out
}
