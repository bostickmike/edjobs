# Google Sites district pages: the third CMS-family routine (2026-11-14).
#
# One routine for the district employment pages built with Google Sites,
# replacing a scraper per district (claude-notes/2026-11-14_googlesites-family/:
# measured against each old parser on its fixtures and the 11-14 live pages).
# A plain request gets the whole page; the text is the body with the site's
# navigation, header, footer and scripts removed, ending at Google's "Report
# abuse" footer. Titles come from the shared title line filter
# (apptegy_title_lines()), with the same optional per-district rules.

#' Fetch a Google Sites district page's job postings
#'
#' @param url The district's jobs page (registry `Job_Link`).
#' @param location The posting location to record (registry `City`).
#' @param start,stop,skip_section Optional per-district regexes, see
#'   [apptegy_title_lines()].
#' @return Data frame: `Title`, `Location`, `Posted_Date` (NA), `Link` (the
#'   page), and the posted pay columns when a title's own line states pay.
#' @export
fetch_googlesites_postings <- function(url, location, start = NA, stop = NA, skip_section = NA) {
  resp <- httr2::request(url) %>%
    httr2::req_user_agent(POSTING_STATUS_UA) %>%
    perform_with_retry(is_transient = is_transient_gateway_error)
  parse_googlesites_postings(httr2::resp_body_string(resp), url, location, start, stop, skip_section)
}

#' Parse a Google Sites district page
#'
#' @param html_text The page's HTML.
#' @inheritParams fetch_googlesites_postings
#' @return See [fetch_googlesites_postings()].
#' @export
parse_googlesites_postings <- function(html_text, url, location, start = NA, stop = NA, skip_section = NA) {
  text <- googlesites_page_text(html_text)
  rows <- if (is.na(text)) apptegy_title_rows(NULL) else apptegy_title_rows(text, na_rule(start), na_rule(stop), na_rule(skip_section))
  title_rows_postings(rows, url, location)
}

#' A Google Sites page's text
#'
#' The body without navigation, header, footer, scripts and styles (the
#' site's menu repeats every page name), ending at Google's "Report abuse"
#' footer. Google Sites' `role=main` element is only a skip-link target, so
#' the body is used.
#' @param html_text The page's HTML.
#' @return One string, or NA when the HTML can't be read.
#' @export
googlesites_page_text <- function(html_text) {
  if (is.null(html_text) || length(html_text) == 0 || is.na(html_text[1]) || !nzchar(trimws(html_text[1]))) return(NA_character_)
  doc <- tryCatch(rvest::read_html(html_text), error = function(e) NULL)
  if (is.null(doc)) return(NA_character_)
  xml2::xml_remove(rvest::html_elements(doc, "nav, header, footer, script, style, noscript, [role=navigation]"))
  body <- rvest::html_element(doc, "body")
  if (inherits(body, "xml_missing")) return(NA_character_)
  sub("\\s*-?\\s*(Google Sites)?\\s*Report abuse[\\s\\S]*$", "", rvest::html_text2(body), perl = TRUE)
}
