# Edlio district pages: a CMS-family routine (2026-11-14).
#
# One routine for district employment pages on Edlio, replacing a scraper per
# district (claude-notes/2026-11-14_small-families/edlio/). A plain request;
# the text is the body without navigation, header, footer, sidebars, menus and
# scripts; the shared title line filter with the same optional per-district
# rules; a title that is itself a link keeps the link.

#' Fetch an Edlio district page's job postings
#'
#' @inheritParams fetch_googlesites_postings
#' @return See [fetch_googlesites_postings()].
#' @export
fetch_edlio_postings <- function(url, location, start = NA, stop = NA, skip_section = NA) {
  resp <- httr2::request(url) %>%
    httr2::req_user_agent(POSTING_STATUS_UA) %>%
    perform_with_retry(is_transient = is_transient_gateway_error)
  parse_edlio_postings(httr2::resp_body_string(resp), url, location, start, stop, skip_section)
}

#' Parse an Edlio district page
#'
#' @param html_text The page's HTML.
#' @inheritParams fetch_googlesites_postings
#' @return See [fetch_googlesites_postings()].
#' @export
parse_edlio_postings <- function(html_text, url, location, start = NA, stop = NA, skip_section = NA) {
  doc <- district_page_doc(html_text)
  text <- if (is.null(doc)) NA_character_ else district_page_doc_text(doc)
  rows <- if (is.na(text)) apptegy_title_rows(NULL) else apptegy_title_rows(text, na_rule(start), na_rule(stop), na_rule(skip_section))
  title_rows_postings(rows, url, location, doc)
}

#' A district page's text, without the site's chrome
#'
#' The body without navigation, header, footer, sidebars, menus (WordPress,
#' Joomla, UIkit), widgets, breadcrumbs and scripts, each table cell on its own
#' line. The page text the Edlio and Joomla routines read.
#' @param html_text The page's HTML.
#' @return One string, or NA when the HTML can't be read.
#' @export
district_page_text <- function(html_text) {
  doc <- district_page_doc(html_text)
  if (is.null(doc)) NA_character_ else district_page_doc_text(doc)
}

CELL_END <- "\u241e"

district_page_doc <- function(html_text) {
  if (is.null(html_text) || length(html_text) == 0 || is.na(html_text[1]) || !nzchar(trimws(html_text[1]))) return(NULL)
  # A 200 body with no HTML ("site under maintenance", Roberts 2026-11-04) is a failed fetch, not an empty
  # page; read_html() would take it for a file path and the page would read as zero postings.
  if (!grepl("<", html_text[1], fixed = TRUE))
    stop("page returned no HTML: \"", substr(trimws(html_text[1]), 1, 80), "\"", call. = FALSE)
  # Block ends (table cells, list items, paragraphs, divs, headings, <br>) get a marker that becomes a line
  # break after html_text2(), which can run them together ("CustodianFull Time Custodial ...", Roberts, Joliet).
  html_text <- gsub("(?i)(</(t[dh]|li|p|div|h[1-6])>|<br\\s*/?>|<li[\\s>])", paste0(CELL_END, "\\1"), html_text[1], perl = TRUE)
  doc <- tryCatch(rvest::read_html(html_text), error = function(e) NULL)
  if (is.null(doc)) return(NULL)
  xml2::xml_remove(rvest::html_elements(doc, paste("nav, header, footer, aside, script, style, noscript, [role=navigation],",
                                                   ".widget, .sidebar, .menu, .breadcrumbs, .breadcrumb, .moduletable, .mod-menu,",
                                                   ".uk-nav, .uk-navbar, .uk-offcanvas, .tm-header, .tm-toolbar, .tm-footer")))
  doc
}

district_page_doc_text <- function(doc) {
  body <- rvest::html_element(doc, "body")
  if (inherits(body, "xml_missing")) NA_character_ else gsub(paste0("[ \\t]*", CELL_END, "[ \\t]*"), "\n", rvest::html_text2(body), perl = TRUE)
}
