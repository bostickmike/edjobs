# WordPress district pages: the fifth CMS-family routine (2026-11-14).
#
# One routine for district employment pages on WordPress (often the Divi
# theme), replacing a scraper per district (claude-notes/2026-11-14_wordpress-family/).
# A plain request gets the whole page; the text is the post/page content
# (.entry-content, else Divi's main area, main, article, #content) without
# navigation, header, footer, sidebars and widgets. Titles come from the shared
# title line filter with the same optional per-district rules; a title that is
# itself a link keeps the link.

#' Fetch a WordPress district page's job postings
#'
#' @inheritParams fetch_googlesites_postings
#' @return See [fetch_googlesites_postings()].
#' @export
fetch_wordpress_postings <- function(url, location, start = NA, stop = NA, skip_section = NA) {
  resp <- httr2::request(url) %>%
    httr2::req_user_agent(POSTING_STATUS_UA) %>%
    perform_with_retry(is_transient = is_transient_gateway_error)
  parse_wordpress_postings(httr2::resp_body_string(resp), url, location, start, stop, skip_section)
}

#' Parse a WordPress district page
#'
#' @param html_text The page's HTML.
#' @inheritParams fetch_googlesites_postings
#' @return See [fetch_googlesites_postings()].
#' @export
parse_wordpress_postings <- function(html_text, url, location, start = NA, stop = NA, skip_section = NA) {
  doc <- wordpress_doc(html_text)
  text <- if (is.null(doc)) NA_character_ else wordpress_doc_text(doc)
  rows <- if (is.na(text)) apptegy_title_rows(NULL) else apptegy_title_rows(text, na_rule(start), na_rule(stop), na_rule(skip_section))
  title_rows_postings(rows, url, location, doc)
}

#' A WordPress page's text
#'
#' @param html_text The page's HTML.
#' @return One string, or NA when the HTML can't be read.
#' @export
wordpress_page_text <- function(html_text) {
  doc <- wordpress_doc(html_text)
  if (is.null(doc)) NA_character_ else wordpress_doc_text(doc)
}

wordpress_doc <- function(html_text) {
  if (is.null(html_text) || length(html_text) == 0 || is.na(html_text[1]) || !nzchar(trimws(html_text[1]))) return(NULL)
  doc <- tryCatch(rvest::read_html(html_text), error = function(e) NULL)
  if (is.null(doc)) return(NULL)
  xml2::xml_remove(rvest::html_elements(doc, paste("nav, header, footer, aside, script, style, noscript, [role=navigation],",
                                                   ".widget, .sidebar, #secondary, #sidebar, .menu, #et-top-navigation,",
                                                   "#main-header, #main-footer, .breadcrumbs, .breadcrumb")))
  doc
}

wordpress_doc_text <- function(doc) {
  for (sel in c(".entry-content", "#et-main-area", "main", "#main", "article", "#content", "body")) {
    m <- rvest::html_elements(doc, sel)
    if (length(m)) return(paste(rvest::html_text2(m), collapse = "\n"))
  }
  NA_character_
}
