# Finalsite district pages: the fourth CMS-family routine (2026-11-14).
#
# One routine for the district employment pages built with Finalsite,
# replacing a scraper per district (claude-notes/2026-11-14_finalsite-family/).
# A plain request gets the whole page. The text is Finalsite's page content
# (.fsPageContent) without the site header, footer, menus, tab navigation and
# hidden copies; element <header>s inside the content stay (they hold element
# titles), and a resource link is read by its data-resource-title. Titles come
# from the shared title line filter with the same optional per-district rules;
# a title that is itself a link (a posting's document) keeps that link.

#' Fetch a Finalsite district page's job postings
#'
#' @inheritParams fetch_googlesites_postings
#' @return See [fetch_googlesites_postings()].
#' @export
fetch_finalsite_postings <- function(url, location, start = NA, stop = NA, skip_section = NA) {
  resp <- httr2::request(url) %>%
    httr2::req_user_agent(POSTING_STATUS_UA) %>%
    perform_with_retry(is_transient = is_transient_gateway_error)
  parse_finalsite_postings(httr2::resp_body_string(resp), url, location, start, stop, skip_section)
}

#' Parse a Finalsite district page
#'
#' @param html_text The page's HTML.
#' @inheritParams fetch_googlesites_postings
#' @return See [fetch_googlesites_postings()].
#' @export
parse_finalsite_postings <- function(html_text, url, location, start = NA, stop = NA, skip_section = NA) {
  doc <- finalsite_doc(html_text)
  text <- if (is.null(doc)) NA_character_ else finalsite_doc_text(doc)
  rows <- if (is.na(text)) apptegy_title_rows(NULL) else apptegy_title_rows(text, na_rule(start), na_rule(stop), na_rule(skip_section))
  title_rows_postings(rows, url, location, doc)
}

#' A Finalsite page's text
#'
#' @param html_text The page's HTML.
#' @return One string, or NA when the HTML can't be read.
#' @export
finalsite_page_text <- function(html_text) {
  doc <- finalsite_doc(html_text)
  if (is.null(doc)) NA_character_ else finalsite_doc_text(doc)
}

# The page with resource links read by their title and the site chrome removed.
finalsite_doc <- function(html_text) {
  if (is.null(html_text) || length(html_text) == 0 || is.na(html_text[1]) || !nzchar(trimws(html_text[1]))) return(NULL)
  doc <- tryCatch(rvest::read_html(html_text), error = function(e) NULL)
  if (is.null(doc)) return(NULL)
  for (a in rvest::html_elements(doc, "a[data-resource-title]")) {
    t <- rvest::html_attr(a, "data-resource-title")
    if (!is.na(t) && nzchar(t)) xml2::xml_text(a) <- paste0("\n", t, "\n")
  }
  xml2::xml_remove(rvest::html_elements(doc, paste("#fsHeader, #fsFooter, #fsMenu, .fsNavigation, .fsTabsNav, .fsStateHidden,",
                                                   "nav, script, style, noscript, [role=navigation], body > header, body > footer")))
  doc
}

finalsite_doc_text <- function(doc) {
  main <- rvest::html_elements(doc, ".fsPageContent, #fsPageContent")
  node <- if (length(main)) main[[1]] else rvest::html_element(doc, "body")
  if (inherits(node, "xml_missing")) NA_character_ else rvest::html_text2(node)
}
