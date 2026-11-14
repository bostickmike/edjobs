# Joomla district pages: a CMS-family routine (2026-11-14).
#
# One routine for district employment pages on Joomla (with or without the
# UIkit/YOOtheme templates), replacing a scraper per district
# (claude-notes/2026-11-14_small-families/joomla/). The same reading as the
# Edlio routine: a plain request; district_page_text() (Joomla's module menus
# and the UIkit navigation, toolbar and footer removed); the shared title line
# filter with the same optional per-district rules; a title that is itself a
# link keeps the link.

#' Fetch a Joomla district page's job postings
#'
#' @inheritParams fetch_googlesites_postings
#' @return See [fetch_googlesites_postings()].
#' @export
fetch_joomla_postings <- function(url, location, start = NA, stop = NA, skip_section = NA) {
  resp <- httr2::request(url) %>%
    httr2::req_user_agent(POSTING_STATUS_UA) %>%
    # Roberts's Cloudflare front answers 502/520 to CI runners at times: a longer budget (~1 min), as its old scraper had
    perform_with_retry(max_tries = 6, is_transient = is_transient_gateway_error)
  parse_joomla_postings(httr2::resp_body_string(resp), url, location, start, stop, skip_section)
}

#' Parse a Joomla district page
#'
#' A body with no HTML at all (a maintenance notice) is an error, not zero postings.
#' @param html_text The page's HTML.
#' @inheritParams fetch_googlesites_postings
#' @return See [fetch_googlesites_postings()].
#' @export
parse_joomla_postings <- function(html_text, url, location, start = NA, stop = NA, skip_section = NA) {
  parse_edlio_postings(html_text, url, location, start, stop, skip_section)
}
