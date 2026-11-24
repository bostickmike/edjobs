# SmartSites (ParentSquare) district pages: a CMS-family routine (2026-11-24).
#
# One reader for district job pages built with ParentSquare's SmartSites, replacing a scraper per site
# (claude-notes/2026-11-24_smartsites-family/: three boards, each on one of three widgets). SmartSites renders
# its widgets with the same markup on every site, so the reader needs no per-district settings: it reads every
# widget it finds. Free text typed into an editor block has no markup per posting and isn't read here (a
# per-source selector, `parse_css_board()`, reads that).

# Blank application forms sit in the same widgets as postings, with no markup to tell them apart ("Certified
# Application", "Certified and Classified Applications"), and galleries carry decorative "logo" images.
SMARTSITES_EXCLUDE <- "application|^logo$"

#' Fetch a SmartSites (ParentSquare) district page's job postings
#'
#' A plain request for the page, read by [parse_smartsites_postings()].
#' @param url The district's job page.
#' @param location The postings' location (the registry's City), or NA.
#' @return Title, Location, Posted_Date (YYYY-MM-DD or NA), Link (the page): one row per posting.
#' @export
fetch_smartsites_postings <- function(url, location) {
  resp <- httr2::request(url) %>%
    httr2::req_user_agent(POSTING_STATUS_UA) %>%
    perform_with_retry(is_transient = is_transient_gateway_error)
  parse_smartsites_postings(httr2::resp_body_string(resp), url, location)
}

#' Parse a SmartSites (ParentSquare) district page
#'
#' Reads each SmartSites widget on the page, in this order:
#' * documents (`li.ss-document-item`): the title in `data-searchText`, the date as "added YYYY-MM-DD" in the
#'   link's `aria-label`;
#' * image gallery (`li.picseries_image img[alt]`): the image's alt text, no date (some boards post each
#'   opening as a flyer image whose alt text is its only title);
#' * posts (`a.ss-post-page-row`): the title in `h2.ss-post-title`, the date in `div.ss-post-date`
#'   ("July 10, 2026").
#'
#' Titles have their spaces collapsed; blank titles, application forms and "logo" images are dropped. Every
#' row's Link is the page (the widgets' own links are often a form or a file shared by several postings).
#' A SmartSites page with none of the widgets reads as zero rows; a page with no SmartSites markup at all (no
#' `ss-` id or class: the site has moved off SmartSites) or a body with no HTML is an error, so it isn't logged
#' as an empty board.
#' @param html_text The page's HTML.
#' @inheritParams fetch_smartsites_postings
#' @return See [fetch_smartsites_postings()].
#' @export
parse_smartsites_postings <- function(html_text, url, location) {
  stop_if_no_html(html_text)
  page <- rvest::read_html(html_text[1])
  if (length(rvest::html_elements(page, "[id^='ss-'], [class^='ss-'], [class*=' ss-']")) == 0)
    stop("no SmartSites markup on the page (no ss- ids or classes): ", url, call. = FALSE)

  docs <- rvest::html_elements(page, "li.ss-document-item")
  # libxml2 lowercases attribute names: the page's data-searchText is data-searchtext. html_element() (singular)
  # keeps one label per item, NA when an item has no link, so each date stays with its own title.
  doc_label <- rvest::html_attr(rvest::html_element(docs, "a.ss-document-link"), "aria-label")
  doc_date <- ifelse(!is.na(doc_label) & grepl("added \\d{4}-\\d{2}-\\d{2}", doc_label),
                     sub(".*added (\\d{4}-\\d{2}-\\d{2}).*", "\\1", doc_label), NA_character_)

  # The gallery template self-closes its <a>, so the <figure>/<img> are the link's siblings, not inside it.
  imgs <- rvest::html_elements(page, "li.picseries_image img[alt]")

  posts <- rvest::html_elements(page, "a.ss-post-page-row")
  post_date <- smartsites_post_date(rvest::html_text2(rvest::html_element(posts, "div.ss-post-date")))

  title <- smartsites_clean(c(rvest::html_attr(docs, "data-searchtext"), rvest::html_attr(imgs, "alt"),
                              rvest::html_text2(rvest::html_element(posts, "h2.ss-post-title"))))
  date <- c(doc_date, rep(NA_character_, length(imgs)), post_date)
  keep <- !is.na(title) & nzchar(title) & !grepl(SMARTSITES_EXCLUDE, title, ignore.case = TRUE)
  n <- sum(keep)
  data.frame(Title = title[keep], Location = rep(as.character(location), n), Posted_Date = date[keep],
             Link = rep(url, n), stringsAsFactors = FALSE)
}

smartsites_clean <- function(x) trimws(gsub("[[:space:]\u00a0]+", " ", x))

# "July 10, 2026" -> "2026-07-10", by English month name or abbreviation whatever the locale (as.Date's %B reads
# the locale's names).
smartsites_post_date <- function(x) {
  x <- smartsites_clean(x)
  m <- regmatches(x, regexec("^([A-Za-z]+)\\.? (\\d{1,2}), (\\d{4})", x))
  vapply(m, function(p) {
    month <- if (length(p) == 4) match(tolower(p[2]), tolower(c(month.name, month.abb))) %% 12 else NA_integer_
    if (is.na(month)) return(NA_character_)
    if (month == 0) month <- 12
    d <- tryCatch(as.Date(sprintf("%s-%02d-%02d", p[4], month, as.integer(p[3]))), error = function(e) NA)
    if (is.na(d)) NA_character_ else format(d)
  }, character(1))
}
