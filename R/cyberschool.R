# CyberSchool (IES) district sites: the first CMS-family routine (2026-11-14).
#
# One routine for every district whose site runs on IES's CyberSchool CMS,
# replacing a scraper per district (claude-notes/2026-11-12_cms-families.md,
# 2026-11-12_cyberschool-pilot/). Two generations of the platform are read:
#   - classic "CyberSchool 2.0" (generator IESAres404): each job is a
#     .listItem_holder with a heading link, "Posted on <day>" and sometimes a
#     "Deadline:" line, at /District/Jobs/;
#   - the newer IES platform (Vaughn moved 2026-11, its /District/Jobs/ now
#     redirects to /jobs): each job is a list item whose heading links to
#     /jobs/<slug>, with a "Published: MM/DD/YYYY" line.
# A page that holds its whole list as free text in one item (Nashua, Frazer,
# Lavina, Hinsdale) is not read here: it needs the shared title line filter.

#' Fetch a CyberSchool district's job postings
#'
#' @param url The district's jobs page (registry `Job_Link`).
#' @param location The posting location to record (registry `City`).
#' @return Data frame: `Title`, `Location`, `Posted_Date` (ISO date or NA),
#'   `Closing_Date` (ISO date or NA; "until filled" is NA), `Link`.
#' @export
fetch_cyberschool_postings <- function(url, location) {
  resp <- httr2::request(url) %>%
    httr2::req_user_agent(POSTING_STATUS_UA) %>%
    perform_with_retry(is_transient = is_transient_gateway_error)
  # A moved site redirects; links resolve against the page actually served.
  parse_cyberschool_postings(httr2::resp_body_string(resp), resp$url %||% url, location)
}

#' Parse a CyberSchool jobs page
#'
#' @param html_text The page's HTML.
#' @param url The page's URL (relative links resolve against it).
#' @param location The posting location to record.
#' @return See [fetch_cyberschool_postings()].
#' @export
parse_cyberschool_postings <- function(html_text, url, location) {
  empty <- cyberschool_empty()
  if (is.null(html_text) || !nzchar(trimws(html_text))) return(empty)
  doc <- tryCatch(rvest::read_html(html_text), error = function(e) NULL)
  if (is.null(doc)) return(empty)

  out <- cyberschool_classic_items(doc, url, location)
  if (nrow(out) == 0) out <- cyberschool_ies_items(doc, url, location)
  unique(out)
}

cyberschool_empty <- function() {
  data.frame(Title = character(0), Location = character(0), Posted_Date = character(0),
             Closing_Date = character(0), Link = character(0), stringsAsFactors = FALSE)
}

cyberschool_text <- function(x) trimws(gsub("[ \r\n\t ]+", " ", x))

# Classic: one .listItem_holder per job.
cyberschool_classic_items <- function(doc, url, location) {
  holders <- rvest::html_elements(doc, ".listItem_holder")
  if (length(holders) == 0) return(cyberschool_empty())
  link_nodes <- rvest::html_element(holders, ".listItem_headingLink")
  titles <- cyberschool_text(rvest::html_text(link_nodes))
  missing <- is.na(titles) | !nzchar(titles)
  titles[missing] <- cyberschool_text(rvest::html_text(rvest::html_element(holders[missing], ".listItem_heading")))
  hrefs <- rvest::html_attr(link_nodes, "href")
  posted <- cyberschool_text(rvest::html_text(rvest::html_element(holders, ".listItem_metaPostedDate")))
  deadline <- vapply(holders, function(h) {
    d <- cyberschool_text(rvest::html_text(rvest::html_elements(h, ".listItem_metaDate")))
    d <- d[grepl("^Deadline:", d)]
    if (length(d)) sub("^Deadline:\\s*", "", d[1]) else NA_character_
  }, character(1))

  keep <- !is.na(titles) & nzchar(titles) & !grepl("^(?:Title|Jobs)$", titles, ignore.case = TRUE)
  if (!any(keep)) return(cyberschool_empty())
  links <- ifelse(is.na(hrefs) | !nzchar(hrefs), url, xml2::url_absolute(hrefs, url))
  data.frame(Title = titles[keep], Location = rep(location, sum(keep)),
             Posted_Date = cyberschool_date(posted[keep]), Closing_Date = cyberschool_date(deadline[keep]),
             Link = links[keep], stringsAsFactors = FALSE)
}

# Newer IES platform: a list item per job, heading link to /jobs/<slug>.
cyberschool_ies_items <- function(doc, url, location) {
  # The heading link carries data-flux-link; the item's "See Description"
  # button links to the same page. Not keyed on <h2>: the page puts a <p>
  # inside the <h2>, and the HTML parser moves it (and the link) out.
  anchors <- rvest::html_elements(doc, xpath = "//li//a[@data-flux-link][contains(@href, '/jobs/')]")
  anchors <- anchors[grepl("/jobs/[^/?#]+/?$", rvest::html_attr(anchors, "href"))]
  if (length(anchors) == 0) return(cyberschool_empty())
  titles <- cyberschool_text(rvest::html_text(anchors))
  hrefs <- xml2::url_absolute(rvest::html_attr(anchors, "href"), url)
  posted <- vapply(anchors, function(a) {
    item <- xml2::xml_find_first(a, "ancestor::li[1]")
    txt <- cyberschool_text(rvest::html_text(item))
    m <- regmatches(txt, regexpr("Published:\\s*[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}", txt))
    if (length(m)) sub("^Published:\\s*", "", m) else NA_character_
  }, character(1))
  keep <- nzchar(titles)
  if (!any(keep)) return(cyberschool_empty())
  data.frame(Title = titles[keep], Location = rep(location, sum(keep)),
             Posted_Date = cyberschool_date(posted[keep]), Closing_Date = NA_character_,
             Link = hrefs[keep], stringsAsFactors = FALSE)
}

# "Friday, Aug 14, 2026 12:00:00 AM", "09/09/2026", "Nov 20, 2026" -> ISO; else NA.
cyberschool_date <- function(x) {
  x <- sub("\\s+[0-9]{1,2}:[0-9]{2}(:[0-9]{2})?\\s*[AP]M$", "", cyberschool_text(x))
  x <- sub("^[A-Za-z]+day,\\s*", "", x)
  out <- rep(NA_character_, length(x))
  # Month names by table, not %b: as.Date's %b follows the system locale.
  mon <- c(jan = 1, feb = 2, mar = 3, apr = 4, may = 5, jun = 6, jul = 7, aug = 8, sep = 9, oct = 10, nov = 11, dec = 12)
  named <- regmatches(x, regexec("^([A-Za-z]{3})[A-Za-z]*\\.? ([0-9]{1,2}),? ([0-9]{4})$", x))
  slashed <- regmatches(x, regexec("^([0-9]{1,2})/([0-9]{1,2})/([0-9]{4})$", x))
  iso <- regmatches(x, regexec("^([0-9]{4})-([0-9]{2})-([0-9]{2})$", x))
  for (i in seq_along(x)) {
    ymd <- if (length(named[[i]]) == 4) {
      c(named[[i]][4], mon[tolower(named[[i]][2])], named[[i]][3])
    } else if (length(slashed[[i]]) == 4) {
      slashed[[i]][c(4, 2, 3)]
    } else if (length(iso[[i]]) == 4) {
      iso[[i]][2:4]
    } else NULL
    if (is.null(ymd) || anyNA(ymd)) next
    d <- tryCatch(as.Date(sprintf("%04d-%02d-%02d", as.integer(ymd[1]), as.integer(ymd[2]), as.integer(ymd[3]))),
                  error = function(e) NA)
    if (!is.na(d)) out[i] <- format(d, "%Y-%m-%d")
  }
  out
}
