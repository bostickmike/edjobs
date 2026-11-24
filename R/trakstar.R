# Trakstar Hire (formerly Recruiterbox): <org>.hire.trakstar.com. First used for St. Labre Indian School's board
# (Ashland, MT, 2026-11-23), which also lists Pretty Eagle Catholic Academy (St. Xavier) and St. Charles Mission
# School (Pryor).

#' Fetch a Trakstar Hire board's postings
#'
#' The board's list page (`https://<org>.hire.trakstar.com/`, then `?p=2`, ...) gives each opening's title, city
#' and state on a card; 25 cards a page, and a page past the last has none (measured 2026-11-23: St. Labre's 38
#' openings on two pages, `?p=3` empty). With `details`, each posting's own page (`/jobs/<id>/`) is read for its
#' schema.org JobPosting block ([parse_jobposting_jsonld()]): `datePosted` on 38 of 38 pages, no `validThrough` and
#' no `baseSalary` on any; the pay is in the description text ("Salary Grade: 4B ($13.44-$15.47-$17.49)/hour"),
#' read by [extract_posted_pay()]. Titles come from the cards: the block's title is HTML-escaped twice ("Mission
#' &amp;amp; Ministry").
#' @param board_url The board's address, `https://<org>.hire.trakstar.com/` (any path is dropped).
#' @param details Read each posting's page for its posted date, closing date and pay.
#' @param detail_delay Seconds between posting pages.
#' @param max_pages Stop after this many list pages (with a warning).
#' @return Title, Location ("Ashland, Montana"), Posted_Date (YYYY-MM-DD, or NA without details), Posting_ID
#'   ("trakstar:<org>:<id>"), Link, Employment_Type (the card's "Full-time", "Part-time"), the `POSTED_PAY_COLUMNS`
#'   and Close_Date.
#' @export
fetch_trakstar_postings <- function(board_url, details = TRUE, detail_delay = 0.3, max_pages = 20) {
  base <- sub("^(https?://[^/?#]+).*$", "\\1", trimws(board_url))
  if (!grepl("^https://[a-z0-9-]+\\.hire\\.trakstar\\.com$", base)) {
    stop("fetch_trakstar_postings() takes a board address like https://<org>.hire.trakstar.com/, not '", board_url, "'")
  }
  pages <- list()
  page <- 1
  repeat {
    req <- request(paste0(base, "/")) %>% httr2::req_user_agent(POSTING_STATUS_UA)
    if (page > 1) req <- req_url_query(req, p = page)
    found <- parse_trakstar_list(resp_body_string(perform_with_retry(req)), base)
    if (nrow(found) == 0) break
    if (repeats_seen_links(found$Link, pages)) break   # the board repeated a page
    pages[[length(pages) + 1]] <- found
    if (page >= max_pages) { warn_page_cap("Trakstar", base, max_pages); break }
    page <- page + 1
  }
  postings <- if (length(pages)) dplyr::bind_rows(pages) else parse_trakstar_list("<html></html>", base)
  postings <- postings[!duplicated(postings$Link), , drop = FALSE]
  postings$Close_Date <- rep(NA_character_, nrow(postings))
  if (!details || nrow(postings) == 0) return(postings)

  found <- fetch_trakstar_posting_details(postings$Link, delay = detail_delay)
  postings$Posted_Date <- found$Posted_Date
  postings$Close_Date <- found$Close_Date
  for (col in POSTED_PAY_COLUMNS) postings[[col]] <- found[[col]]
  postings
}

#' Parse a Trakstar Hire list page
#'
#' Each opening is a `div.js-careers-page-job-list-item` card: `data-href="/jobs/<id>/"`, the title in
#' `h3.js-job-list-opening-name`, the place in `.meta-job-location-city` / `.meta-job-location-state`, the employment
#' type in `.js-job-list-opening-meta`. Every field is read inside its own card, so a card missing one gives NA there
#' only.
#' @param html_text The list page.
#' @param base The board's address (`https://<org>.hire.trakstar.com`), for the links.
#' @return One row per card: Title, Location, Posted_Date (NA: the list gives none), Posting_ID, Link,
#'   Employment_Type and the posted pay columns (NA).
#' @keywords internal
parse_trakstar_list <- function(html_text, base) {
  cards <- rvest::html_elements(rvest::read_html(html_text), "div.js-careers-page-job-list-item")
  href <- rvest::html_attr(cards, "data-href")
  keep <- !is.na(href) & grepl("^/jobs/[A-Za-z0-9]+/?$", href)
  cards <- cards[keep]; href <- href[keep]
  text_in <- function(css) {
    x <- trimws(rvest::html_text2(rvest::html_element(cards, css)))
    x[!is.na(x) & !nzchar(x)] <- NA_character_
    x
  }
  city <- text_in(".meta-job-location-city")
  state <- text_in(".meta-job-location-state")
  # "Ashland, MT" in the city field on some postings: the state is already there.
  location <- ifelse(is.na(city), state,
                     ifelse(is.na(state) | grepl(",", city, fixed = TRUE), city, paste0(city, ", ", state)))
  org <- sub("^https?://([^.]+)\\..*$", "\\1", base)
  id <- sub("^/jobs/([A-Za-z0-9]+)/?$", "\\1", href)
  ensure_posted_pay_columns(data.frame(
    Title = text_in("h3.js-job-list-opening-name"), Location = location, Posted_Date = rep(NA_character_, length(id)),
    Posting_ID = if (length(id)) paste0("trakstar:", org, ":", id) else character(0),
    Link = if (length(id)) paste0(base, "/jobs/", id, "/") else character(0),
    Employment_Type = text_in(".js-job-list-opening-meta"), stringsAsFactors = FALSE))
}

#' Fetch Trakstar posting details
#'
#' Each posting's page, for its posted date, closing date and pay. A page that cannot be read gives NA for that
#' posting; the postings themselves are unaffected.
#' @param links Posting links.
#' @param delay Seconds between pages.
#' @return One row per link: Link, Posted_Date, Close_Date and the `POSTED_PAY_COLUMNS`.
#' @keywords internal
fetch_trakstar_posting_details <- function(links, delay = 0.3) {
  rows <- lapply(links, function(link) {
    Sys.sleep(delay)
    page <- tryCatch(resp_body_string(request(link) %>% httr2::req_user_agent(POSTING_STATUS_UA) %>% perform_with_retry()),
                     error = function(e) NA_character_)
    tryCatch(parse_trakstar_posting_page(page), error = function(e) {
      message("Trakstar posting page could not be read (", link, "): ", conditionMessage(e))
      parse_trakstar_posting_page(NA_character_)
    })
  })
  out <- if (length(rows)) dplyr::bind_rows(rows) else parse_trakstar_posting_page(NA_character_)[0, , drop = FALSE]
  failed <- sum(is.na(out$Posted_Date))
  if (failed > 0) message("Trakstar posting pages without a posted date: ", failed, " of ", length(links))
  cbind(data.frame(Link = as.character(links), stringsAsFactors = FALSE), out)
}

#' Parse a Trakstar posting page
#'
#' The posted and closing dates from the page's JobPosting block, and the pay from its description: the block's
#' `baseSalary` when a board fills it in, else the description text ([extract_posted_pay()]).
#' @param html_text The page, or NA.
#' @return One row: Posted_Date, Close_Date and the `POSTED_PAY_COLUMNS`.
#' @keywords internal
parse_trakstar_posting_page <- function(html_text) {
  j <- parse_jobposting_jsonld(html_text)
  out <- data.frame(Posted_Date = j$Date_Posted, Close_Date = j$Valid_Through, stringsAsFactors = FALSE)
  pay <- if (!is.na(j$Pay_Min)) {
    data.frame(Pay_Min = j$Pay_Min, Pay_Max = j$Pay_Max, Pay_Unit = j$Pay_Unit, Pay_Text = NA_character_,
               stringsAsFactors = FALSE)
  } else extract_posted_pay(trakstar_description_text(html_text))
  cbind(out, pay[, POSTED_PAY_COLUMNS, drop = FALSE])
}

# The JobPosting block's description as plain text. Trakstar stores it as HTML escaped once more ("&lt;p&gt;"), so
# it is unescaped to HTML and then read as text, one line per paragraph.
trakstar_description_text <- function(html_text) {
  j <- jobposting_blocks(html_text)
  d <- if (length(j)) jp_chr(j[[1]]$description) else NA_character_
  if (is.na(d) || !nzchar(trimws(d))) return(NA_character_)
  inner <- xml2::xml_text(xml2::read_html(paste0("<div>", d, "</div>")))
  rvest::html_text2(rvest::read_html(paste0("<div>", inner, "</div>")))
}
