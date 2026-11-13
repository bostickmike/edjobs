# The platform fetchers both states call: SchoolSpring (the list), TedK12,
# PeopleAdmin (the feed) and NEOGOV (the classic careers page, with each
# posting's own page for its dates). Moved from the MT and WY repos'
# direct_api_scrapers.R in phase 4 (2026-11-12), where each pair had drifted
# a few lines apart; the differences are settled here (NEWS.md, 0.11.0).
# A fix to one of these belongs here; the Claude auto-fix opens its PR on
# this repo when the broken scraper lives here.

# ---------------------------------------------------------------------------
# SchoolSpring / PowerSchool Unified Talent (K-12 districts)
# ---------------------------------------------------------------------------

#' SchoolSpring domain from a registry link
#'
#' The district's own SchoolSpring hostname, e.g. "crook1.schoolspring.com",
#' from its registry Job_Link ("https://crook1.schoolspring.com/"): the
#' `domain_name` [fetch_schoolspring_postings()] takes. Shared by the pipeline
#' and the drift check's live check, so both call the scraper the same way.
#' @param job_link The registry's Job_Link.
#' @export
schoolspring_domain <- function(job_link) sub("/$", "", sub("^https?://", "", job_link))

#' Fetch SchoolSpring postings
#'
#' The interactive site at a district's schoolspring.com domain sits behind
#' bot protection (Incapsula), but api.schoolspring.com, the host the site's
#' own React app calls, is not protected and takes the domain as a plain query
#' parameter. Paginated at `page_size` per page; keeps requesting pages until
#' one comes back short or empty.
#'
#' Returns Title, Location, Posted_Date, Posting_ID
#' ("schoolspring:<domain>:<job id>", the identity WY's K-12 history keys on;
#' MT builds its ids from the link and ignores it) and Link.
#' @param domain_name The district's SchoolSpring hostname, e.g.
#'   "laurel.schoolspring.com".
#' @param page_size Postings per request.
#' @param max_pages Stop after this many pages (with a warning): a board that
#'   repeats its last page would otherwise loop until the job's time limit.
#' @export
fetch_schoolspring_postings <- function(domain_name, page_size = 50, max_pages = 40) {
  all_pages <- list()
  page <- 1
  repeat {
    resp <- request("https://api.schoolspring.com/api/Jobs/GetPagedJobsWithSearch") %>%
      req_url_query(
        domainName = domain_name, keyword = "", location = "", category = "",
        gradelevel = "", jobtype = "", organization = "",
        page = page, size = page_size, sortDateAscending = "false"
      ) %>%
      perform_with_retry()

    page_df <- parse_schoolspring_json(resp_body_string(resp), domain_name)
    if (nrow(page_df) == 0) break
    if (repeats_seen_links(page_df$Link, all_pages)) break   # the board repeated a page

    all_pages[[length(all_pages) + 1]] <- page_df
    if (nrow(page_df) < page_size) break  # last page was partial -> no more pages
    if (page >= max_pages) { warn_page_cap("SchoolSpring", domain_name, max_pages); break }
    page <- page + 1
  }

  if (length(all_pages) == 0) return(schoolspring_empty())
  dplyr::bind_rows(all_pages)
}

schoolspring_empty <- function() {
  data.frame(Title = character(0), Location = character(0),
             Posted_Date = character(0), Posting_ID = character(0),
             Link = character(0), stringsAsFactors = FALSE)
}

#' Parse SchoolSpring JSON
#'
#' One page of the GetPagedJobsWithSearch response. SchoolSpring occasionally
#' exposes its own example vacancies ("Sample Certified Position") as active
#' records; they are placeholder text, not jobs a candidate can apply for, and
#' are dropped.
#' @param json_text The response body.
#' @param domain_name The district's SchoolSpring hostname (for the links and ids).
#' @export
parse_schoolspring_json <- function(json_text, domain_name) {
  parsed <- jsonlite::fromJSON(json_text, simplifyVector = TRUE)
  jobs <- parsed$value$jobsList

  if (is.null(jobs) || length(jobs) == 0 || nrow(jobs) == 0) return(schoolspring_empty())

  jobs <- jobs[!grepl("^Sample (Classified|Certified) Position$", jobs$title,
                      ignore.case = TRUE), , drop = FALSE]
  if (nrow(jobs) == 0) return(schoolspring_empty())

  data.frame(
    Title = jobs$title,
    Location = jobs$location,
    Posted_Date = substr(jobs$displayDate, 1, 10),
    Posting_ID = paste0("schoolspring:", tolower(domain_name), ":", jobs$jobId),
    Link = paste0("https://", domain_name, "/jobs/", jobs$jobId),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# TedK12 (PowerSchool's "Hire" applicant tracking system)
# ---------------------------------------------------------------------------

#' Fetch TedK12 postings
#'
#' TedK12 / PowerSchool Hire serves different content depending on the
#' request's User-Agent, not on whether a browser runs JavaScript: httr2's
#' default UA gets a near-empty modern app shell with zero job rows, while a
#' request that identifies as a real browser gets the classic job board with
#' real `<tr id="JobList_N">` rows. Confirmed 2026-08-06 by comparing a plain
#' fetch (3.7 KB shell) against a chromote-rendered page (112 KB, real rows)
#' for the same URL, then confirming a real browser UA alone reproduces the
#' rows. That explained the flaky ok/empty alternation the scrape log showed
#' for one URL across runs on the same day.
#' @param url The board URL, e.g. "https://goshen.tedk12.com/hire/index.aspx".
#' @export
fetch_tedk12_postings <- function(url) {
  resp <- request(url) %>%
    httr2::req_user_agent(POSTING_STATUS_UA) %>%
    perform_with_retry()
  parse_tedk12_postings(resp_body_string(resp), url)
}

#' Parse TedK12 postings
#'
#' Columns td 1..4 of each row are title / date / position / location. The
#' header row's title cell reads literally "Job Title" (its sortable-column
#' link text) and is dropped. Each row's url is the posting's own page
#' (ViewJob.aspx?JobID=<n>: a stable id, a page that says when the posting
#' closed, and the posted pay the list does not show); the board URL only
#' where a row has no such link.
#' @param html_text The board page.
#' @param url The board URL (base for relative links, fallback url).
#' @export
parse_tedk12_postings <- function(html_text, url) {
  empty <- data.frame(title = character(0), date_posted = character(0), position = character(0),
                      location = character(0), url = character(0), stringsAsFactors = FALSE)

  page <- rvest::read_html(html_text)
  rows <- rvest::html_elements(page, "tr")
  if (length(rows) == 0) return(empty)

  cell <- function(row, css) rvest::html_text(rvest::html_element(row, css), trim = TRUE)
  result <- dplyr::bind_rows(lapply(rows, function(row) {
    title_link <- rvest::html_element(row, "td:nth-child(1) a")
    href <- rvest::html_attr(title_link, "href")
    detail_url <- if (is.na(href) || !grepl("JobID=\\d+", href, ignore.case = TRUE)) url else xml2::url_absolute(href, url)
    data.frame(title = rvest::html_text(title_link, trim = TRUE),
               date_posted = cell(row, "td:nth-child(2)"),
               position = cell(row, "td:nth-child(3)"),
               location = cell(row, "td:nth-child(4)"),
               url = detail_url, stringsAsFactors = FALSE)
  }))
  if (nrow(result) == 0) return(empty)

  result <- result[!is.na(result$title) & nzchar(result$title) & result$title != "Job Title", , drop = FALSE]
  rownames(result) <- NULL
  result
}

# ---------------------------------------------------------------------------
# PeopleAdmin -- the institution's published Atom feed
# ---------------------------------------------------------------------------

#' Fetch PeopleAdmin postings from the Atom feed
#'
#' `feed_url` is the institution's own published feed, e.g.
#' "https://jobs.montana.edu/postings/search.atom" (every PeopleAdmin instance
#' checked publishes one at that path; "/postings/all_jobs" also answers).
#'
#' The feed carries no pay, so with `fetch_pay` each posting's own page is
#' read for it ([parse_peopleadmin_posting_pay()]): one request per posting,
#' spaced out. The same page gives the posting's dates
#' ([parse_peopleadmin_posting_dates()]); its open date replaces the feed's
#' `<published>`, which moves when a posting is re-published (11 of 16 MSU
#' Billings / Great Falls postings on 2026-11-04 opened earlier than
#' `<published>` said, one by two years). A failed page leaves only that
#' posting's pay and dates blank.
#' @param feed_url The feed.
#' @param location_fallback Location for an entry with no `<author>`, or for
#'   every entry when `location_from_author` is FALSE.
#' @param fetch_pay Read each posting's page for pay and dates.
#' @param detail_delay Seconds between posting pages.
#' @param location_from_author Use each entry's `<author><name>` (the
#'   department, e.g. "Facility Services") as its Location, falling back to
#'   `location_fallback` only where it is absent. MT's choice (its PeopleAdmin
#'   institutions are multi-department universities). FALSE gives every row
#'   `location_fallback`, WY's choice for its single-campus colleges.
#' @export
fetch_peopleadmin_atom <- function(feed_url, location_fallback = NA_character_, fetch_pay = TRUE, detail_delay = 0.3,
                                   location_from_author = TRUE) {
  resp <- request(feed_url) %>% perform_with_retry()
  postings <- parse_peopleadmin_atom(resp_body_string(resp), location_fallback, location_from_author)
  if (!fetch_pay || nrow(postings) == 0) {
    postings <- ensure_posted_pay_columns(postings)
    postings$Close_Date <- rep(NA_character_, nrow(postings))
    return(postings)
  }
  pages <- lapply(postings$Link, function(link) {
    Sys.sleep(detail_delay)
    tryCatch(resp_body_string(request(link) %>% perform_with_retry()), error = function(e) NA_character_)
  })
  pay <- dplyr::bind_rows(lapply(pages, parse_peopleadmin_posting_pay))
  dates <- dplyr::bind_rows(lapply(pages, parse_peopleadmin_posting_dates))
  postings$Posted_Date <- dplyr::coalesce(dates$Open_Date, as.character(postings$Posted_Date))
  postings$Close_Date <- dates$Close_Date
  cbind(postings, pay)
}

#' Parse a PeopleAdmin Atom feed
#' @inheritParams fetch_peopleadmin_atom
#' @param xml_text The feed body.
#' @export
parse_peopleadmin_atom <- function(xml_text, location_fallback = NA_character_, location_from_author = TRUE) {
  doc <- xml2::read_xml(xml_text)
  ns <- c(a = "http://www.w3.org/2005/Atom")
  entries <- xml2::xml_find_all(doc, "//a:entry", ns)

  if (length(entries) == 0) {
    return(data.frame(Title = character(0), Location = character(0),
                      Posted_Date = character(0), Link = character(0),
                      stringsAsFactors = FALSE))
  }

  titles <- xml2::xml_text(xml2::xml_find_first(entries, "a:title", ns))
  links <- xml2::xml_attr(xml2::xml_find_first(entries, "a:link", ns), "href")
  published <- xml2::xml_text(xml2::xml_find_first(entries, "a:published", ns))
  posted_date <- as.Date(substr(published, 1, 10))

  locations <- rep(location_fallback, length(entries))
  if (location_from_author) {
    authors <- xml2::xml_text(xml2::xml_find_first(entries, "a:author/a:name", ns))
    # xml_find_first() gives "" (not NA) when the element is absent.
    authors[!nzchar(authors)] <- NA_character_
    locations <- ifelse(is.na(authors), location_fallback, authors)
  }

  data.frame(
    Title = titles,
    Location = locations,
    Posted_Date = as.character(posted_date),
    Link = links,
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# NEOGOV -- the classic careers page (schooljobs.com / governmentjobs.com)
# ---------------------------------------------------------------------------

#' `NEOGOV_BASE`
#'
#' The domain every NEOGOV request starts from. NEOGOV answers for any agency
#' on either of its domains and 301-redirects to the agency's own (cwc to
#' schooljobs.com, lcccwy to governmentjobs.com; measured 2026-11-12), so a
#' registry needs only the agency slug. Links are built on the domain the
#' redirect lands on, so an agency's posting URLs stay what they have always
#' been.
#' @export
NEOGOV_BASE <- "https://www.schooljobs.com"

#' Fetch NEOGOV postings
#'
#' The job list of one NEOGOV agency, read from the classic careers page's own
#' AJAX fragment (`/careers/home/index?agency=<slug>`), which the page's
#' JavaScript calls; the `X-Requested-With: XMLHttpRequest` header is what
#' makes the server return the fragment instead of an empty page shell (found
#' by tracing the page's network traffic). Paginated at 10 cards per page with
#' no total count, so this reads pages until one has no cards. The list drops
#' a posting when it closes, which NEOGOV's optional "Attract" sites do not:
#' measured 2026-11-12, every posting only an Attract site listed had closed
#' months earlier, so Attract is not read.
#'
#' Each card gives the title, campus or city, the board's pay line
#' ([parse_neogov_card_pay()]) and, on some agencies, a relative date
#' ("Posted 2 weeks ago"). With `details`, each posting's own page is read
#' for its exact posted date and closing date
#' ([fetch_neogov_posting_details()]): one request per posting. Posted_Date is
#' then the exact date where the page gave one and the card's text otherwise;
#' Close_Date is the stated closing date or NA ("Continuous", "until filled").
#' @param agency_slug The agency's short code from its careers URL
#'   (schooljobs.com/careers/<slug>): "cwc", "ummissoula".
#' @param details Read each posting's page for its dates.
#' @param detail_delay Seconds between posting pages.
#' @param max_pages Stop after this many list pages (with a warning).
#' @return Title, Location, Posted_Date, Link, the `POSTED_PAY_COLUMNS`, Close_Date.
#' @export
fetch_neogov_postings <- function(agency_slug, details = TRUE, detail_delay = 0.3, max_pages = 40) {
  if (grepl("^https?://", agency_slug)) {
    stop("fetch_neogov_postings() takes the agency slug ('cwc'), not a domain: NEOGOV redirects to the agency's own domain")
  }
  all_pages <- list()
  page <- 1
  repeat {
    resp <- request(paste0(NEOGOV_BASE, "/careers/home/index")) %>%
      req_url_query(agency = agency_slug, sort = "PositionTitle", isDescendingSort = "false", page = page) %>%
      req_headers(`X-Requested-With` = "XMLHttpRequest") %>%
      perform_with_retry()
    # The domain the redirect landed on (governmentjobs.com for some agencies).
    base_domain <- sub("^(https?://[^/]+).*$", "\\1", httr2::resp_url(resp))
    if (!grepl("^https?://", base_domain)) base_domain <- NEOGOV_BASE

    page_result <- parse_neogov_html(resp_body_string(resp), base_domain)
    if (nrow(page_result) == 0) break
    if (repeats_seen_links(page_result$Link, all_pages)) break   # the board repeated a page

    all_pages[[length(all_pages) + 1]] <- page_result
    if (page >= max_pages) { warn_page_cap("NEOGOV", agency_slug, max_pages); break }
    page <- page + 1
  }

  if (length(all_pages) == 0) {
    out <- neogov_empty()
    out$Close_Date <- character(0)
    return(out)
  }
  postings <- dplyr::bind_rows(all_pages)
  # A shared board's campus assignment reads Location
  # (assign_shared_board_institutions()); all NA means the card markup changed
  # and every posting would stay with the board's owner.
  if (all(is.na(postings$Location))) {
    warning("NEOGOV agency '", agency_slug, "': no posting has a Location (", nrow(postings),
            " postings); the card markup may have changed", call. = FALSE)
  }
  postings$Close_Date <- rep(NA_character_, nrow(postings))
  if (!details) return(postings)

  found <- fetch_neogov_posting_details(postings$Link, delay = detail_delay)
  postings$Posted_Date <- dplyr::coalesce(found$Posted_Date, as.character(postings$Posted_Date))
  postings$Close_Date <- found$Close_Date
  postings
}

# TRUE when a page brings no link not already seen (a board repeating its last
# page instead of returning an empty one).
repeats_seen_links <- function(links, pages) {
  links <- links[!is.na(links)]
  length(pages) > 0 && length(links) > 0 && all(links %in% unlist(lapply(pages, `[[`, "Link")))
}

warn_page_cap <- function(platform, board, max_pages) {
  warning(platform, " board '", board, "': stopped at ", max_pages, " pages; raise max_pages if it really has more",
          call. = FALSE)
}

neogov_empty <- function() {
  ensure_posted_pay_columns(
    data.frame(Title = character(0), Location = character(0),
               Posted_Date = character(0), Link = character(0),
               stringsAsFactors = FALSE))
}

#' Parse a NEOGOV job-list fragment
#'
#' The fragment renders each job twice: in a card `<ul>` (with location and
#' date) and again in a plain `<table>` (title only), both present regardless
#' of viewport. Only the card container is read. Pay is matched to each card
#' by job id (the number in its /jobs/<id>/ link), not by position; if the
#' pay cannot be read the postings still come through with NA pay.
#' @param html_text The fragment.
#' @param base_domain The domain the links are built on.
#' @export
parse_neogov_html <- function(html_text, base_domain) {
  soup <- rvest::read_html(html_text)
  container <- rvest::html_elements(soup, "ul.job-listing-container")
  cards <- rvest::html_elements(container, "li.list-item")
  cards <- cards[vapply(cards, function(card) length(rvest::html_elements(card, ".item-details-link")) > 0, logical(1))]
  if (length(cards) == 0) return(neogov_empty())

  # Every field is read inside its own card, so a card that lacks one gives NA
  # for that card only (matching by position across the page would shift or
  # blank every card after it).
  link_nodes <- rvest::html_element(cards, ".item-details-link")
  titles <- rvest::html_text2(link_nodes)
  links <- paste0(base_domain, rvest::html_attr(link_nodes, "href"))
  locations <- vapply(cards, neogov_card_location, character(1))
  posted_dates <- rvest::html_text2(rvest::html_element(cards, ".list-entry-starts span"))

  pay <- tryCatch(parse_neogov_card_pay(html_text), error = function(e) {
    message("NEOGOV card pay could not be read: ", conditionMessage(e))
    ensure_posted_pay_columns(data.frame(Job_ID = character(0), stringsAsFactors = FALSE))
  })
  idx <- match(neogov_job_id(links), pay$Job_ID)

  cbind(data.frame(Title = titles, Location = locations, Posted_Date = posted_dates,
                   Link = links, stringsAsFactors = FALSE),
        pay[idx, POSTED_PAY_COLUMNS, drop = FALSE], row.names = NULL)
}

#' The campus or city line of one NEOGOV card
#'
#' A card's `.list-meta` lines are, in order: the location, the pay line
#' ("Full-Time - $21.00 Hourly", or just the pay type: "Adjunct"), then
#' labelled lines ("Category: ...", "Department: ...", "Division: ..."). The
#' location is the first line only when it is not one of the others: it has
#' no class, no "Label:" prefix, no "$" and no nested `<span>` (the pay
#' line's separator). When a card has a single unlabelled line it is taken
#' as the location only if it does not look like pay. Measured 2026-11-12 on
#' every card in the fixtures (four agencies): all have the location first.
#' @param card One `li.list-item` node.
#' @return The location, or NA.
#' @noRd
neogov_card_location <- function(card) {
  lines <- rvest::html_elements(card, ".list-meta > li")
  if (length(lines) == 0) return(NA_character_)
  text <- trimws(rvest::html_text2(lines))
  plain <- is.na(rvest::html_attr(lines, "class")) & !grepl("^[A-Za-z ]+:", text) & nzchar(text)
  if (!plain[1]) return(NA_character_)
  has_span <- vapply(lines, function(l) length(rvest::html_elements(l, "span")) > 0, logical(1))
  looks_like_pay <- has_span[1] || grepl("$", text[1], fixed = TRUE)
  if (looks_like_pay) return(NA_character_)
  text[1]
}

#' NEOGOV job id from a posting link
#'
#' The number in /careers/<agency>/jobs/<id>/...: the posting's identity on
#' both NEOGOV domains, its Attract sites and its own page.
#' @param url Posting links.
#' @export
neogov_job_id <- function(url) sub("^.*/jobs/([0-9]+).*$", "\\1", url)

#' Fetch NEOGOV posting details
#'
#' Each posting's own page, for what the list does not give: the exact posted
#' date and the closing date. Both are in the page's schema.org JobPosting
#' block ([parse_jobposting_jsonld()]): `datePosted` on every page measured
#' (142 of 142 across five colleges, 2026-11-12), `validThrough` when the
#' posting has a closing date (25 of 142; the rest are continuous or open
#' until filled). A page that cannot be read or has no block gives NA for that
#' posting; the postings themselves are unaffected.
#' @param links Posting links.
#' @param delay Seconds between pages.
#' @return One row per link: Link, Posted_Date, Close_Date (YYYY-MM-DD or NA).
#' @export
fetch_neogov_posting_details <- function(links, delay = 0.3) {
  rows <- lapply(links, function(link) {
    Sys.sleep(delay)
    page <- tryCatch(resp_body_string(request(link) %>% httr2::req_user_agent(POSTING_STATUS_UA) %>% perform_with_retry()),
                     error = function(e) NA_character_)
    tryCatch(parse_neogov_posting_page(page), error = function(e) {
      message("NEOGOV posting page could not be read (", link, "): ", conditionMessage(e))
      data.frame(Posted_Date = NA_character_, Close_Date = NA_character_, stringsAsFactors = FALSE)
    })
  })
  out <- if (length(rows) == 0) data.frame(Posted_Date = character(0), Close_Date = character(0), stringsAsFactors = FALSE) else dplyr::bind_rows(rows)
  failed <- sum(is.na(out$Posted_Date))
  if (failed > 0) message("NEOGOV posting pages without a posted date: ", failed, " of ", length(links))
  cbind(data.frame(Link = as.character(links), stringsAsFactors = FALSE), out)
}

#' Parse a NEOGOV posting page
#'
#' The posted and closing dates from the page's JobPosting block.
#' @param html_text The page, or NA.
#' @return One row: Posted_Date, Close_Date.
#' @export
parse_neogov_posting_page <- function(html_text) {
  j <- parse_jobposting_jsonld(html_text)
  data.frame(Posted_Date = j$Date_Posted, Close_Date = j$Valid_Through, stringsAsFactors = FALSE)
}

#' Parse a page's schema.org JobPosting block
#'
#' Many job boards (NEOGOV among them) embed a JSON-LD `<script
#' type="application/ld+json">` block of `@type` JobPosting, the structured
#' data search engines read. This returns its fields in the dashboards' terms:
#' Title, Date_Posted and Valid_Through (YYYY-MM-DD), Organization, Locality
#' and Region (the address), Pay_Min / Pay_Max / Pay_Unit from `baseSalary`
#' (unit "hour", "year", ... from `unitText`; a 0-0 range means none stated
#' and is NA) and Employment_Type. A page with no such block, or one that
#' cannot be parsed, gives a row of NA. The first JobPosting block is read if
#' a page has several (or a block that is an array of them).
#' @param html_text The page, or NA.
#' @export
parse_jobposting_jsonld <- function(html_text) {
  none <- jobposting_none()
  if (is.null(html_text) || length(html_text) != 1 || is.na(html_text)) return(none)
  blocks <- tryCatch(rvest::html_text(rvest::html_elements(rvest::read_html(html_text), "script[type='application/ld+json']")),
                     error = function(e) character(0))
  for (b in blocks) {
    j <- tryCatch(jsonlite::fromJSON(b, simplifyVector = FALSE), error = function(e) NULL)
    j <- jobposting_find(j)
    if (is.null(j)) next
    # One odd block gives NA for that page, never an error for the board.
    row <- tryCatch(jobposting_row(j), error = function(e) {
      message("JobPosting block could not be read: ", conditionMessage(e))
      NULL
    })
    if (!is.null(row)) return(row)
  }
  none
}

jobposting_none <- function() {
  data.frame(Title = NA_character_, Date_Posted = NA_character_, Valid_Through = NA_character_,
             Organization = NA_character_, Locality = NA_character_, Region = NA_character_,
             Pay_Min = NA_real_, Pay_Max = NA_real_, Pay_Unit = NA_character_,
             Employment_Type = NA_character_, stringsAsFactors = FALSE)
}

# The first JobPosting in a parsed block: the block itself, an array of
# objects, or an "@graph" list. "@type" may be a string or an array.
jobposting_find <- function(j) {
  is_jp <- function(x) is.list(x) && !is.null(names(x)) && "JobPosting" %in% unlist(x[["@type"]])
  if (is.null(j) || !is.list(j)) return(NULL)
  if (is_jp(j)) return(j)
  cand <- if (!is.null(names(j)) && is.list(j[["@graph"]])) j[["@graph"]] else if (is.null(names(j))) j else list()
  for (x in cand) if (is_jp(x)) return(x)
  NULL
}

# A named field of x, or NULL when x is not an object (a string, a number):
# schema.org lets most properties be either text or a nested object.
jp_get <- function(x, name) if (is.list(x) && !is.null(names(x))) x[[name]] else NULL

# x itself if it is an object; its first element if it is an array of them.
jp_obj <- function(x) {
  if (is.list(x) && is.null(names(x)) && length(x) > 0) x <- x[[1]]
  x
}

jp_chr <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  if (is.list(x)) x <- x[[1]]
  if (is.null(x) || length(x) == 0 || is.list(x)) return(NA_character_)
  as.character(x[[1]])
}

jp_num <- function(x) {
  x <- jp_chr(x)
  if (is.na(x)) NA_real_ else suppressWarnings(as.numeric(gsub("[$,]", "", x)))
}

# A JSON-LD date as an ISO day. Plain "2026-11-01" as is; a date-time with a
# zone ("2026-11-02T06:59:00Z", "...-07:00") as the day in Mountain time (both
# states), so a midnight-local deadline doesn't land a day late; a date-time
# with no zone, its date part. Anything else (11/01/2026, 20261101) is NA, not
# passed on as a date.
jsonld_day <- function(x, tz = "America/Denver") {
  x <- trimws(jp_chr(x))
  if (is.na(x) || !grepl("^\\d{4}-\\d{2}-\\d{2}", x)) return(NA_character_)
  d <- substr(x, 1, 10)
  if (is.na(as.Date(d, format = "%Y-%m-%d", optional = TRUE))) return(NA_character_)
  zone <- regmatches(x, regexpr("(Z|[+-]\\d{2}:?\\d{2})$", x))
  if (!grepl("T\\d{2}:\\d{2}", x) || length(zone) == 0) return(d)
  clock <- sub("^.{10}T(\\d{2}:\\d{2}(:\\d{2})?).*$", "\\1", x)
  if (nchar(clock) == 5) clock <- paste0(clock, ":00")
  utc <- as.POSIXct(paste(d, clock), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  if (zone != "Z") {
    sgn <- if (startsWith(zone, "-")) -1 else 1
    hm <- as.integer(c(substr(gsub(":", "", zone), 2, 3), substr(gsub(":", "", zone), 4, 5)))
    utc <- utc - sgn * (hm[1] * 3600 + hm[2] * 60)
  }
  if (is.na(utc)) return(d)
  format(utc, "%Y-%m-%d", tz = tz)
}

jobposting_row <- function(j) {
  day <- jsonld_day
  # baseSalary: a MonetaryAmount whose value is a QuantitativeValue
  # ({minValue, maxValue, unitText}) or a bare number; or a bare number itself.
  base <- jp_obj(j$baseSalary)
  sal <- if (is.list(base)) jp_obj(base$value) else base
  if (is.list(sal)) {
    pay_min <- jp_num(sal$minValue %||% sal$value); pay_max <- jp_num(sal$maxValue %||% sal$value)
    unit <- jp_chr(sal$unitText %||% jp_get(base, "unitText"))
  } else {
    pay_min <- pay_max <- jp_num(sal)
    unit <- jp_chr(jp_get(base, "unitText"))
  }
  if (!is.na(pay_min) && !is.na(pay_max) && pay_min == 0 && pay_max == 0) pay_min <- pay_max <- NA_real_
  unit <- if (is.na(unit)) NA_character_ else c(HOUR = "hour", DAY = "day", WEEK = "week", MONTH = "month", YEAR = "year")[toupper(unit)]
  if (is.na(pay_min) && is.na(pay_max)) unit <- NA_character_
  org <- j$hiringOrganization
  org <- if (is.list(org)) jp_get(jp_obj(org), "name") else org
  # An address given as one string ("Missoula, MT") has no parts to split
  # reliably, so Locality and Region are NA for it.
  addr <- jp_obj(jp_get(jp_obj(j$jobLocation), "address"))
  emp <- j$employmentType
  data.frame(
    Title = jp_chr(j$title), Date_Posted = day(j$datePosted), Valid_Through = day(j$validThrough),
    Organization = jp_chr(org), Locality = jp_chr(jp_get(addr, "addressLocality")), Region = jp_chr(jp_get(addr, "addressRegion")),
    Pay_Min = pay_min, Pay_Max = pay_max, Pay_Unit = unname(unit),
    Employment_Type = if (is.null(emp)) NA_character_ else paste(unlist(emp), collapse = ","),
    stringsAsFactors = FALSE)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---------------------------------------------------------------------------
# Boards shared by several institutions
# ---------------------------------------------------------------------------

#' Assign postings on a shared board to the institutions it covers
#'
#' Some boards carry several institutions' postings: the University of
#' Montana's NEOGOV agency also lists Helena College, Missoula College and UM
#' Western. The registry says so as data: each covered institution has its
#' own row with `Platform` "SharesBoard", the board's slug in `board_col`,
#' and in `pattern_col` a regular expression matched against each posting's
#' Location (the board's campus name: "^Helena College"). A posting whose
#' Location matches gets that row's Institution; the rest keep theirs (the
#' board owner's). Rows are applied in registry order, later rows winning.
#' A third state's shared board is then a registry row, not code.
#' @param postings The board's postings, with Location and Institution.
#' @param board The board's slug (the value `board_col` holds).
#' @param registry The HE registry.
#' @param board_col,pattern_col,platform,location_col Registry and posting columns.
#' @export
assign_shared_board_institutions <- function(postings, board, registry, board_col = "Feed_URL",
                                             pattern_col = "Location_Pattern", platform = "SharesBoard",
                                             location_col = "Location") {
  if (nrow(postings) == 0 || !pattern_col %in% names(registry)) return(postings)
  pat <- registry[[pattern_col]]
  rows <- registry[registry$Platform == platform & registry[[board_col]] == board & !is.na(pat) & nzchar(pat), , drop = FALSE]
  loc <- as.character(postings[[location_col]])
  for (i in seq_len(nrow(rows))) {
    hit <- grepl(rows[[pattern_col]][i], loc)
    hit[is.na(hit)] <- FALSE
    postings$Institution[hit] <- rows$Institution[i]
  }
  postings
}
