# CSS boards: a job page with no platform behind it, read by a few CSS selectors (package review step 8, 2026-11-24).
# One reader for the one-off pages whose parsers differed only in their selectors. Each source's selectors are a
# row of the state's own rules file (`css_board_rules.csv`), keyed by the registry name; the registry row gives the
# page and the place.

CSS_BOARD_RULE_COLUMNS <- c("Source", "Item_CSS", "Title_CSS", "Link_CSS", "Location_CSS", "Board_CSS",
                            "Exclude_Regex", "Include_Regex", "Strip_Regex", "Max_Title_Chars", "Max_Tries")

#' Read and check a CSS-board rules file
#'
#' One row per source, keyed by `Source` (the name in the state's registry). Columns:
#' * `Title_CSS` (required): the title elements, inside each item when `Item_CSS` is set, else on the whole page.
#' * `Item_CSS`: one element per posting; its title, link and location are read inside it, so an item missing one
#'   gives a fallback for that item only. An item with no title is skipped (a template row).
#' * `Link_CSS`: blank, the page itself; `@title`, the title element's own `href`; `@item`, the item's own (an item
#'   that is the link); otherwise a selector inside the item (both need `Item_CSS`). Links are made absolute against the page
#'   address; an item with no link gets the page.
#' * `Location_CSS`: a selector inside the item (needs `Item_CSS`); blank, or an item without it, gives the
#'   registry's place.
#' * `Board_CSS`: an element the page always has while it is the job board, postings or not. When set and missing,
#'   the reader stops, so a moved or redesigned page is an error in the scrape log instead of "no postings".
#' * `Exclude_Regex`, `Include_Regex` (case-insensitive, on the title): drop matching titles; keep only matching
#'   titles. `Strip_Regex` (case-insensitive): removed from the titles kept ("Hiring " before each); a title it
#'   empties is dropped. `Max_Title_Chars`: drop longer titles (a paragraph caught by the selector).
#' * `Max_Tries`: attempts for the page request (default 3), for a host that rate-limits.
#' @param rules A data frame, or the path of a CSV.
#' @return The rules, every column present (missing optional ones blank), checked.
#' @export
css_board_rules <- function(rules) {
  if (is.character(rules)) rules <- utils::read.csv(rules, stringsAsFactors = FALSE, colClasses = "character",
                                                   na.strings = character(0))
  if (!is.data.frame(rules)) stop("css_board_rules(): a data frame or a CSV path")
  if (!all(c("Source", "Title_CSS") %in% names(rules))) stop("css_board_rules(): needs Source and Title_CSS columns")
  unknown <- setdiff(names(rules), CSS_BOARD_RULE_COLUMNS)
  if (length(unknown)) stop("css_board_rules(): unknown column(s): ", paste(unknown, collapse = ", "))
  for (col in setdiff(CSS_BOARD_RULE_COLUMNS, names(rules))) rules[[col]] <- rep("", nrow(rules))
  rules <- rules[CSS_BOARD_RULE_COLUMNS]
  for (col in CSS_BOARD_RULE_COLUMNS) {
    v <- as.character(rules[[col]])
    v[is.na(v)] <- ""
    # A strip pattern's own spaces count (" - .*$").
    rules[[col]] <- if (col == "Strip_Regex") ifelse(nzchar(trimws(v)), v, "") else trimws(v)
  }
  where <- function(bad) paste(rules$Source[bad], collapse = ", ")
  if (any(!nzchar(rules$Source))) stop("css_board_rules(): a row has no Source")
  if (anyDuplicated(rules$Source)) stop("css_board_rules(): Source listed twice: ", where(duplicated(rules$Source)))
  if (any(!nzchar(rules$Title_CSS))) stop("css_board_rules(): no Title_CSS for ", where(!nzchar(rules$Title_CSS)))
  inside <- (nzchar(rules$Link_CSS) & rules$Link_CSS != "@title") | nzchar(rules$Location_CSS)
  if (any(inside & !nzchar(rules$Item_CSS))) {
    stop("css_board_rules(): Link_CSS / Location_CSS selectors are read inside an item; set Item_CSS for ",
         where(inside & !nzchar(rules$Item_CSS)))
  }
  for (col in c("Max_Title_Chars", "Max_Tries")) {
    v <- rules[[col]]
    bad <- nzchar(v) & !grepl("^[1-9][0-9]*$", v)
    if (any(bad)) stop("css_board_rules(): ", col, " must be a whole number for ", where(bad))
  }
  for (col in c("Exclude_Regex", "Include_Regex", "Strip_Regex")) {
    for (i in which(nzchar(rules[[col]]))) {
      ok <- tryCatch({ suppressWarnings(grepl(rules[[col]][i], "", perl = TRUE)); TRUE }, error = function(e) FALSE)
      if (!ok) stop("css_board_rules(): ", col, " for ", rules$Source[i], " is not a valid regular expression")
    }
  }
  rownames(rules) <- NULL
  rules
}

#' One source's CSS-board rule
#'
#' @param rules A [css_board_rules()] result (or anything it accepts).
#' @param source The source's registry name.
#' @return The rule as a named list.
#' @export
css_board_rule <- function(rules, source) {
  if (!is.data.frame(rules) || !identical(names(rules), CSS_BOARD_RULE_COLUMNS)) rules <- css_board_rules(rules)
  hit <- which(rules$Source == source)
  if (length(hit) == 0) stop("css_board_rule(): no rule for '", source, "' in the CSS-board rules")
  as.list(rules[hit[1], , drop = FALSE])
}

#' A dispatch spec for CSS boards
#'
#' For a state's [scraper_specs()]: `CSSBoard = css_board_spec(...)` calls [fetch_css_board_postings()] with the
#' registry row's page, its place and its rule.
#' @param rules The rules ([css_board_rules()]), a CSV path, or a function returning either (read when a source is
#'   looked up).
#' @param url_col,place_col,name_col The registry's page, place and name columns (K-12 `Job_Link`, `City`,
#'   `District`; higher ed `Feed_URL`, `Town`, `Institution`).
#' @return A [scraper_spec()].
#' @export
css_board_spec <- function(rules, url_col, place_col, name_col) {
  force(rules); force(name_col)
  scraper_spec("fetch_css_board_postings", list(
    url_col, registry_text(place_col),
    function(row) css_board_rule(if (is.function(rules)) rules() else rules, row[[name_col]])),
    parse_fn = "parse_css_board")
}

#' Fetch a CSS board's postings
#'
#' One plain request (a browser User-Agent; some college sites refuse others), then [parse_css_board()].
#' @param url The job page.
#' @param location The registry's place, for postings whose page gives none.
#' @param rule The source's rule ([css_board_rule()]).
#' @return As [parse_css_board()].
#' @export
fetch_css_board_postings <- function(url, location, rule) {
  tries <- if (nzchar(rule$Max_Tries %||% "")) as.integer(rule$Max_Tries) else 3L
  resp <- request(url) %>% httr2::req_user_agent(POSTING_STATUS_UA) %>% perform_with_retry(max_tries = tries)
  parse_css_board(resp_body_string(resp), url, location, rule)
}

#' Parse a CSS board
#'
#' Reads a job page with the selectors in `rule` ([css_board_rules()] describes them). Every title found is a row:
#' no de-duplication, since two openings can share a title. Titles are trimmed; blank titles are dropped.
#' @param html_text The page.
#' @param url The page's address: the link for postings with none, and the base for relative links.
#' @param location The registry's place, for postings whose page gives none.
#' @param rule The source's rule ([css_board_rule()]).
#' @return Title, Location, Posted_Date (NA: these pages give none), Link.
#' @export
parse_css_board <- function(html_text, url, location, rule) {
  get <- function(field) { v <- rule[[field]]; if (is.null(v) || is.na(v[1])) "" else trimws(as.character(v[1])) }
  empty <- data.frame(Title = character(0), Location = character(0), Posted_Date = character(0),
                      Link = character(0), stringsAsFactors = FALSE)
  page <- rvest::read_html(html_text)
  if (nzchar(get("Board_CSS")) && length(rvest::html_elements(page, get("Board_CSS"))) == 0) {
    stop("CSS board: the page has no '", get("Board_CSS"), "' (the board's marker); the page may have moved: ", url)
  }
  link_css <- get("Link_CSS")
  if (nzchar(get("Item_CSS"))) {
    items <- rvest::html_elements(page, get("Item_CSS"))
    title_el <- rvest::html_element(items, get("Title_CSS"))
    title <- rvest::html_text2(title_el)
    href <- if (link_css == "@title") rvest::html_attr(title_el, "href")
            else if (link_css == "@item") rvest::html_attr(items, "href")
            else if (nzchar(link_css)) rvest::html_attr(rvest::html_element(items, link_css), "href")
            else rep(NA_character_, length(items))
    place <- if (nzchar(get("Location_CSS"))) trimws(rvest::html_text2(rvest::html_element(items, get("Location_CSS"))))
             else rep(NA_character_, length(items))
  } else {
    title_el <- rvest::html_elements(page, get("Title_CSS"))
    title <- rvest::html_text2(title_el)
    href <- if (link_css == "@title") rvest::html_attr(title_el, "href") else rep(NA_character_, length(title_el))
    place <- rep(NA_character_, length(title_el))
  }
  if (length(title) == 0) return(empty)
  title <- trimws(title)
  keep <- !is.na(title) & nzchar(title)
  if (nzchar(get("Exclude_Regex"))) keep <- keep & !grepl(get("Exclude_Regex"), title, ignore.case = TRUE, perl = TRUE)
  if (nzchar(get("Include_Regex"))) keep <- keep & grepl(get("Include_Regex"), title, ignore.case = TRUE, perl = TRUE)
  strip <- rule$Strip_Regex %||% ""                          # not get(): its spaces count
  if (!is.na(strip) && nzchar(trimws(strip))) {
    title <- trimws(gsub(strip, "", title, ignore.case = TRUE, perl = TRUE))
    keep <- keep & nzchar(title)
  }
  if (nzchar(get("Max_Title_Chars"))) keep <- keep & nchar(title) <= as.integer(get("Max_Title_Chars"))
  if (!any(keep)) return(empty)
  href <- trimws(href[keep])
  link <- ifelse(is.na(href) | !nzchar(href), url, css_board_absolute(ifelse(is.na(href), "", href), url))
  place <- place[keep]
  place <- ifelse(is.na(place) | !nzchar(place), location, place)
  data.frame(Title = title[keep], Location = as.character(place), Posted_Date = NA_character_,
             Link = as.character(link), stringsAsFactors = FALSE)
}

#' Links as a page writes them, made absolute
#'
#' `xml2::url_absolute()` gives NA for an href with a space in it, which small sites write ("/files/Dean of
#' Students App Packet.pdf", a college page 2026-11-24). Spaces are encoded for the join and put back after, so the
#' link is the page's own (the same text the one-off parsers stored, and so the same posting link). An href that
#' still can't be joined is kept when absolute, joined to the page's host when it starts with "/", else the page.
#' @param href Hrefs as written on the page.
#' @param url The page's address.
#' @return Absolute links.
#' @keywords internal
css_board_absolute <- function(href, url) {
  spaced <- grepl(" ", href, fixed = TRUE)
  out <- xml2::url_absolute(gsub(" ", "%20", href, fixed = TRUE), url)
  out[spaced] <- gsub("%20", " ", out[spaced], fixed = TRUE)
  host <- sub("^(https?://[^/?#]+).*$", "\\1", url)
  bad <- is.na(out)
  out[bad] <- ifelse(grepl("^https?://", href[bad]), href[bad], ifelse(startsWith(href[bad], "/"), paste0(host, href[bad]), url))
  out
}
