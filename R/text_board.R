# Text boards: a job page with no platform and no useful markup, where postings are lines of text (package review step
# 8, 2026-11-25). One reader for the one-off pages whose parsers differed only in which lines they kept. Each source's
# rule is one or more rows of the state's own rules file (`text_board_rules.csv`), keyed by the registry name; the
# registry row gives the page and the place.

TEXT_BOARD_RULE_COLUMNS <- c("Source", "Start_Regex", "Stop_Regex", "Title_Regex", "Marker_Regex", "Exclude_Regex",
                             "Strip_Regex", "Pay_Regex", "Max_Title_Chars")
TEXT_BOARD_REGEX_COLUMNS <- grep("_Regex$", TEXT_BOARD_RULE_COLUMNS, value = TRUE)

#' Read and check a text-board rules file
#'
#' Rows with the same `Source` (the name in the state's registry) are that page's sections, read in order. All
#' patterns are case-insensitive Perl regular expressions, matched on one trimmed line of the page's text. Columns:
#' * `Start_Regex`, `Stop_Regex`: the section runs from the line after the first one matching `Start_Regex` to the line
#'   before the next one matching `Stop_Regex` (blank: the page's top or bottom). A set pattern the page doesn't have
#'   stops the reader, so a moved or redesigned page is an error in the scrape log instead of "no postings".
#' * `Title_Regex`: only matching lines are titles; its first capture group, when it has one, is the title.
#' * `Marker_Regex`: the title is the line just before each matching line (a line every posting repeats). Set one of
#'   `Title_Regex` and `Marker_Regex`, or neither (every line of the section is a title).
#' * `Exclude_Regex` (on the line): not titles. `Strip_Regex`: removed from the title. `Max_Title_Chars`: longer
#'   titles are dropped.
#' * `Pay_Regex`: which of a posting's own lines (its title line up to the next title's) state its pay, read with
#'   [extract_posted_pay()]. Blank: no pay is read, since a page's other amounts (a stipend in lieu of insurance) sit
#'   among its postings.
#' @param rules A data frame, or the path of a CSV.
#' @return The rules, every column present (missing optional ones blank), checked.
#' @export
text_board_rules <- function(rules) {
  if (is.character(rules)) rules <- utils::read.csv(rules, stringsAsFactors = FALSE, colClasses = "character",
                                                   na.strings = character(0))
  if (!is.data.frame(rules)) stop("text_board_rules(): a data frame or a CSV path")
  if (!"Source" %in% names(rules)) stop("text_board_rules(): needs a Source column")
  unknown <- setdiff(names(rules), TEXT_BOARD_RULE_COLUMNS)
  if (length(unknown)) stop("text_board_rules(): unknown column(s): ", paste(unknown, collapse = ", "))
  for (col in setdiff(TEXT_BOARD_RULE_COLUMNS, names(rules))) rules[[col]] <- rep("", nrow(rules))
  rules <- rules[TEXT_BOARD_RULE_COLUMNS]
  for (col in TEXT_BOARD_RULE_COLUMNS) {
    v <- as.character(rules[[col]])
    v[is.na(v)] <- ""
    # A pattern's own spaces count (" - .*$" strips a dash between words, not a leading one).
    rules[[col]] <- if (col %in% TEXT_BOARD_REGEX_COLUMNS) ifelse(nzchar(trimws(v)), v, "") else trimws(v)
  }
  where <- function(bad) paste(unique(rules$Source[bad]), collapse = ", ")
  if (any(!nzchar(rules$Source))) stop("text_board_rules(): a row has no Source")
  if (any(nzchar(rules$Title_Regex) & nzchar(rules$Marker_Regex))) {
    stop("text_board_rules(): set Title_Regex or Marker_Regex, not both, for ",
         where(nzchar(rules$Title_Regex) & nzchar(rules$Marker_Regex)))
  }
  # A rule that names no place on the page and no kind of line would make every line of the page a posting.
  open <- !nzchar(rules$Start_Regex) & !nzchar(rules$Title_Regex) & !nzchar(rules$Marker_Regex)
  if (any(open)) stop("text_board_rules(): set Start_Regex, Title_Regex or Marker_Regex for ", where(open))
  bad <- nzchar(rules$Max_Title_Chars) & !grepl("^[1-9][0-9]*$", rules$Max_Title_Chars)
  if (any(bad)) stop("text_board_rules(): Max_Title_Chars must be a whole number for ", where(bad))
  for (col in TEXT_BOARD_REGEX_COLUMNS) {
    for (i in which(nzchar(rules[[col]]))) {
      ok <- tryCatch({ suppressWarnings(grepl(rules[[col]][i], "", perl = TRUE)); TRUE }, error = function(e) FALSE)
      if (!ok) stop("text_board_rules(): ", col, " for ", rules$Source[i], " is not a valid regular expression")
    }
  }
  rownames(rules) <- NULL
  rules
}

#' One source's text-board rule
#'
#' @param rules A [text_board_rules()] result (or anything it accepts).
#' @param source The source's registry name.
#' @return The source's rows, one per section.
#' @export
text_board_rule <- function(rules, source) {
  if (!is.data.frame(rules) || !identical(names(rules), TEXT_BOARD_RULE_COLUMNS)) rules <- text_board_rules(rules)
  hit <- rules[rules$Source == source, , drop = FALSE]
  if (nrow(hit) == 0) stop("text_board_rule(): no rule for '", source, "' in the text-board rules")
  rownames(hit) <- NULL
  hit
}

#' A dispatch spec for text boards
#'
#' For a state's [scraper_specs()]: `TextBoard = text_board_spec(...)` calls [fetch_text_board_postings()] with the
#' registry row's page, its place and its rule.
#' @inheritParams css_board_spec
#' @param rules The rules ([text_board_rules()]), a CSV path, or a function returning either (read when a source is
#'   looked up).
#' @return A [scraper_spec()].
#' @export
text_board_spec <- function(rules, url_col, place_col, name_col) {
  force(rules); force(name_col)
  scraper_spec("fetch_text_board_postings", list(
    url_col, registry_text(place_col),
    function(row) text_board_rule(if (is.function(rules)) rules() else rules, row[[name_col]])),
    parse_fn = "parse_text_board")
}

#' Fetch a text board's postings
#'
#' One plain request, then [parse_text_board()].
#' @param url The job page.
#' @param location The registry's place.
#' @param rule The source's rule ([text_board_rule()]).
#' @return As [parse_text_board()].
#' @export
fetch_text_board_postings <- function(url, location, rule) {
  resp <- request(url) %>% httr2::req_user_agent(POSTING_STATUS_UA) %>% perform_with_retry()
  parse_text_board(resp_body_string(resp), url, location, rule)
}

#' Parse a text board
#'
#' Reads a job page's lines with `rule` ([text_board_rules()] describes it). Every title line is a row: no
#' de-duplication, since two openings can share a title.
#' @param html_text The page.
#' @param url The page's address, every posting's link.
#' @param location The registry's place.
#' @param rule The source's rule ([text_board_rule()]).
#' @return Title, Location, Posted_Date (NA: these pages give none), Link and the posted pay columns.
#' @export
parse_text_board <- function(html_text, url, location, rule) {
  stop_if_no_html(html_text)
  lines <- strsplit(rvest::html_text2(rvest::read_html(html_text)), "\n", fixed = TRUE)[[1]]
  lines <- trimws(gsub("[[:space:] ]+", " ", gsub("[​‍﻿]", "", lines)))
  lines <- lines[nzchar(lines)]
  parts <- lapply(seq_len(nrow(rule)), function(i) text_board_section(lines, as.list(rule[i, , drop = FALSE]), url))
  rows <- do.call(rbind, parts)
  out <- data.frame(Title = rows$Title, Location = rep(as.character(location), nrow(rows)),
                    Posted_Date = rep(NA_character_, nrow(rows)), Link = rep(url, nrow(rows)), stringsAsFactors = FALSE)
  if (nrow(out) == 0) return(ensure_posted_pay_columns(out))
  cbind(out, extract_posted_pay(rows$Pay_Line))
}

# One section's titles, and the text their pay is read from.
text_board_section <- function(lines, rule, url) {
  has <- function(pattern, x) grepl(pattern, x, ignore.case = TRUE, perl = TRUE)
  found <- function(hits, col) {
    if (length(hits) == 0) stop("text board: the page has no line matching ", col, " '", rule[[col]],
                                "'; the page may have moved or changed: ", url, call. = FALSE)
    hits[1]
  }
  if (nzchar(rule$Start_Regex)) {
    lines <- utils::tail(lines, -found(which(has(rule$Start_Regex, lines)), "Start_Regex"))
  }
  if (nzchar(rule$Stop_Regex)) {
    lines <- utils::head(lines, found(which(has(rule$Stop_Regex, lines)), "Stop_Regex") - 1L)
  }
  at <- if (nzchar(rule$Marker_Regex)) { m <- which(has(rule$Marker_Regex, lines)); m[m > 1] - 1L
        } else if (nzchar(rule$Title_Regex)) which(has(rule$Title_Regex, lines))
        else seq_along(lines)
  if (nzchar(rule$Exclude_Regex)) at <- at[!has(rule$Exclude_Regex, lines[at])]
  title <- lines[at]
  if (nzchar(rule$Title_Regex)) {
    m <- regmatches(title, regexec(rule$Title_Regex, title, ignore.case = TRUE, perl = TRUE))
    title <- vapply(m, function(x) if (length(x) > 1) x[2] else NA_character_, "")
    title <- ifelse(is.na(title), lines[at], title)
  }
  if (nzchar(rule$Strip_Regex)) title <- gsub(rule$Strip_Regex, "", title, ignore.case = TRUE, perl = TRUE)
  title <- trimws(title)
  substr(title, 1, 1) <- toupper(substr(title, 1, 1))     # a title cut from a sentence ("looking for paraprofessionals")
  pay_line <- vapply(seq_along(at), function(k) {
    if (!nzchar(rule$Pay_Regex)) return(NA_character_)
    block <- lines[at[k]:(if (k < length(at)) at[k + 1] - 1L else length(lines))]
    hit <- block[has(rule$Pay_Regex, block)]
    if (length(hit)) paste(hit, collapse = " ") else NA_character_
  }, "")
  keep <- nzchar(title)
  if (nzchar(rule$Max_Title_Chars)) keep <- keep & nchar(title) <= as.integer(rule$Max_Title_Chars)
  data.frame(Title = title[keep], Pay_Line = pay_line[keep], stringsAsFactors = FALSE)
}
