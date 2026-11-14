# Apptegy district sites: the second CMS-family routine (2026-11-14).
#
# One routine for the Apptegy district sites, replacing a scraper per district
# (claude-notes/2026-11-14_apptegy-family/: measured as filter_v14 against each
# old parser's fixtures and live pages; Mike's decision 2026-11-13: the shared
# filter wins where it disagrees with an old parser). Apptegy pages are
# free-form text the district types in, so the routine is a line filter: keep
# the lines that read as job titles. Two ways in:
#   - "text": the rendered page's innerText (most districts);
#   - "pagebuilder": the page-builder JSON embedded in the HTML, for pages
#     whose jobs sit in collapsed accordions, buttons or cards that never
#     render as text at load time (Conrad, Gardiner, Drummond, Deer Lodge,
#     Savage, Olney-Bissell).
# Per-district data, all optional: where the job list starts, where it stops,
# and a section to skip ("CURRENTLY FILLED"), as perl regexes.

APPTEGY_ROLE_WORDS <- paste0("\\b(", paste(c(
  "teachers?", "aides?", "para(professional|educator)?s?", "coach(es|ing)?", "custodians?", "custodial", "janitor(ial)?",
  "cooks?", "drivers?", "bus", "secretar(y|ies)", "clerks?", "principals?", "superintendent", "counsel(or|ors|ing)",
  "nurses?", "substitutes?", "subs", "librarians?", "specialists?", "directors?", "coordinators?", "technicians?",
  "maintenance", "food service", "food program", "support staff", "kitchen", "cafeteria", "lunch", "advisors?",
  "instructors?", "interventionist", "psychologist", "therapist", "pathologist", "speech", "business manager",
  "bookkeeper", "assistants?", "tutors?", "transportation", "mechanic", "groundskeeper", "supervisors?", "dean",
  "athletic", "receptionist", "registrar", "officer", "office manager", "concessions?", "music", "band", "choir",
  "art", "math(ematics)?", "science", "english", "ela", "reading", "history", "social studies", "spanish",
  "physical education", "pe", "health", "special education", "sped", "kindergarten", "preschool", "k-?12", "7-12",
  "jh", "hs", "ag(riculture)?", "fcs", "industrial arts", "shop", "cte", "volleyball", "basketball", "football",
  "wrestling", "track", "cross country", "golf", "softball", "baseball", "cheer(leading)?", "speech and drama",
  "drama"), collapse = "|"), ")\\b")

# Lines that are never a title: instructions, contacts, addresses, pay and
# benefit lines, job-description headings, navigation and footer text.
APPTEGY_BOILER <- paste(c(
  "^(apply|please|click|submit|download|contact|email|e-mail|call|send|mail|visit|for (more|questions)|all application",
  "applications? (are|will|must|for|form|packet|materials|information)|the district|we |our |if you|to apply|interested)",
  "equal opportunity", "^find us$", "^skip to", "privacy", "^(phone|fax)\\b", "@", "https?://",
  "[0-9]{3}[-. )]+[0-9]{3}-[0-9]{4}", "\\bp\\.?o\\.? box\\b",
  paste0("^(key responsibilities|position summary|qualifications|requirements|duties|essential|job description|",
         "work schedule|evaluation|employment type|reports to|salary|compensation|about us|none at this time|",
         "no (current )?openings)"),
  "^[\u00b7\u2022*\u25aa\u25e6\u25cf\ufffd-]", "^city of\\b", "county$", "forms?\\b.*\\b(here|located)", "\\(fillable\\)",
  "sign-on bonus", "housing", "collective bargaining", "title ix", "^bp [0-9]+", "^copy of\\b", "\\bpolicy\\b",
  "^questions\\??$", "^[0-9]{4}\\s*[-\u2013]\\s*[0-9]{4}$", "applicants$", "jobs for teachers",
  "office of public instruction", "\\bor be\\b|\\bable to\\b", "\\bpositions? (are|is) (available|open)",
  "[0-9]{1,2}(:[0-9]{2})? ?(am|pm)\\b", "pay (scale|range)", "negotiated agreement", "video", "website", "\\bforms?$",
  "^\\$", "^(read more|back to top|google maps|tags:)", "^resume$", "^transcripts$", "licenses and credentials",
  "letters? of (recommendation|reference|interest)", "^(superintendent|principal|clerk|business manager),? [A-Z][a-z]+",
  "volunteer", "^(required|encouraged|job dependent)$", "until filled",
  "^(start date|department|supervisor|position type|school/building|supervisory|knowledge skills)",
  "\\b(mt|montana) [0-9]{5}\\b", "insurance", "benefits?( package)?$",
  "^(employment|current openings|job openings|open positions)$", "application$", "applications$", "^menu$",
  "^translate$", "^search site$", "^sign in$", "\\bportal\\b", "wish list", "^endorsements?\\b", "^flexible hours$",
  "holidays off"), collapse = "|")

#' Split one Apptegy line into its title part(s)
#'
#' "1. Assistant Cook 2. Activities Bus Driver" becomes two items; "Title:
#' details", "Title. details", "Title | details" and "Title - details" become
#' the title part when it is short and names a role. Any other line comes back
#' unchanged.
#' @param l One line of text.
#' @return Character vector of one or more items.
#' @export
apptegy_split_line <- function(l) {
  if (grepl("(^|\\s)1[.)]\\s+\\S.*\\s2[.)]\\s+\\S", l)) {
    items <- trimws(strsplit(sub("^.*?(^|\\s)1[.)]\\s+", "", l, perl = TRUE), "\\s+[0-9]+[.)]\\s+")[[1]])
    return(items[nzchar(items)])
  }
  # Separators inside parentheses don't count ("Food Service Worker (temporary
  # Oct. 2026-Dec 2026)- Job Description"): match on a copy with each
  # parenthetical blanked to the same length, then cut the original.
  masked <- l
  p <- gregexpr("\\([^)]*\\)", l)
  if (p[[1]][1] != -1) regmatches(masked, p) <- list(strrep("x", attr(p[[1]], "match.length")))
  # Perl engine: R's default engine ignores the lazy quantifier.
  m <- regmatches(masked, regexec("^(.{3,70}?)(:\\s+|\\.\\s+|\\s+\\|\\s+|\\s*[-\u2013\u2014]\\s+)(.{8,})$", masked, perl = TRUE))[[1]]
  if (length(m) == 4) {
    left <- trimws(substr(l, 1, nchar(m[2])))
    right <- trimws(substr(l, nchar(l) - nchar(m[4]) + 1, nchar(l)))
    left_np <- trimws(gsub("\\s*\\([^)]*\\)", "", left))
    if (lengths(strsplit(left_np, "\\s+")) <= 8 && grepl(APPTEGY_ROLE_WORDS, left_np, ignore.case = TRUE)) {
      # "Title. Sentence": the period ends the title unless it ends an
      # abbreviation ("Assistant H.S. Boys Basketball Coach").
      if (grepl("^\\.", m[3])) {
        if (grepl("(?i)(\\b[a-z]|\\b(asst|jr|sr|st|no|elem|dept|ed|esp))$", left, perl = TRUE)) return(l)
        return(left)
      }
      # Otherwise cut only when the rest is details, not part of the title
      # ("Music - Instrumental", "JH Football: Head Coach", "Bus Driver -
      # Substitutes"). A title part followed by a parenthetical aside keeps
      # the part ("Long Term Substitute - Kindergarten (Approximate start date ...)").
      if (!apptegy_is_detail(right)) return(l)
      right_np <- trimws(gsub("\\s*\\([^)]*\\)", "", right))
      if (nzchar(right_np) && !apptegy_is_detail(right_np) && grepl("\\)$", right)) {
        return(trimws(sub("\\s*\\([^)]*\\)$", "", l)))
      }
      return(left)
    }
  }
  l
}

# The part after a title's separator is details when it states pay, a year or
# date, reads as a sentence, or is application or job-description text.
apptegy_is_detail <- function(x) {
  grepl("\\$|[0-9]{2,}|[.!?]$|^[a-z]", x) || lengths(strsplit(x, "\\s+")) >= 6 ||
    grepl("(?i)\\b(job description|vacanc(y|ies)|apply|application|posting|details|click|see|deadline|open(ing)?|until|closes?|start(s|ing)?|begin(s|ning)?|immediate(ly)?|must|required|salary|pay|wage|stipend|doe|negotiable)\\b",
          x, perl = TRUE)
}

#' The job titles in an Apptegy page's text
#'
#' The shared title line filter: reads the page's lines top to bottom, stops
#' at Apptegy's "Find Us" footer (or `stop`), tracks section headers, splits
#' list and "Title: details" lines, and keeps a line that names a role, or a
#' short line under a jobs header. Drops bullets, contacts, sentences,
#' application-form links, pay-scale and benefit lines, job-description
#' headings, school names, navigation and footer lines.
#' @param text The page's text, one line per row (innerText).
#' @param start Optional perl regex: the list starts after the first matching
#'   line (no match: no titles). A start line that is itself a numbered list is
#'   read.
#' @param stop Optional perl regex: the list ends before the first matching line.
#' @param skip_section Optional perl regex: a section header whose lines are
#'   skipped until the next header ("CURRENTLY FILLED").
#' @return Character vector of titles, each once.
#' @export
apptegy_title_lines <- function(text, start = NA, stop = NA, skip_section = NA) {
  apptegy_title_rows(text, start, stop, skip_section)$Title
}

# The filter, keeping each title's source line (read for posted pay).
apptegy_title_rows <- function(text, start = NA, stop = NA, skip_section = NA) {
  none <- data.frame(Title = character(0), Line = character(0), stringsAsFactors = FALSE)
  if (is.null(text) || length(text) == 0 || is.na(text[1])) return(none)
  L <- trimws(strsplit(paste(text, collapse = "\n"), "\n")[[1]])
  L <- trimws(gsub("[\u00a0\t ]+", " ", L)); L <- L[nzchar(L)]
  if (!length(L)) return(none)
  ok <- function(x) length(x) == 1 && !is.na(x) && nzchar(x)
  if (ok(start)) {
    i <- grep(start, L, perl = TRUE)
    if (!length(i)) return(none)
    first <- if (length(apptegy_split_line(L[i[1]])) > 1) i[1] else i[1] + 1
    if (first > length(L)) return(none)
    L <- L[first:length(L)]
  }
  stop_at <- which(L == "Find Us" | (if (ok(stop)) grepl(stop, L, perl = TRUE) else FALSE))
  if (length(stop_at)) L <- L[seq_len(stop_at[1] - 1)]
  if (!length(L)) return(none)

  role_rx <- APPTEGY_ROLE_WORDS
  has_role <- function(x) grepl(role_rx, x, ignore.case = TRUE)
  titles <- character(0); lines <- character(0)
  keep <- function(t, src) { titles <<- c(titles, t); lines <<- c(lines, rep(src, length(t))) }
  skipping <- FALSE; header <- NA_character_; label_next <- FALSE
  for (li in seq_along(L)) {
    l <- L[li]; src <- l
    # Card template: a bare "Title:" label, then the title
    if (label_next) { label_next <- FALSE; if (!grepl(":$", l)) keep(l, src); next }
    if (l == "Title:") { label_next <- TRUE; next }
    is_header <- (ok(skip_section) && grepl(skip_section, l, perl = TRUE)) || grepl(":$", l) ||
      grepl("^(certified|certificated|classified|coaching|teaching|extra ?curricular|district staff|support staff|certified teachers|athletics?)( (positions|openings|staff|employees))?$", l, ignore.case = TRUE) ||
      (l == toupper(l) && grepl("(OPENINGS|POSITIONS|SUBSTITUTES|EMPLOYEES|OPPORTUNITIES)$", l)) ||
      grepl("^(job title|job type|job description|employment opportunities)$", l, ignore.case = TRUE) ||
      grepl("^(open|current|available|certified|classified|substitute|coaching|teaching|support)\\b.*\\b(positions|openings|staff)$", l, ignore.case = TRUE) ||
      grepl("^current openings$|^openings$|^vacancies$|^all other positions$|^(internal|external) openings$|^various .* positions open$|positions and substitutes$|^substitute positions opened?$", l, ignore.case = TRUE) ||
      grepl("^(classified|certified|certificated|coaching|extra ?curricular) positions?\\s*[-\u2013:]\\s*\\S+(,\\s*\\S+)+", l, ignore.case = TRUE)
    # A short "... Openings" / "Positions" line with no role word is a section header ("Other Employment Openings")
    is_header <- is_header || (lengths(strsplit(l, "\\s+")) <= 5 &&
                                 grepl("(?i)(openings|positions|vacancies|opportunities)$", l, perl = TRUE) && !has_role(l))
    if (is_header) { header <- l; skipping <- ok(skip_section) && grepl(skip_section, l, perl = TRUE); next }
    if (skipping) next
    # An all-caps title wrapped onto a second line ("SEEKING TWO (2) EVENING SHIFT SCHOOL CUSTODIANS" /
    # "AND ONE (1) MID SHIFT SCHOOL CUSTODIAN") continues the title kept from the line above.
    if (grepl("^AND\\s", l) && length(titles) && li > 1 && lines[length(lines)] == L[li - 1]) {
      titles[length(titles)] <- paste(titles[length(titles)], l); next
    }
    # A card's "Job Title:" label
    l <- trimws(sub("(?i)^(job |position )?title:\\s+", "", l, perl = TRUE))
    # "Elementary Teaching Position(permanent, full time)Beginning August": details glued on after a
    # parenthetical; a parenthetical that ends the line stays ("Coaches(1)", "(K-12 School)").
    if (length(apptegy_split_line(l)) == 1) l <- trimws(sub("\\s*\\([^)]*\\)(?=[A-Za-z]).*$", "", l, perl = TRUE))
    if (!nzchar(l)) next
    parts <- apptegy_split_line(l)
    if (length(parts) > 1) { keep(parts[has_role(parts)], src); next }
    l <- parts
    l <- trimws(sub("(?i)^(now )?(seeking|hiring|looking for)( an?)?\\s+", "", l, perl = TRUE))
    l <- trimws(sub("(?i)\\s*(for|in) the 20[0-9]{2}\\s*[-\u2013]\\s*(20)?[0-9]{2} school year\\.?$", "", l, perl = TRUE))
    if (!nzchar(l)) next
    words <- lengths(strsplit(l, "\\s+"))
    # Length is judged without parenthetical asides ("Two paraeducators (Bona fide occupational qualifications) to ...")
    l_np <- trimws(gsub("\\s*\\([^)]*\\)", "", l))
    if (nchar(l_np) > 90 || lengths(strsplit(l_np, "\\s+")) > 12) next
    if (grepl("[.!?]$", l) && words > 4) next
    if (grepl(APPTEGY_BOILER, l, ignore.case = TRUE)) next
    # A contact: a name above, an email below ("Sam Hoff" / "K-12 Principal" / "SamHoff@...")
    if (li > 1 && li < length(L) && grepl("@", L[li + 1]) && grepl("^[A-Z][a-z]+ [A-Z][a-z'-]+$", L[li - 1]) &&
        !has_role(L[li - 1])) next
    # A sentence fragment wrapped from the line above starts in lower case
    if (grepl("^[a-z]", l)) next
    # A sentence: ends in a period that is not an abbreviation's
    if (grepl("\\.$", l) && !grepl("\\b(asst|jr|sr|st|no|dept|ed|esp)\\.$", l, ignore.case = TRUE) && words >= 2) next
    # A school or district name on its own
    if (grepl("\\b(schools?|school district)$", l, ignore.case = TRUE) &&
        !has_role(sub("(?i)\\b(high|middle|elementary)? ?schools?$", "", l, perl = TRUE))) next
    # "Penny Benner, Superintendent": a contact, not a posting
    if (grepl("^[A-Z][a-z]+ [A-Z][a-z]+,\\s", l) && !has_role(sub(",.*$", "", l))) next
    if (grepl("(?i)^20[0-9]{2}\\s*[-\u2013]\\s*(20)?[0-9]{2} school year$", l, perl = TRUE)) next
    if (grepl("application", l, ignore.case = TRUE) && !has_role(sub("(?i)application.*$", "", l, perl = TRUE))) next
    under_jobs <- words <= 5 && !is.na(header) &&
      grepl("position|opening|vacanc|teaching|certified|classified|coach|substitute|staff|hiring", header, ignore.case = TRUE)
    if (!has_role(l) && under_jobs) {
      # A line naming no role under a header that does takes the header's role
      # ("Track Coaching Positions:" / "High School - Head" -> "Track Coach - High School - Head").
      core <- sub("(?i)\\s+(positions?|openings?|vacancies)$", "", sub(":$", "", header), perl = TRUE)
      core <- sub("(?i)coaching$", "Coach", core, perl = TRUE)
      # Two words or more: a bare "Coaching Positions:" adds nothing (Conrad's "ESports").
      if (has_role(core) && lengths(strsplit(core, "\\s+")) >= 2) l <- paste(core, "-", l)
    }
    if (has_role(l) || under_jobs) keep(l, src)
  }
  out <- data.frame(Title = titles, Line = lines, stringsAsFactors = FALSE)
  out[!duplicated(out$Title), , drop = FALSE]
}

#' Decode an Apptegy page-builder page's content tree
#'
#' Page-builder pages carry their content as `window.clientWorkStateTemp =
#' JSON.parse("...")`: a JSON string literal whose value is a second JSON
#' document. Both layers are undone.
#' @param html_text The page's HTML.
#' @return The page's `content$structure$nodes` list, or NULL.
#' @export
decode_apptegy_pagebuilder_nodes <- function(html_text) {
  if (is.null(html_text) || length(html_text) == 0 || is.na(html_text[1])) return(NULL)
  needle <- "window.clientWorkStateTemp = JSON.parse("
  start <- regexpr(needle, html_text, fixed = TRUE)
  if (start == -1) return(NULL)
  rest <- substring(html_text, start + attr(start, "match.length"))
  # One vectorized regex match for the escaped string: a per-character loop
  # took minutes on a ~200KB payload.
  quoted <- regmatches(rest, regexpr('^"(?:[^"\\\\]|\\\\.)*"', rest, perl = TRUE))
  if (length(quoted) == 0) return(NULL)
  inner <- tryCatch(jsonlite::fromJSON(quoted), error = function(e) NULL)
  if (is.null(inner)) return(NULL)
  data <- tryCatch(jsonlite::fromJSON(inner, simplifyVector = FALSE), error = function(e) NULL)
  data$page$content$structure$nodes
}

#' A page-builder HTML fragment as text lines
#'
#' @param html_frag A node's `content$html` (`<p>`/`<li>` items).
#' @return Character vector of non-empty lines.
#' @export
apptegy_html_fragment_to_lines <- function(html_frag) {
  if (is.null(html_frag) || !is.character(html_frag) || !nzchar(html_frag[1])) return(character(0))
  x <- gsub("<li>|<p( [^>]*)?>", "\n", html_frag[1])
  x <- gsub("</li>|</p>", "", x)
  x <- gsub("<[^>]+>", "", x)
  x <- gsub("&amp;", "&", x)
  lines <- trimws(strsplit(x, "\n")[[1]])
  lines[nzchar(lines)]
}

#' A page-builder content tree as text lines
#'
#' Panel names and headings become header lines ("Certified Positions:"), so
#' their content is read under them by [apptegy_title_lines()]. A card's
#' heading names a kind of job and its text lists the openings: titles in the
#' text are read under the heading; a text with no titles gives the heading
#' itself ("Bus Driver"); a card saying "No current openings" gives nothing.
#' @param nodes From [decode_apptegy_pagebuilder_nodes()].
#' @return Character vector of lines.
#' @export
apptegy_pagebuilder_lines <- function(nodes) {
  out <- character(0)
  html_text <- function(h) trimws(gsub("&nbsp;", " ", gsub("<[^>]+>", " ", h)))
  walk <- function(ns) for (n in ns) {
    ty <- if (is.null(n$type)) "" else n$type
    if (ty == "CONTENT_NODE_PANEL" && is.character(n$properties$text)) {
      out <<- c(out, paste0(sub(":$", "", trimws(n$properties$text)), ":"))
    } else if (ty == "CONTENT_NODE_HEADING" && is.character(n$content$html)) {
      h <- html_text(n$content$html); if (nzchar(h)) out <<- c(out, paste0(sub(":$", "", h), ":"))
    } else if (ty == "CONTENT_NODE_CARD" && is.list(n$content)) {
      hd <- n$content$heading$content$html; tx <- n$content$text$content$html
      hd <- if (is.character(hd)) html_text(hd) else ""
      body <- if (is.character(tx)) apptegy_html_fragment_to_lines(tx) else character(0)
      closed <- any(grepl("(?i)\\bno (current )?openings\\b", body, perl = TRUE))
      got <- if (length(body)) apptegy_title_lines(paste(c(paste0(sub(":$", "", hd), ":"), body), collapse = "\n")) else character(0)
      if (length(got)) {
        out <<- c(out, paste0(sub(":$", "", hd), ":"), body)
      } else if (!closed && nzchar(hd) && !(sub("(?i)\\s+wanted$", "", hd, perl = TRUE) %in% out)) {
        out <<- c(out, hd)
      }
    } else if (ty == "CONTENT_NODE_BUTTON" && is.list(n$content$buttons)) {
      out <<- c(out, unlist(lapply(n$content$buttons, function(b) if (is.character(b$title)) trimws(b$title))))
    } else if (is.character(n$content$html)) {
      out <<- c(out, apptegy_html_fragment_to_lines(n$content$html))
    } else if (is.character(n$properties$text)) {
      out <<- c(out, n$properties$text)
    }
    if (!is.null(n$nodes)) walk(n$nodes)
  }
  walk(nodes)
  out[nzchar(out)]
}

#' Fetch an Apptegy district's job postings
#'
#' Apptegy pages need a real browser (a plain request gets a bot-challenge
#' shell), so this takes a chromote session.
#' @param chromote_session A `chromote::ChromoteSession`.
#' @param url The district's jobs page (registry `Job_Link`).
#' @param location The posting location to record (registry `City`).
#' @param mode "text" (the rendered text) or "pagebuilder" (the page-builder
#'   JSON in the HTML).
#' @param start,stop,skip_section Optional per-district regexes, see
#'   [apptegy_title_lines()].
#' @return Data frame: `Title`, `Location`, `Posted_Date` (always NA: Apptegy
#'   pages give none), `Link` (the page), and the posted pay columns
#'   (`Pay_Min`, `Pay_Max`, `Pay_Unit`, `Pay_Text`) when a title's own line
#'   states pay.
#' @export
fetch_apptegy_postings <- function(chromote_session, url, location, mode = "text",
                                   start = NA, stop = NA, skip_section = NA) {
  mode <- match.arg(mode, c("text", "pagebuilder"))
  chromote_session$Page$navigate(url)
  chromote_session$Page$loadEventFired(wait_ = TRUE, timeout_ = 30)
  Sys.sleep(4)
  js <- if (mode == "text") "document.body.innerText" else "document.documentElement.outerHTML"
  content <- chromote_session$Runtime$evaluate(js)$result$value
  parse_apptegy_postings(content, url, location, mode, start, stop, skip_section)
}

#' Parse an Apptegy jobs page
#'
#' @param content The page's innerText (mode "text") or HTML (mode "pagebuilder").
#' @inheritParams fetch_apptegy_postings
#' @return See [fetch_apptegy_postings()].
#' @export
parse_apptegy_postings <- function(content, url, location, mode = "text",
                                   start = NA, stop = NA, skip_section = NA) {
  mode <- match.arg(mode, c("text", "pagebuilder"))
  na_rule <- function(x) if (is.null(x) || length(x) != 1 || is.na(x) || !nzchar(trimws(x))) NA_character_ else x
  start <- na_rule(start); stop <- na_rule(stop); skip_section <- na_rule(skip_section)
  text <- if (mode == "text") {
    content
  } else {
    nodes <- decode_apptegy_pagebuilder_nodes(content)
    if (is.null(nodes)) NULL else paste(apptegy_pagebuilder_lines(nodes), collapse = "\n")
  }
  rows <- if (is.null(text)) apptegy_title_rows(NULL) else apptegy_title_rows(text, start, stop, skip_section)
  out <- data.frame(Title = rows$Title, Location = rep(location, nrow(rows)), Posted_Date = rep(NA_character_, nrow(rows)),
                    Link = rep(url, nrow(rows)), stringsAsFactors = FALSE)
  # Pay from the title's own line, only when that line gave one title (a
  # "1. Cook 2. Driver" line's pay belongs to no single item).
  if (nrow(out) == 0) return(ensure_posted_pay_columns(out))
  shared <- rows$Line %in% rows$Line[duplicated(rows$Line)]
  pay <- extract_posted_pay(ifelse(shared, NA_character_, rows$Line))
  cbind(out, pay)
}
