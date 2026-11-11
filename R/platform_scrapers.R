# Shared fetch and parse functions for job-board platforms both states scrape:
# AppliTrack, SchoolSpring (posting details and ending reasons), PeopleAdmin
# pay and dates, and NEOGOV card pay. Moved from the MT and WY repos'
# direct_api_scrapers.R in phase 3 (2026-11-11), where these definitions were
# identical. A fix to one of these belongs here; the Claude auto-fix opens its
# PR on this repo when the broken scraper lives here.

#' `PEOPLEADMIN_OPEN_LABELS`
#'
#' A PeopleAdmin posting page's own open and close dates, as YYYY-MM-DD. Each
#' college labels them its own way: MSU campuses, Western and Eastern "Open
#' Date"/"Close Date", Northwest "Posting Date"/"Closing Date", Sheridan "Job
#' Posting Date" and "Initial Review Date". A close date is kept even when
#' "Open Until Filled" is also Yes (it is then the first-review deadline,
#' like AppliTrack's "Closing Date"); with no close date, "Open Until Filled:
#' Yes" gives Close_Date "Open Until Filled". Blank fields are NA.
#' @export
PEOPLEADMIN_OPEN_LABELS <- c("Open Date", "Posting Date", "Job Posting Date")

#' `PEOPLEADMIN_CLOSE_LABELS`
#' @export
PEOPLEADMIN_CLOSE_LABELS <- c("Close Date", "Closing Date", "Initial Review Date")

#' Parse peopleadmin posting dates
#' @param html_text See the description.
#' @export
parse_peopleadmin_posting_dates <- function(html_text) {
  none <- data.frame(Open_Date = NA_character_, Close_Date = NA_character_, stringsAsFactors = FALSE)
  if (is.na(html_text)) return(none)
  rows <- rvest::html_elements(rvest::read_html(html_text), xpath = "//tr[th and td]")
  labels <- tolower(trimws(rvest::html_text2(rvest::html_element(rows, "th"))))
  values <- trimws(gsub("\\s+", " ", rvest::html_text2(rvest::html_element(rows, "td"))))
  first_date <- function(wanted) {
    v <- values[labels %in% tolower(wanted)]
    d <- suppressWarnings(as.Date(v, format = "%m/%d/%Y"))
    d <- d[!is.na(d) & grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", v)]
    if (length(d) == 0) NA_character_ else format(d[1])
  }
  close <- first_date(PEOPLEADMIN_CLOSE_LABELS)
  if (is.na(close) && any(labels == "open until filled" & grepl("^yes", values, ignore.case = TRUE))) {
    close <- "Open Until Filled"
  }
  data.frame(Open_Date = first_date(PEOPLEADMIN_OPEN_LABELS), Close_Date = close, stringsAsFactors = FALSE)
}

#' `PEOPLEADMIN_PAY_LABELS`
#'
#' Pay from a PeopleAdmin posting page. Each college labels its pay field
#' differently (MSU campuses "Salary", Western "Estimated Salary", Sheridan
#' "Hiring Range"/"Hourly Rate (if applicable)"/"Full Salary Range", Northwest
#' "Starting Salary Range"); the first labeled field stating an amount wins,
#' and its text is kept as Pay_Text. When the figure has no unit next to it,
#' the unit comes from the posting's own "Compensation" field (MSU Billings,
#' Northern, Great Falls: Annual / Hourly / Monthly / By Credit) or from an
#' "Hourly Rate" label. Eastern has no pay field at all and puts pay in the job
#' description ("Salary Hire Range: $60,598 - 66,889 annually"), so a page
#' with no pay field is read from all its fields' text instead; a page that has
#' one but states no amount ("commensurate with experience") stays NA.
#' @export
PEOPLEADMIN_PAY_LABELS <- c("Salary", "Estimated Salary", "Hiring Range", "Starting Salary Range",
                            "Hourly Rate (if applicable)", "Full Salary Range")

#' Parse peopleadmin posting pay
#' @param html_text See the description.
#' @export
parse_peopleadmin_posting_pay <- function(html_text) {
  none <- extract_posted_pay(NA_character_)
  if (is.na(html_text)) return(none)
  rows <- rvest::html_elements(rvest::read_html(html_text), xpath = "//tr[th and td]")
  labels <- trimws(rvest::html_text2(rvest::html_element(rows, "th")))
  values <- trimws(gsub("\\s+", " ", rvest::html_text2(rvest::html_element(rows, "td"))))

  pay_fields <- intersect(PEOPLEADMIN_PAY_LABELS, labels)
  if (length(pay_fields) == 0) return(extract_posted_pay(paste(values, collapse = ". ")))
  for (label in pay_fields) {
    value <- values[match(label, labels)]
    pay <- extract_posted_pay(value)
    if (is.na(pay$Pay_Min)) next
    if (is.na(pay$Pay_Unit)) {
      basis <- tolower(values[match("Compensation", labels)])
      pay$Pay_Unit <- dplyr::case_when(
        grepl("^hourly", label, ignore.case = TRUE) ~ "hour",
        basis %in% "annual" ~ "year",
        basis %in% "hourly" ~ "hour",
        basis %in% "monthly" ~ "month",
        basis %in% "by credit" ~ "credit hour",
        # A bare $7-$100 figure in a pay field ("Hiring Range: $18.44 -
        # $23.06") can only be an hourly wage.
        pay$Pay_Max >= 7 & pay$Pay_Max <= 100 ~ "hour",
        TRUE ~ NA_character_
      )
    }
    # A long salary paragraph keeps only the extractor's snippet around the amount.
    if (nchar(value) <= 200) pay$Pay_Text <- value
    return(pay)
  }
  none
}

#' Parse neogov card pay
#'
#' Posted pay from each card of a NEOGOV (schooljobs.com / governmentjobs.com)
#' job-list fragment, keyed by the card's data-job-id. The card's meta line
#' carries the board's pay field ("Full-Time - $61,356.00 - $76,695.00
#' Annually"); "Depends on Qualifications" or "See Position Description" means
#' none is stated, and then the card's description preview is read instead
#' ("Current adjunct pay is $775 per credit hour"). Shared verbatim with the
#' Montana dashboard's direct_api_scrapers.R.
#' @param html_text See the description.
#' @export
parse_neogov_card_pay <- function(html_text) {
  container <- rvest::html_elements(rvest::read_html(html_text), "ul.job-listing-container")
  cards <- rvest::html_elements(container, "li.list-item")
  if (length(cards) == 0) {
    return(ensure_posted_pay_columns(data.frame(Job_ID = character(0), stringsAsFactors = FALSE)))
  }
  meta_pay <- vapply(cards, function(card) {
    lines <- rvest::html_text2(rvest::html_elements(card, ".list-meta li"))
    lines <- lines[grepl("$", lines, fixed = TRUE)]
    if (length(lines) == 0) NA_character_ else trimws(lines[1])
  }, character(1))
  preview <- rvest::html_text2(rvest::html_element(cards, ".list-entry"))
  pay <- extract_posted_pay(ifelse(is.na(meta_pay), preview, meta_pay))
  from_meta <- !is.na(meta_pay) & !is.na(pay$Pay_Min)
  pay$Pay_Text[from_meta] <- meta_pay[from_meta]
  # The pay field's unit is the board's pay type, which can be wrong: Gillette's
  # adjunct cards say "Adjunct - $900.00 Hourly" while the description says
  # "Grade: $900 per credit hour" (2026-11-10). When the field's unit is
  # rejected as implausible, the description preview's pay is used instead,
  # if it states a unit.
  no_unit <- which(from_meta & is.na(pay$Pay_Unit) & !is.na(preview))
  if (length(no_unit) > 0) {
    alt <- extract_posted_pay(preview[no_unit])
    use <- !is.na(alt$Pay_Min) & !is.na(alt$Pay_Unit)
    for (col in POSTED_PAY_COLUMNS) pay[[col]][no_unit[use]] <- alt[[col]][use]
  }
  cbind(data.frame(Job_ID = rvest::html_attr(cards, "data-job-id"), stringsAsFactors = FALSE), pay)
}

#' `SCHOOLSPRING_STATUS`
#'
#' One posting's own record (api.schoolspring.com/api/Jobs/<jobId>, the call
#' the posting page makes; no login). It carries what the listing doesn't:
#' 
#' - Close_Date: the deadline applicants see (formattedExternalCloseDate,
#'   "Nov 13, 2026 11:59 PM (Mountain Standard Time)"). The raw closeDate is
#'   UTC (2026-11-14T06:59), and "Posted until filled" is stored as
#'   2050-01-01, so it is NA here.
#' - Job_Status: jobStatusID 30 posted, 40 review (closed to applicants),
#'   50 filled, 60 canceled. Live listings only show 30; the record keeps its
#'   status after the posting leaves the board, which is what tells "filled"
#'   from "canceled" (posting-dates plan, step 4).
#' - Posted pay from payMin/payMax/salaryCode when payDisplay is on (88 of
#'   107 MT postings, 2026-11-03; the listing has none). 0 means "not given".
#'   salaryCode is the district's own label, so units are read with the
#'   amount: "Per Year" under $10,000 is a coaching/extra-duty stipend
#'   (Ronan $1,919.10), "Hourly and Certified Pay" holds both teacher
#'   salaries ($45,450) and hourly rates ($24.38), and "Substitute" (WY: $150,
#'   $14.95, $1,132) doesn't say per hour, day or season, so it is left
#'   without pay, as is any other label ("Please see our Salary Schedule").
#'   WY 2026-11-04: 132 of 214 postings show pay; 19 have a dated close.
#' 
#' displayDate (the listing's posted date, kept as Posted_Date) stays the
#' start date: postDate is when the record was created, often a few days
#' before it went public and sometimes re-saved years later.
#' @export
SCHOOLSPRING_STATUS <- c(`30` = "posted", `40` = "review", `50` = "filled", `60` = "canceled")

#' Parse schoolspring job json
#' @param json_text See the description.
#' @export
parse_schoolspring_job_json <- function(json_text) {
  v <- jsonlite::fromJSON(json_text, simplifyVector = TRUE)$value
  ji <- v$jobInfo
  num <- function(x) { x <- suppressWarnings(as.numeric(x)); if (length(x) == 0 || is.na(x) || x <= 0) NA_real_ else x }

  closing <- v$formattedExternalCloseDate
  close_date <- if (is.null(closing) || is.na(closing)) NA_character_ else {
    d <- as.Date(sub("^([A-Za-z]{3} \\d{1,2}, \\d{4}).*$", "\\1", closing), format = "%b %d, %Y")
    if (is.na(d)) NA_character_ else format(d)
  }

  status <- unname(SCHOOLSPRING_STATUS[as.character(ji$jobStatusID)])
  if (length(status) == 0) status <- NA_character_
  if (is.na(status) && !is.null(ji$jobStatusID)) status <- paste0("status ", ji$jobStatusID)

  lo <- num(ji$payMin); hi <- num(ji$payMax)
  lo <- dplyr::coalesce(lo, hi); hi <- dplyr::coalesce(hi, lo)
  code <- if (is.null(ji$salaryCode)) "" else trimws(as.character(ji$salaryCode))
  unit <- if (!isTRUE(as.integer(ji$payDisplay) == 1L) || is.na(hi)) NA_character_ else
    switch(code,
           "Per Hour" = "hour",
           "Per Day" = "day",
           "Stipend" = "stipend",
           "Per Year" = if (hi >= 10000) "year" else "stipend",
           "Hourly and Certified Pay" = if (hi >= 10000) "year" else if (hi <= 200) "hour" else NA_character_,
           NA_character_)
  pay_text <- if (is.na(unit)) NA_character_ else
    paste0(if (lo == hi) format_pay_amount(lo) else paste0(format_pay_amount(lo), "-", format_pay_amount(hi)),
           " ", code, " (SchoolSpring pay field)")

  data.frame(job_id = as.character(ji$jobId), Close_Date = close_date, Job_Status = status,
             Pay_Min = if (is.na(unit)) NA_real_ else lo, Pay_Max = if (is.na(unit)) NA_real_ else hi,
             Pay_Unit = unit, Pay_Text = pay_text, stringsAsFactors = FALSE)
}

#' Format pay amount
#' @param x See the description.
#' @export
format_pay_amount <- function(x) paste0("$", formatC(x, format = "f", digits = if (x %% 1 == 0) 0 else 2, big.mark = ","))

#' `SCHOOLSPRING_DETAIL_EMPTY`
#' @export
SCHOOLSPRING_DETAIL_EMPTY <- data.frame(job_id = character(0), Close_Date = character(0), Job_Status = character(0),
                                         Pay_Min = numeric(0), Pay_Max = numeric(0), Pay_Unit = character(0),
                                         Pay_Text = character(0), stringsAsFactors = FALSE)

#' Fetch schoolspring details
#'
#' Detail records for a set of job IDs, one request each. A failed request
#' leaves only that posting's fields NA (it is skipped here and NA after the
#' join), and fetching stops once budget_secs is spent, so a slow API can't
#' hold up the weekly run.
#' @param job_ids See the description.
#' @param delay See the description.
#' @param budget_secs See the description.
#' @export
fetch_schoolspring_details <- function(job_ids, delay = 0.2, budget_secs = 300) {
  deadline <- Sys.time() + budget_secs
  out <- lapply(unique(job_ids[!is.na(job_ids)]), function(id) {
    if (Sys.time() > deadline) return(NULL)
    Sys.sleep(delay)
    tryCatch({
      resp <- request(paste0("https://api.schoolspring.com/api/Jobs/", id)) %>%
        req_timeout(30) %>%
        perform_with_retry()
      parse_schoolspring_job_json(resp_body_string(resp))
    }, error = function(e) NULL)
  })
  dplyr::bind_rows(SCHOOLSPRING_DETAIL_EMPTY, dplyr::bind_rows(out))
}

#' Add schoolspring details
#'
#' The listing plus each posting's detail fields (Close_Date, Job_Status,
#' Pay_*), joined on the job ID in Link. Run before
#' normalize_id_backed_k12_postings(), which passes Pay_* through.
#' @param postings See the description.
#' @param ... See the description.
#' @export
add_schoolspring_details <- function(postings, ...) {
  if (nrow(postings) == 0) return(cbind(postings, SCHOOLSPRING_DETAIL_EMPTY[0, -1]))
  ids <- sub("^.*/jobs/(\\d+)$", "\\1", postings$Link)
  details <- fetch_schoolspring_details(ids, ...)
  details[match(ids, details$job_id), -1, drop = FALSE] %>%
    { rownames(.) <- NULL; cbind(postings, .) }
}

#' `SCHOOLSPRING_FINAL_STATUS`
#'
#' How SchoolSpring postings ended (posting-dates plan, step 4)
#' A SchoolSpring posting's record keeps its status after it leaves the board,
#' so each week every posting that the raw archives listed before but not today
#' is looked up: filled, canceled, or review (closed to applicants, outcome not
#' yet entered). Filled and canceled are final; review is rechecked weekly, as
#' it can still turn into filled. How a district uses the statuses varies, so
#' "filled" is not comparable across districts: all 12 Libby postings that left
#' Aug-Oct 2026 were still "review", and Campbell County WY closed 79 as
#' "canceled" against 8 "filled" (first lookup, 2026-11-04).
#' A lookup that still says "posted" means our scrape missed a live posting
#' (a failed run, say), so nothing is recorded for it.
#' 
#' schoolspring_endings.csv, one row per ended posting: job_id, District,
#' Title, Posted_Date (board's own), First_Seen/Last_Seen (archive dates it was
#' listed), Job_Status, Status_Since (first check that saw this status) and
#' Last_Checked. Last_Seen is the last weekly sighting, so the true end lies
#' between Last_Seen and the next run.
#' @export
SCHOOLSPRING_FINAL_STATUS <- c("filled", "canceled")

#' Schoolspring job id
#' @param url See the description.
#' @export
schoolspring_job_id <- function(url) {
  url <- as.character(url)
  ifelse(!is.na(url) & grepl("schoolspring\\.com/jobs/\\d+$", url), sub("^.*/jobs/", "", url), NA_character_)
}

#' Schoolspring sightings
#'
#' One row per SchoolSpring job ID seen in the archives since `since`, with
#' the dates it was first and last listed. District names go through
#' canonicalize_k12_district() when it is loaded (old WY archives spell
#' "Campbell County Scholl District 1").
#' @param archive_dir See the description.
#' @param since See the description.
#' @param canonicalize See the description.
#' @export
schoolspring_sightings <- function(archive_dir = "Archivek12_Data", since = Sys.Date() - 365,
                                   canonicalize = get0("canonicalize_k12_district", envir = globalenv(), mode = "function",
                                                       ifnotfound = identity)) {
  files <- list.files(archive_dir, "^combined_\\d{4}-\\d{2}-\\d{2}\\.csv$", full.names = TRUE)
  dates <- as.Date(sub("^combined_(.*)\\.csv$", "\\1", basename(files)))
  rows <- lapply(which(dates >= since), function(i) {
    d <- utils::read.csv(files[i], colClasses = "character", check.names = FALSE)
    id <- schoolspring_job_id(d$url)
    ok <- !is.na(id)
    if (!any(ok)) return(NULL)
    data.frame(job_id = id[ok], District = canonicalize(d$District[ok]), Title = d$title[ok],
               Posted_Date = d$date_posted[ok], Seen = dates[i], stringsAsFactors = FALSE)
  })
  rows <- dplyr::bind_rows(rows)
  if (nrow(rows) == 0) {
    return(data.frame(job_id = character(0), District = character(0), Title = character(0),
                      Posted_Date = character(0), First_Seen = as.Date(character(0)),
                      Last_Seen = as.Date(character(0)), stringsAsFactors = FALSE))
  }
  rows %>%
    dplyr::arrange(Seen) %>%
    dplyr::group_by(job_id) %>%
    dplyr::summarize(District = dplyr::last(District), Title = dplyr::last(Title),
                     Posted_Date = dplyr::first(Posted_Date), First_Seen = min(Seen),
                     Last_Seen = max(Seen), .groups = "drop") %>%
    as.data.frame()
}

#' `SCHOOLSPRING_ENDINGS_EMPTY`
#' @export
SCHOOLSPRING_ENDINGS_EMPTY <- data.frame(
  job_id = character(0), District = character(0), Title = character(0), Posted_Date = character(0),
  First_Seen = character(0), Last_Seen = character(0), Job_Status = character(0),
  Status_Since = character(0), Last_Checked = character(0), stringsAsFactors = FALSE)

#' Update schoolspring endings
#'
#' Looks up every posting listed before but not on `today` (unless it already
#' has a final status or was last listed more than recheck_days ago) and
#' returns the updated endings table. `fetch` is fetch_schoolspring_details()
#' in production.
#' @param sightings See the description.
#' @param endings See the description.
#' @param today See the description.
#' @param fetch See the description.
#' @param recheck_days See the description.
#' @export
update_schoolspring_endings <- function(sightings, endings = SCHOOLSPRING_ENDINGS_EMPTY, today = Sys.Date(),
                                        fetch = fetch_schoolspring_details, recheck_days = 365) {
  endings <- dplyr::bind_rows(SCHOOLSPRING_ENDINGS_EMPTY, dplyr::mutate(endings, dplyr::across(dplyr::everything(), as.character)))
  final_ids <- endings$job_id[endings$Job_Status %in% SCHOOLSPRING_FINAL_STATUS]
  gone <- sightings[sightings$Last_Seen < today & sightings$Last_Seen >= today - recheck_days &
                      !sightings$job_id %in% final_ids, , drop = FALSE]
  if (nrow(gone) == 0) return(endings)

  details <- fetch(gone$job_id)
  details <- details[!is.na(details$Job_Status) & details$Job_Status != "posted", c("job_id", "Job_Status"), drop = FALSE]
  if (nrow(details) == 0) return(endings)

  today <- format(as.Date(today))
  found <- merge(gone, details, by = "job_id")
  prev <- endings[match(found$job_id, endings$job_id), , drop = FALSE]
  same <- !is.na(prev$Job_Status) & prev$Job_Status == found$Job_Status
  updated <- data.frame(job_id = found$job_id, District = found$District, Title = found$Title,
                        Posted_Date = found$Posted_Date, First_Seen = format(found$First_Seen),
                        Last_Seen = format(found$Last_Seen), Job_Status = found$Job_Status,
                        Status_Since = ifelse(same, prev$Status_Since, today), Last_Checked = today,
                        stringsAsFactors = FALSE)
  dplyr::bind_rows(endings[!endings$job_id %in% updated$job_id, , drop = FALSE], updated) %>%
    dplyr::arrange(Last_Seen, job_id)
}

#' Refresh schoolspring endings
#'
#' Weekly step: reads the archives and the endings file, looks up postings
#' that left the board, rewrites the file. Run after today's raw archive is
#' written. Returns the table (invisibly).
#' @param archive_dir See the description.
#' @param path See the description.
#' @param today See the description.
#' @param ... See the description.
#' @export
refresh_schoolspring_endings <- function(archive_dir = "Archivek12_Data", path = "schoolspring_endings.csv",
                                         today = Sys.Date(), ...) {
  endings <- if (file.exists(path)) utils::read.csv(path, colClasses = "character") else SCHOOLSPRING_ENDINGS_EMPTY
  sightings <- schoolspring_sightings(archive_dir, since = as.Date(today) - 365)
  out <- update_schoolspring_endings(sightings, endings, today = as.Date(today), ...)
  utils::write.csv(out, path, row.names = FALSE)
  message("SchoolSpring endings: ", nrow(out), " postings (",
          paste(names(table(out$Job_Status)), table(out$Job_Status), collapse = ", "), ")")
  invisible(out)
}

#' Applitrack posting url
#'
#' The posting's own page on the district's AppliTrack board (the link the
#' board's "Email To A Friend" button shares). NA without a job ID.
#' @param tenant_path See the description.
#' @param job_id See the description.
#' @export
applitrack_posting_url <- function(tenant_path, job_id) {
  ifelse(is.na(job_id) | !nzchar(job_id), NA_character_,
         paste0("https://www.applitrack.com/", tenant_path,
                "/onlineapp/default.aspx?AppliTrackLayoutMode=detail&AppliTrackJobId=", job_id))
}

#' Fetch applitrack postings
#' @param tenant_path See the description.
#' @export
fetch_applitrack_postings <- function(tenant_path) {
  resp <- request(paste0("https://www.applitrack.com/", tenant_path, "/onlineapp/jobpostings/Output.asp")) %>%
    req_url_query(all = "1") %>%
    perform_with_retry()
  # Applitrack serves Windows-1252 bytes with no charset in Content-Type, so
  # httr2 guesses UTF-8. Any posting containing a curly quote/en-dash/etc.
  # then fails to decode -- resp_body_string() returns NA rather than
  # erroring, and parse_applitrack_output(NA) silently yields zero rows.
  # That looks identical to a district with genuinely no openings, and it's
  # exactly what happened: confirmed live for 10 of 25 districts, one
  # (Sweetwater County SD1) hiding 69 real postings this way.
  parse_applitrack_output(resp_body_string(resp, encoding = "Windows-1252"))
}

#' Parse applitrack output
#' @param js_text See the description.
#' @export
parse_applitrack_output <- function(js_text) {
  # Response body is a series of document.write('<html fragment>') calls
  # with single quotes escaped as \' for JS string-literal safety; joining
  # every literal's un-escaped content reconstructs the full HTML the
  # browser would otherwise inject into the page.
  writes <- regmatches(js_text, gregexpr("document\\.write\\('(.*?)'\\);", js_text))[[1]]
  literals <- sub("^document\\.write\\('(.*)'\\);$", "\\1", writes)
  html <- paste(gsub("\\\\'", "'", literals), collapse = "")

  if (!nzchar(html)) {
    return(data.frame(title = character(0), position = character(0), position2 = character(0),
                       date_posted = character(0), location = character(0), closing_date = character(0),
                       job_id = character(0),
                       Pay_Min = numeric(0), Pay_Max = numeric(0), Pay_Unit = character(0),
                       Pay_Text = character(0), stringsAsFactors = FALSE))
  }

  soup <- rvest::read_html(html)
  postings <- rvest::html_elements(soup, "ul.postingsList")

  if (length(postings) == 0) {
    return(data.frame(title = character(0), position = character(0), position2 = character(0),
                       date_posted = character(0), location = character(0), closing_date = character(0),
                       job_id = character(0),
                       Pay_Min = numeric(0), Pay_Max = numeric(0), Pay_Unit = character(0),
                       Pay_Text = character(0), stringsAsFactors = FALSE))
  }

  rows <- lapply(postings, function(p) {
    title <- rvest::html_text2(rvest::html_element(p, "table.title td#wrapword"))
    # Each field is its own <li>: one span.label and its span.normal
    # value(s). "Position Type:" has one value or two (category +
    # subcategory, e.g. "Support Staff/" + "Para-Educator Special
    # Services"). Values are read per <li>, never by a running position
    # across the posting: guessing whether a second Position Type value
    # existed (from whether the next value had a ":") swallowed the next
    # field's value when it didn't, e.g. a "Date Posted" date, and shifted
    # every later field by one (WY Teton #1 and Carbon #1, 2026-11-03).
    # Description <li>s carry no span.label and are skipped.
    field <- stats::setNames(as.list(rep(NA_character_, 5)), c("position", "position2", "date_posted", "location", "closing_date"))
    for (li in rvest::html_elements(p, xpath = ".//li[span[contains(concat(' ', normalize-space(@class), ' '), ' label ')]]")) {
      label <- trimws(rvest::html_text2(rvest::html_element(li, "span.label")))
      values <- rvest::html_text2(rvest::html_elements(li, "span.normal"))
      if (length(values) == 0) next
      if (label == "Position Type:") {
        field$position <- values[1]
        if (length(values) >= 2) field$position2 <- values[2]
      } else if (label == "Date Posted:") {
        field$date_posted <- values[1]
      } else if (label == "Location:") {
        field$location <- values[1]
      } else if (label == "Closing Date:") {
        field$closing_date <- values[1]
      }
    }

    # Most AppliTrack postings state their pay in the body ("SALARY:
    # $15.14 per hour", "Stipend: $3,548.00", "$46,500-$71,803"), which this
    # all=1 listing already includes -- no extra request needed.
    pay <- extract_posted_pay(rvest::html_text2(p))

    # AppliTrack's own per-posting ID: the block's id ("p2138_") and the
    # "JobID: 2138" beside its title. It makes a stable per-posting link
    # (applitrack_posting_url()), so postings sharing a title stay apart.
    job_id <- sub("^p(\\d+)_?$", "\\1", rvest::html_attr(p, "id"))
    if (is.na(job_id) || !grepl("^\\d+$", job_id)) {
      job_id <- stringr::str_match(rvest::html_text2(rvest::html_element(p, "table.title")), "JobID:\\s*(\\d+)")[, 2]
    }

    data.frame(title = title, position = field$position, position2 = field$position2,
               date_posted = field$date_posted, location = field$location,
               closing_date = field$closing_date, job_id = job_id, pay, stringsAsFactors = FALSE)
  })

  do.call(rbind, rows)
}
