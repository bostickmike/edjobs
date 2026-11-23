# The Data tab's UI (step 7 slice 2b, 2026-11-21; Mike's "yes" to: the package builds each tab, the state passes its
# text). Both states' app.R had this nav_panel with the same structure and per-state prose: which files there are and
# what is in them, the provenance table, the citation. The package keeps the structure and the text both states
# shared (the defaults below); each app.R passes the rest. The downloads' server side is in dashboard_server_shared().

# Tile fields both states had the same, by download output id. `text` is here only where it was the same in both.
data_tile_defaults <- function() list(
  download_active_k12 = list(icon = "briefcase", heading = "Active K-12 Jobs", label = "Active Jobs (CSV)",
                             dictionary_file = "combinedclean.csv"),
  download_history_k12 = list(icon = "clock-rotate-left", heading = "K-12 History Archive", label = "Historical K-12 (CSV)",
                              sources = "the districts' job boards, one row per teacher posting per update (twice a week); subjects by this dashboard's title classifier.",
                              dictionary_file = "k12jobanalysis.csv"),
  download_spells = list(icon = "stopwatch", heading = "K-12 Listing Duration", label = "Listing Duration (CSV)",
                         sources = "built from the twice-weekly updates: the first and last date each posting was seen.",
                         dictionary_file = "job_duration_spells.csv"),
  download_district_benchmarks = list(icon = "school", label = "District Profiles (CSV)", dictionary_file = "salarymap2.csv"),
  download_weekly_totals = list(icon = "chart-line", heading = "K-12 Weekly Vacancies",
                                sources = "the twice-weekly updates: each district's postings at each update.",
                                dictionary_file = "k12_district_weekly_totals.csv"),
  download_history_he = list(icon = "building-columns", heading = "Higher Ed Archive", label = "Higher Ed Archive (CSV)",
                             sources = "each college's own job board, one row per posting per update (twice a week); categories by this dashboard's title classifier.",
                             dictionary_file = "facultydata.csv"),
  download_he_spells = list(icon = "stopwatch", heading = "Higher Ed Listing Duration", label = "Higher Ed Listing Duration (CSV)",
                            text = "Each faculty/staff posting's first and last week on the boards this dashboard tracks, days listed, and discipline flags. Not a time-to-fill measure: see the note on the Listing Duration tab.",
                            sources = "built from the twice-weekly updates: the first and last date each posting was seen.",
                            dictionary_file = "he_job_duration_spells.csv"),
  download_he_benchmarks = list(icon = "graduation-cap", heading = "Higher Ed Profiles", label = "Higher Ed Profiles (CSV)",
                                text = "Institution-level profiles: IPEDS UNITID, instructional faculty salaries (all ranks and professor), enrollment, and federal Pell shares.",
                                sources = "IPEDS and Federal Student Aid data (via the Urban Institute's Education Data Portal); Census American Community Survey 5-year estimates.",
                                dictionary_file = "salarymap.csv"),
  download_he_weekly_totals = list(icon = "chart-line", heading = "Higher Ed Weekly Vacancies", label = "Higher Ed Weekly Totals (CSV)",
                                   sources = "the twice-weekly updates: each college's postings at each update.",
                                   dictionary_file = "he_institution_weekly_totals.csv"),
  download_dictionary = list(icon = "book", heading = "Data Dictionary", label = "Codebook (CSV)",
                             text = "Comprehensive machine-readable codebook documenting variable names, data types, definitions, and source authorities across all datasets.")
)

#' A download tile on the Data tab
#'
#' Fields left NULL take the shared default for `output_id` (the ones both states had the same); a download the
#' package has no default for needs `icon`, `heading`, `text` and `label`.
#' @param output_id The download button's output id (e.g. `"download_active_k12"`).
#' @param text Tile text.
#' @param icon,heading,label Font Awesome icon name, tile heading, button label.
#' @param sources Text after "Sources: " in the tile's help popover; NULL for no popover.
#' @param dictionary_file The file's `File_Name` in the data dictionary, for the popover's column list.
#' @return A list for [data_tab_panel()]'s `tiles`.
#' @export
data_tile <- function(output_id, text = NULL, icon = NULL, heading = NULL, label = NULL, sources = NULL,
                      dictionary_file = NULL) {
  given <- Filter(Negate(is.null), list(output_id = output_id, text = text, icon = icon, heading = heading,
                                        label = label, sources = sources, dictionary_file = dictionary_file))
  x <- utils::modifyList(data_tile_defaults()[[output_id]] %||% list(), given)
  miss <- setdiff(c("icon", "heading", "text", "label"), names(x))
  if (length(miss)) stop("data_tile(\"", output_id, "\") needs ", paste(miss, collapse = ", "), call. = FALSE)
  x
}

# Provenance rows both states had the same.
data_provenance_defaults <- function() list(
  county = list(layer = "County Economic Context", source = "US Census Bureau (ACS 5-Year Estimates)", cadence = "Annual",
                description = "County median household income, median gross rent, share of employed residents 16 and over working in mining, quarrying, and oil and gas extraction (ACS S2403), and 5-year population trajectory."),
  student_need = list(layer = "Student Need Context", source = "Census SAIPE & US Dept of Education FSA", cadence = "Annual",
                      description = "Small Area Income and Poverty Estimates (SAIPE) district-level estimates of school-age children in poverty for K-12, and Federal Student Aid (FSA) undergraduate Pell Grant recipient shares for colleges."),
  ccd_staffing = list(source = "NCES Common Core of Data (CCD) via Urban Institute", cadence = "Annual",
                      description = "Federal National Center for Education Statistics district-level teacher FTE and student enrollment counts. Powers vacancy-rate calculations and student-to-teacher ratios."),
  ipeds_pay = list(layer = "Higher Ed Faculty Compensation", source = "NCES IPEDS Instructional Staff Salary Survey", cadence = "Annual")
)

#' A row of the Data tab's provenance table
#'
#' @param key A shared row (`"county"`, `"student_need"`, `"ccd_staffing"`, `"ipeds_pay"`) whose fields fill in
#'   those left NULL, or NULL.
#' @param layer,source,cadence,description The four cells.
#' @param badge Bootstrap color of the cadence badge (`"info"`, `"success"`, ...).
#' @export
data_provenance_row <- function(key = NULL, layer = NULL, source = NULL, cadence = NULL, description = NULL,
                                badge = "info") {
  given <- Filter(Negate(is.null), list(layer = layer, source = source, cadence = cadence, description = description))
  x <- utils::modifyList(if (is.null(key)) list() else data_provenance_defaults()[[key]], given)
  miss <- setdiff(c("layer", "source", "cadence", "description"), names(x))
  if (length(miss)) stop("data_provenance_row() needs ", paste(miss, collapse = ", "), call. = FALSE)
  x$badge <- badge
  x
}

#' Data tab
#'
#' The `nav_panel()` for the Data tab (value `"data_tab"`): download tiles, the searchable data dictionary
#' (`output$data_dictionary_table`), the provenance table and the citation.
#' @param tiles List of [data_tile()]s, in display order.
#' @param dictionary The app's data dictionary (for the tiles' column lists).
#' @param provenance_rows List of [data_provenance_row()]s.
#' @param citation Citation text before the repository link ("Author (year). Title. ").
#' @param repo_url The state repository's GitHub URL (citation and issue tracker links).
#' @param provenance_title,provenance_intro Provenance card title and the sentence above its table (NULL: Montana's).
#' @param provenance_headers The table's four column headers (NULL: Montana's).
#' @param provenance_table_class,provenance_table_style The table's class and style attributes (NULL: Montana's
#'   `"table table-striped table-bordered table-hover"` and none).
#' @export
data_tab_panel <- function(tiles, dictionary, provenance_rows, citation, repo_url,
                           provenance_title = NULL, provenance_intro = NULL, provenance_headers = NULL,
                           provenance_table_class = NULL, provenance_table_style = NULL) {
  need_shiny_ui("data_tab_panel")
  provenance_title <- provenance_title %||% "Data Provenance & Source Transparency"
  provenance_intro <- provenance_intro %||% "All datasets are compiled through automated, reproducible collection pipelines and federal/state data integration. Here is where every data point originates:"
  provenance_headers <- provenance_headers %||% c("Dataset Layer", "Primary Authority / Source", "Refresh Frequency", "Description & Research Utility")
  provenance_table_class <- provenance_table_class %||% "table table-striped table-bordered table-hover"
  tags <- htmltools::tags
  tile_ui <- function(x) download_tile(
    x$icon, x$heading, x$text, x$output_id, x$label,
    info = if (!is.null(x$sources)) help_popover("What is in this file", htmltools::p(tags$strong("Sources: "), x$sources),
                                                 if (!is.null(x$dictionary_file)) dictionary_columns_help(x$dictionary_file, dictionary)))
  row_ui <- function(x) tags$tr(
    tags$td(htmltools::strong(x$layer)),
    tags$td(x$source),
    tags$td(htmltools::span(class = paste0("badge text-bg-", x$badge), x$cadence)),
    tags$td(x$description)
  )
  bslib::nav_panel(
    "Data", value = "data_tab", icon = shiny::icon("database"),
    htmltools::h1(class = "page-title", "Data Sources & Open Research Downloads"),
    htmltools::p(class = "lead", "Transparent data provenance, methodology documentation, and downloadable datasets for researchers, policy analysts, school administrators, and the public."),

    dash_card(
      "Open Research Downloads",
      htmltools::p("Every update is kept. Download current or multi-year longitudinal datasets in standard CSV format:"),
      do.call(bslib::layout_column_wrap, c(list(width = "230px", fill = FALSE), unname(lapply(tiles, tile_ui))))
    ),

    dash_card(
      "Interactive Data Dictionary & Variable Codebook",
      htmltools::p("Search, sort, and filter variable definitions, data types, and primary source authorities across all datasets:"),
      htmltools::div(style = "overflow-x: auto;", shinycssloaders::withSpinner(DT::DTOutput("data_dictionary_table", fill = FALSE)))
    ),

    dash_card(
      provenance_title,
      htmltools::p(provenance_intro),
      htmltools::div(class = "table-responsive",
        tags$table(
          class = provenance_table_class, style = provenance_table_style,
          tags$thead(do.call(tags$tr, unname(lapply(provenance_headers, tags$th)))),
          do.call(tags$tbody, unname(lapply(provenance_rows, row_ui)))
        )
      )
    ),

    dash_card(
      "Open Research Citation & Code",
      htmltools::p("This project is open-source and maintained to promote labor market transparency in public education. If you use these datasets in academic research, policy analysis, or journalism, please cite:"),
      tags$blockquote(
        style = "font-size: 13px; background: #f9f9f9; padding: 10px 15px; border-left: 4px solid #2a78d6;",
        citation,
        tags$a(href = repo_url, target = "_blank", repo_url)
      ),
      htmltools::p(style = "margin-bottom: 0; font-size: 12px; color: #777;",
        "To report discrepancies, missing districts, or data issues, use the Feedback button at the top right (no account needed), or open an issue on the ",
        tags$a(href = paste0(repo_url, "/issues"), target = "_blank", "GitHub Issue Tracker"), ".")
    )
  )
}

#' Rows a download hands out
#'
#' For a Data tab tile's count, so it doesn't go stale (the hand-written ones had: on 2026-11-21 Montana's tiles said
#' 3,200+ K-12 history records and 3,700+ weekly observations for files of 8,401 and 2,891 rows, Wyoming's 76,000+
#' and 29,000+ for 13,513 and 18,532).
#' @param file A CSV in the app folder.
#' @param latest_week TRUE for the weekly totals, whose download is [latest_snapshot_per_week()] of the file.
#' @return The row count, or NA when the file can't be read.
#' @export
download_rows <- function(file, latest_week = FALSE) {
  if (!file.exists(file)) return(NA_integer_)
  x <- tryCatch(utils::read.csv(file, fileEncoding = "UTF-8"), error = function(e) NULL)
  if (is.null(x)) return(NA_integer_)
  if (latest_week) x <- latest_snapshot_per_week(x)
  nrow(x)
}

#' " (8,400+ records)" for a tile's text
#'
#' Rounded down to the hundred (thousand from 10,000), as the hand-written counts were.
#' @param n A count (NA gives "").
#' @param unit The word after the number.
#' @export
rows_phrase <- function(n, unit = "records") {
  if (length(n) != 1 || is.na(n)) return("")
  step <- if (n >= 10000) 1000 else if (n >= 1000) 100 else 1
  paste0(" (", format(floor(n / step) * step, big.mark = ",", scientific = FALSE), if (step > 1) "+", " ", unit, ")")
}
