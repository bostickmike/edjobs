# The K-12 Districts tab's UI (step 7 slice 2b, 2026-11-21). Both states' app.R had the same nav_panel: the District
# Directory (table and footnote) and the Head-to-Head Comparator (district picker, presets, cards, the salary,
# enrollment and openings charts, and the jobs table). What differs is each state's text: where its postings come
# from, its pay and county columns (MT: DLI average and 10th-90th percentile, General Fund; WY: WSBA starting base,
# superintendent, WDE602 average), and its salary chart's card. The columns themselves are built by each state's
# server (step 7 slice 3 is the district columns spec).

#' K-12 Districts tab
#'
#' The `nav_panel()` with value `"k12_summary"` and its `k12_summary_tabs` card (`k12_summary_dir`,
#' `k12_summary_comp`).
#' @param districts The comparator's choices (the state passes `sort(unique(mapdata2_k12$Name))`).
#' @param compare_selected The districts compared when the tab opens (up to four).
#' @param openings_note What the directory's Openings and Posting Data columns hold (text after the bold label).
#' @param directory_notes The directory popover's paragraphs on the state's pay, finance and county columns
#'   (after the shared one on vacancy rates).
#' @param directory_source The directory popover's source line.
#' @param compare_blurb The line above the comparator's picker.
#' @param salary_card The comparator's salary chart card: a `dash_card()` holding `plotlyOutput("k12_compare_salary_plot")`.
#' @param openings_source The Active Openings chart's source line.
#' @return A bslib `nav_panel()`.
#' @export
k12_districts_panel <- function(districts, compare_selected, openings_note, directory_notes, directory_source,
                                compare_blurb, salary_card, openings_source) {
  need_shiny_ui("k12_districts_panel")
  bslib::nav_panel(
    "Districts", value = "k12_summary", icon = shiny::icon("school"),
    bslib::navset_card_tab(
      id = "k12_summary_tabs",
      bslib::nav_panel(
        title = htmltools::tagList(shiny::icon("table-list"), " District Directory"),
        value = "k12_summary_dir",
        htmltools::h4("One row per district -- current openings, teacher vacancy rate, and salary data ",
           help_popover("District directory",
             htmltools::p(htmltools::tags$strong("Openings: "), openings_note),
             htmltools::p(htmltools::tags$strong("Teacher vacancy rate and students per teacher: "), "current teacher postings, and enrollment, divided by teacher FTE (NCES Common Core of Data)."),
             directory_notes,
             source = directory_source)),
        DT::DTOutput("k12_summary_table", fill = FALSE),
        shiny::uiOutput("k12_summary_footnote")
      ),
      bslib::nav_panel(
        title = htmltools::tagList(shiny::icon("code-compare"), " Head-to-Head Comparator"),
        value = "k12_summary_comp",
        htmltools::p(class = "text-muted small", style = "margin-top: 6px;",
          compare_blurb),
        bslib::layout_columns(
          col_widths = c(8, 4),
          shiny::selectizeInput(
            "k12_compare_districts",
            label = "Select School Districts (up to 4):",
            choices = districts,
            selected = compare_selected,
            multiple = TRUE,
            width = "100%",
            options = list(maxItems = 4, placeholder = "Type or select up to 4 districts...")
          ),
          htmltools::div(
            htmltools::tags$label("Quick Presets:", style = "display: block; margin-bottom: 5px;"),
            htmltools::div(
              class = "btn-group",
              shiny::actionButton("k12_comp_preset_top", "Top Hiring", class = "btn-sm btn-outline-secondary", icon = shiny::icon("fire")),
              shiny::actionButton("k12_comp_preset_largest", "Largest", class = "btn-sm btn-outline-secondary", icon = shiny::icon("users")),
              shiny::actionButton("k12_comp_preset_clear", "Clear", class = "btn-sm btn-outline-secondary", icon = shiny::icon("xmark"))
            )
          )
        ),
        shiny::uiOutput("k12_compare_cards"),
        bslib::layout_columns(
          col_widths = c(4, 4, 4),
          salary_card,
          dash_card("Student Enrollment",
                    shinycssloaders::withSpinner(plotly::plotlyOutput("k12_compare_enrollment_plot", height = "360px")),
                    info = help_popover("Student enrollment",
                      htmltools::p("Pre-K through grade 12 fall enrollment. Districts are in the same order in all three charts, largest enrollment first."),
                      source = "NCES Common Core of Data, via the Urban Institute's Education Data Portal.")),
          # Two bar charts, not one dual-axis chart (Mike 2026-11-16): a line
          # across districts implied a trend, and the two axes' scales made the
          # bars-vs-line comparison arbitrary. Same district order in both.
          dash_card("Active Openings",
                    shinycssloaders::withSpinner(plotly::plotlyOutput("k12_compare_openings_plot", height = "360px")),
                    info = help_popover("Active openings",
                      htmltools::p("Postings in the latest update, all job types (teachers, aides, coaches, drivers and so on)."),
                      source = openings_source))
        ),
        dash_card("Active Openings in Selected Districts",
                  DT::DTOutput("k12_compare_jobs_table", fill = FALSE),
                  info = help_popover("Openings in the selected districts",
                    htmltools::p("Every current posting from the districts chosen above, with the same columns as the K-12 Jobs tab: see \"About the pay columns\" there.")))
      )
    )
  )
}
