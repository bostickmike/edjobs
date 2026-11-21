# The Higher Ed Colleges tab's UI (step 7 slice 2b, 2026-11-21). Both states' app.R had the same nav_panel: the
# Institution Directory (table and footnote) and the Head-to-Head Comparator (institution picker, presets, cards and
# plots). Only two lines of the directory's popover (what the Salaries and County columns hold; WY shows three salary
# years and the professor-rank average, MT the county's mining/energy share and population trend) and the default
# pair to compare differ. Server side is in dashboard_server_shared() and each state's own he_summary_table.

#' Higher Ed Colleges tab
#'
#' The `nav_panel()` with value `"he_summary"` and its `he_summary_tabs` card (`he_summary_dir`, `he_summary_comp`).
#' @param institutions The comparator's choices (the state passes `sort(unique(mapdata2_he$Name))`).
#' @param compare_selected The institutions compared when the tab opens (up to four).
#' @param salaries_note,county_note What the directory's Salaries and County columns hold (text after the bold label).
#' @export
he_colleges_panel <- function(institutions, compare_selected, salaries_note, county_note) {
  need_shiny_ui("he_colleges_panel")
  bslib::nav_panel(
    "Colleges", value = "he_summary", icon = shiny::icon("building-columns"),
    bslib::navset_card_tab(
      id = "he_summary_tabs",
      bslib::nav_panel(
        title = htmltools::tagList(shiny::icon("building-columns"), " Institution Directory"),
        value = "he_summary_dir",
        htmltools::h4("One row per institution -- current openings, faculty vacancy rate, and salary data ",
           help_popover("Institution directory",
             htmltools::p(htmltools::tags$strong("Openings: "), "postings in this week's scrape, split into full-time and adjunct/part-time faculty by the job title, and those new since last week."),
             htmltools::p(htmltools::tags$strong("Faculty vacancy rate, faculty count and students per faculty: "), "full-time faculty postings, and enrollment, divided by full-time instructional staff (IPEDS)."),
             htmltools::p(htmltools::tags$strong("Salaries: "), salaries_note),
             htmltools::p(htmltools::tags$strong("Students: "), "5-year enrollment trend (IPEDS) and the share of students receiving Pell Grants (Federal Student Aid)."),
             htmltools::p(htmltools::tags$strong("County: "), county_note),
             source = "IPEDS and Federal Student Aid data via the Urban Institute's Education Data Portal; Census ACS; the colleges' job boards.")),
        DT::DTOutput("he_summary_table", fill = FALSE),
        shiny::uiOutput("he_summary_footnote")
      ),
      bslib::nav_panel(
        title = htmltools::tagList(shiny::icon("code-compare"), " Head-to-Head Comparator"),
        value = "he_summary_comp",
        htmltools::p(class = "text-muted small", style = "margin-top: 6px;",
          "Select up to four higher education institutions to evaluate faculty demand, IPEDS salary benchmarks, and institutional enrollment side-by-side."),
        bslib::layout_columns(
          col_widths = c(8, 4),
          shiny::selectizeInput(
            "he_compare_insts",
            label = "Select Institutions (up to 4):",
            choices = institutions,
            selected = compare_selected,
            multiple = TRUE,
            width = "100%",
            options = list(maxItems = 4, placeholder = "Type or select up to 4 institutions...")
          ),
          htmltools::div(
            htmltools::tags$label("Quick Presets:", style = "display: block; margin-bottom: 5px;"),
            htmltools::div(
              class = "btn-group",
              shiny::actionButton("he_comp_preset_top", "Top Hiring", class = "btn-sm btn-outline-secondary", icon = shiny::icon("fire")),
              shiny::actionButton("he_comp_preset_clear", "Clear", class = "btn-sm btn-outline-secondary", icon = shiny::icon("xmark"))
            )
          )
        ),
        shiny::uiOutput("he_compare_cards"),
        bslib::layout_columns(
          col_widths = c(6, 6),
          dash_card("Avg. Salary, Full-Time Instructional Staff (IPEDS)", info = help_popover("Faculty average salary",
                      htmltools::p("The average salary of full-time instructional staff on 9- to 12-month contracts, all academic ranks, for the latest IPEDS year and the year before. IPEDS collects no salaries for part-time staff, or for full-time staff on contracts under 9 months."),
                      source = "IPEDS, via the Urban Institute's Education Data Portal."),
                    shinycssloaders::withSpinner(plotly::plotlyOutput("he_compare_salary_plot", height = "360px"))),
          dash_card("Openings by Appointment Type", info = help_popover("Openings by appointment type",
                      htmltools::p("This week's postings, sorted by the job title: adjunct or part-time faculty (titles with \"adjunct\" or \"part-time\"), full-time faculty or instructor (titles such as instructor, professor, lecturer, librarian), or other (staff)."),
                      source = "each college's own job board."),
                    shinycssloaders::withSpinner(plotly::plotlyOutput("he_compare_openings_plot", height = "360px")))
        ),
        dash_card("Active Openings in Selected Institutions", info = help_popover("Openings in the selected institutions",
                    htmltools::p("Every current posting from the colleges chosen above, with the same columns as the Higher Ed Jobs tab.")),
                  DT::DTOutput("he_compare_jobs_table", fill = FALSE))
      )
    )
  )
}
