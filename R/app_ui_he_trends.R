# The Higher Ed Trends tab's UI (step 7 slice 2b, 2026-11-21). Both states' app.R had the same nav_panel: Current
# Trends (institution, appointment and view filters; the sparkline table) and Multi-Year Trends (institution,
# category, jobs and view filters; the timeline slider and plot). Only the slider's opening window differed (MT
# bounds it to the last WINDOW_WEEKS, WY shows the last 365 days), so the state passes it. Server side is in
# dashboard_server_shared().

#' Higher Ed Trends tab
#'
#' The `nav_panel()` with value `"he_trends"` and its `he_trends_tabs` card (`he_trends_current`,
#' `he_trends_historical`).
#' @param henowsum This week's Higher Ed summary (its `Institution` column fills the Current Trends choices).
#' @param hesum The weekly Higher Ed summary over the archive (`Institution`, `Category`, `Archive_Date`).
#' @param timeline_value The timeline slider's opening range: two dates within `range(hesum$Archive_Date)`.
#' @return A bslib `nav_panel()`.
#' @export
he_trends_panel <- function(henowsum, hesum,
                            timeline_value = c(max(hesum$Archive_Date) - 365, max(hesum$Archive_Date))) {
  need_shiny_ui("he_trends_panel")
  bslib::nav_panel(
    "Trends", value = "he_trends", icon = shiny::icon("chart-line"),
    bslib::navset_card_tab(
      id = "he_trends_tabs",
      bslib::nav_panel(
        title = htmltools::tagList(shiny::icon("chart-column"), " Current Trends"),
        value = "he_trends_current",
        htmltools::p(class = "text-muted", style = "margin-top: 5px; margin-bottom: 12px;",
          "Current institution and discipline breakdown across active higher education postings with 12-week sparklines. ",
          help_popover("Current higher ed trends",
            htmltools::p("Faculty postings in the latest update by discipline, with each discipline's postings in each of the last 12 weeks. The discipline is read from the job title. Staff postings are not counted here; the Appointment buttons split full-time from adjunct/part-time."),
            source = "each college's own job board, read twice a week; disciplines by this dashboard's title classifier.")),
        bslib::layout_sidebar(
          fillable = FALSE,
          sidebar = bslib::sidebar(
            width = 260,
            shiny::selectInput("inst_current", "Institution:",
                        choices = sort(unique(henowsum$Institution)), selected = "Total"),
            shiny::radioButtons("he_current_appointment", "Appointment:",
                         choices = c(
                           "All faculty postings" = "all",
                           "Faculty/instructor (non-adjunct)" = "Instructor/Teacher/Faculty",
                           "Adjunct/part-time faculty" = "Adjunct/Part-Time Faculty"
                         ),
                         selected = "all"),
            shiny::radioButtons("he_detail_level_current", "View:",
                         choices = c("Individual fields" = "detail", "Major academic areas" = "agg"),
                         selected = "detail")
          ),
          htmltools::div(class = "trends-table-container mt-0",
              shinycssloaders::withSpinner(shiny::tableOutput("he_current_trends_table")))
        )
      ),
      bslib::nav_panel(
        title = htmltools::tagList(shiny::icon("chart-line"), " Multi-Year Trends"),
        value = "he_trends_historical",
        htmltools::p(class = "text-muted", style = "margin-top: 5px; margin-bottom: 12px;",
          "Multi-year vacancy volume time series tracking higher education hiring trends over time. ",
          help_popover("Multi-year higher ed trends",
            htmltools::p("Faculty postings counted at each update (twice a week; weekly before 2026-11-17), by discipline (read from the job title). A count is how many postings were up that week, not how many were new."),
            source = "each college's own job board, read twice a week and kept in the Higher Ed Archive (Data tab).")),
        bslib::layout_sidebar(
          fillable = FALSE,
          sidebar = bslib::sidebar(
            width = 260,
            shiny::selectInput(
              "inst_trend",
              "Institution:",
              choices = sort(unique(hesum$Institution)),
              selected = "Total"
            ),
            shinyWidgets::pickerInput(
              "he_category",
              "Category:",
              choices  = sort(unique(hesum$Category)),
              selected = sort(unique(hesum$Category)),
              multiple = TRUE,
              width = "100%",
              options = shinyWidgets::pickerOptions(actionsBox = TRUE, liveSearch = TRUE, selectedTextFormat = "count > 3")
            ),
            shiny::radioButtons("he_chart_type", "Jobs:", choices = c(
              "All Jobs" = "all",
              "Faculty/instructor (non-adjunct)" = "Instructor/Teacher/Faculty",
              "Adjunct/part-time faculty" = "Adjunct/Part-Time Faculty"
            ), selected = "all"),
            shiny::radioButtons("he_detail_level_trends", "View:",
                         choices = c("Individual fields" = "detail", "Major academic areas" = "agg"),
                         selected = "detail")
          ),
          htmltools::div(style = "color:#777; font-size:0.9em;", shiny::textOutput("he_slider_label")),
          shiny::sliderInput(
            "he_scroll",
            "Scroll timeline:",
            min = min(hesum$Archive_Date),
            max = max(hesum$Archive_Date),
            value = timeline_value,
            timeFormat = "%Y-%m-%d",
            width = "100%"
          ),
          shinycssloaders::withSpinner(plotly::plotlyOutput("he_longitudinal_plot"))
        )
      )
    )
  )
}
