# The K-12 Trends tab's UI (step 7 slice 2b, 2026-11-21). Both states' app.R had the same nav_panel: Current Trends
# (district and view filters; the sparkline table) and Multi-Year Trends (district, category and view filters; the
# timeline slider and plot). Montana alone shows a label above the slider and, under the plot, how postings on both
# a district board and OPI's statewide board are counted; the slider's opening window also differs (MT bounds it to
# the last WINDOW_WEEKS, WY shows the last 365 days). Server side is in dashboard_server_shared() and the state.

#' K-12 Trends tab
#'
#' The `nav_panel()` with value `"k12_trends"` and its `k12_trends_tabs` card (`k12_trends_current`,
#' `k12_trends_historical`).
#' @param k12nowsum This week's K-12 summary (its `District` column fills the Current Trends choices).
#' @param k12sum The weekly K-12 summary over the archive (`District`, `Broad_Category`, `Archive_Date`).
#' @param timeline_value The timeline slider's opening range: two dates within `range(k12sum$Archive_Date)`.
#' @param slider_label `TRUE` to show the `k12_slider_label` text output above the slider (the state's server must
#'   then define it).
#' @param counting_note `NULL`, or tags/strings for a note under the plot on how postings are counted.
#' @return A bslib `nav_panel()`.
#' @export
k12_trends_panel <- function(k12nowsum, k12sum,
                             timeline_value = c(max(k12sum$Archive_Date) - 365, max(k12sum$Archive_Date)),
                             slider_label = FALSE, counting_note = NULL) {
  need_shiny_ui("k12_trends_panel")
  bslib::nav_panel(
    "Trends", value = "k12_trends", icon = shiny::icon("chart-line"),
    bslib::navset_card_tab(
      id = "k12_trends_tabs",
      bslib::nav_panel(
        title = htmltools::tagList(shiny::icon("chart-column"), " Current Trends"),
        value = "k12_trends_current",
        htmltools::p(class = "text-muted", style = "margin-top: 5px; margin-bottom: 12px;",
          "Current subject distribution across active K-12 postings with 12-week vacancy sparklines. ",
          help_popover("Current K-12 trends",
            htmltools::p("Teacher postings in the latest update by subject, with each subject's postings in each of the last 12 weeks. The subject is read from the job title (\"Major subject groups\" combines subjects, e.g. all STEM). Non-teaching jobs are not counted here."),
            source = "the districts' job boards, read twice a week; subjects by this dashboard's title classifier.")),
        bslib::layout_sidebar(
          fillable = FALSE,
          sidebar = bslib::sidebar(
            width = 260,
            shiny::selectInput(
              "district_current",
              "District:",
              choices = sort(unique(k12nowsum$District)),
              selected = "Total"
            ),
            shiny::radioButtons("k12_detail_level_current", "View:",
                         choices = c("Individual subjects" = "detail",
                                     "Major subject groups" = "agg"),
                         selected = "detail")
          ),
          htmltools::div(class = "trends-table-container mt-0",
              shinycssloaders::withSpinner(shiny::tableOutput("k12_current_trends_table")))
        )
      ),
      bslib::nav_panel(
        title = htmltools::tagList(shiny::icon("chart-line"), " Multi-Year Trends"),
        value = "k12_trends_historical",
        htmltools::p(class = "text-muted", style = "margin-top: 5px; margin-bottom: 12px;",
          "Multi-year vacancy volume time series tracking hiring trends over time. ",
          help_popover("Multi-year K-12 trends",
            htmltools::p("Teacher postings counted at each update (twice a week; weekly before 2026-11-17), by subject (read from the job title). A count is how many postings were up that week, not how many were new."),
            source = "the districts' job boards, read twice a week and kept in the K-12 History Archive (Data tab).")),
        bslib::layout_sidebar(
          fillable = FALSE,
          sidebar = bslib::sidebar(
            width = 260,
            shiny::selectInput(
              "district_trend",
              "District:",
              choices = sort(unique(k12sum$District)),
              selected = "Total"
            ),
            shinyWidgets::pickerInput(
              "broad_category",
              "Teacher category:",
              choices  = sort(unique(k12sum$Broad_Category)),
              selected = sort(unique(k12sum$Broad_Category)),
              multiple = TRUE,
              width = "100%",
              options = shinyWidgets::pickerOptions(actionsBox = TRUE, liveSearch = TRUE, selectedTextFormat = "count > 3")
            ),
            shiny::radioButtons("k12_detail_level_trends", "View:",
                         choices = c("Individual subjects" = "detail",
                                     "Major subject groups" = "agg"),
                         selected = "detail")
          ),
          if (slider_label) htmltools::div(style = "color:#777; font-size:0.9em;", shiny::textOutput("k12_slider_label")),
          shiny::sliderInput(
            "k12_scroll",
            "Scroll timeline:",
            min = min(k12sum$Archive_Date),
            max = max(k12sum$Archive_Date),
            value = timeline_value,
            timeFormat = "%Y-%m-%d",
            width = "100%"
          ),
          shinycssloaders::withSpinner(plotly::plotlyOutput("k12_longitudinal_plot")),
          if (!is.null(counting_note)) htmltools::div(class = "alert alert-info small mt-3 mb-0", role = "note", counting_note)
        )
      )
    )
  )
}
