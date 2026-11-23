# The Home tab's UI (step 7 slice 2b, 2026-11-21). Both states' app.R had the same nav_panel: the KPI cards, the
# switch that leaves out substitutes and other not-full-time postings, and the four "top this week" tables. The
# state passes its name, where its K-12 postings come from, the refreshed date and the credit line, and any card of
# its own (Montana's K-12 coverage card). The switch label and the two notes on what it leaves out were the same
# constants in both states and nothing else used them, so they live here. Server side is in dashboard_server_shared().

# What the Home page's job-type and top-employer tables leave out while its switch is on.
home_switch_label <- function() "Leave out substitutes, coaching, part-time, student and temporary jobs"
not_marked_note <- function(level = c("k12", "he")) switch(match.arg(level),
  k12 = paste("With the switch above the tables on (the default), substitutes, coaching and other extra-duty",
              "stipends, and postings whose title says part-time, student or temporary are left out."),
  he = paste("With the switch above the tables on (the default), adjunct and part-time faculty, student jobs,",
             "substitutes, and postings whose title says part-time or temporary are left out."))

#' Home tab
#'
#' The `nav_panel()` with value `"intro"`.
#' @param state_name The state's name, for the page title ("Education Jobs in <state_name>").
#' @param k12_source Where the K-12 postings come from (the K-12 tables' popover source line).
#' @param last_refreshed_date The data's refreshed date, shown in the footer.
#' @param credit The footer's credit after the date (e.g. "Created by ...").
#' @param extra `NULL`, or a card of the state's own shown under the four tables.
#' @export
home_tab_panel <- function(state_name, k12_source, last_refreshed_date, credit, extra = NULL) {
  need_shiny_ui("home_tab_panel")
  bslib::nav_panel(
    "Home", value = "intro", icon = shiny::icon("house"),
    htmltools::h1(class = "page-title", paste("Education Jobs in", state_name)),
    shiny::uiOutput("data_load_issues_banner"),
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      shiny::uiOutput("kpi_k12_total", fill = TRUE),
      shiny::uiOutput("kpi_he_total", fill = TRUE),
      shiny::uiOutput("kpi_last_refreshed", fill = TRUE)
    ),
    # Drives the four tables below (counts, Share, sparklines) and the Jobs table their links open; KPI cards keep
    # both numbers.
    htmltools::div(class = "home-basis-switch", style = "margin: 4px 0 -4px;",
        bslib::input_switch("home_not_marked", home_switch_label(), value = TRUE, width = "100%")),
    bslib::layout_columns(
      col_widths = c(6, 6),
      dash_card("Top K-12 Hiring Districts This Week",
                htmltools::div(style = "overflow-x: auto;", shiny::tableOutput("top_k12_districts")),
                info = help_popover("Top K-12 hiring districts",
                  htmltools::p("The five districts with the most postings in the latest update, all job types (teachers, aides, drivers and so on). ", not_marked_note("k12"),
                    " Last 12 Weeks: the district's postings in each of the last 12 weeks (the latest count each week), counted the same way. Click a district to see its jobs."),
                  source = k12_source)),
      dash_card("Top Higher Ed Hiring Institutions This Week",
                htmltools::div(style = "overflow-x: auto;", shiny::tableOutput("top_he_institutions")),
                info = help_popover("Top higher ed hiring institutions",
                  htmltools::p("The five colleges with the most postings in the latest update, faculty and staff. ", not_marked_note("he"),
                    " Last 12 Weeks: the college's postings in each of the last 12 weeks (the latest count each week), counted the same way. Click a college to see its jobs."),
                  source = "each college's own job board, read twice a week."))
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      dash_card("Top K-12 Job Types This Week",
                htmltools::div(style = "overflow-x: auto;", shiny::tableOutput("top_k12_job_types")),
                info = help_popover("Top K-12 job types",
                  htmltools::p("The five most-posted K-12 job types in the latest update, with each type's share of those postings. ", not_marked_note("k12"),
                    " The sources never say a job is full-time, so this counts postings not marked otherwise. Most-posted is not the same as most in demand: a count of open postings also reflects turnover and how often a district posts."),
                  htmltools::p("Last 12 Weeks: that type's postings in each of the last 12 weeks (the latest count each week), counted the same way. Click a type to see its jobs."),
                  source = k12_source)),
      dash_card("Top Higher Ed Job Types This Week",
                htmltools::div(style = "overflow-x: auto;", shiny::tableOutput("top_he_job_types")),
                info = help_popover("Top higher ed job types",
                  htmltools::p("The five most-posted college job types in the latest update, faculty and staff, with each type's share of those postings. ", not_marked_note("he"),
                    " Most-posted is not the same as most in demand."),
                  htmltools::p("Last 12 Weeks: that type's postings in each of the last 12 weeks (the latest count each week), counted the same way. Click a type to see its jobs."),
                  source = "each college's own job board, read twice a week."))
    ),
    extra,
    htmltools::div(style = "text-align:center; color:#999; font-size:0.85em; padding:15px;",
        paste0("Refreshed on: ", last_refreshed_date, " \u00b7 ", credit))
  )

}
