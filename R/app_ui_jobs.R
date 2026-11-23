# The Jobs tabs' UI (step 7 slice 2b, 2026-11-21). K-12 Jobs and Higher Ed Jobs had the same nav_panel in both
# states' app.R: the quick filter, the filter status, the pay line and its popover, and the table. Higher Ed adds the
# Appointment filter; each state's pay note differs. Their server side is in dashboard_server_shared().

#' Jobs tab (K-12 or Higher Ed)
#'
#' The `nav_panel()` for a Jobs table: value `"k12_table"` or `"he_table"`, inputs `<level>_quick_filter` (and
#' `he_table_appointment` for Higher Ed), outputs `<level>_filter_status` and `<level>_jobs`.
#' @param level `"k12"` or `"he"`.
#' @param pay_note The "About the pay columns" popover's text: strings or tags, pasted into one paragraph (each
#'   state passes its `POSTED_LISTED_NOTE` first).
#' @return A bslib `nav_panel()`.
#' @export
jobs_tab_panel <- function(level = c("k12", "he"), pay_note) {
  need_shiny_ui("jobs_tab_panel")
  level <- match.arg(level)
  id <- function(x) paste0(level, "_", x)
  appointment <- if (level == "he") shiny::radioButtons(
    "he_table_appointment", "Appointment:",
    choices = c(
      "All roles" = "all",
      "Faculty/instructor (non-adjunct)" = "Faculty/instructor (non-adjunct)",
      "Adjunct/part-time faculty" = "Adjunct/part-time faculty",
      "Other / not faculty" = "Other / not faculty"
    ),
    selected = "all", inline = TRUE
  )
  bslib::nav_panel(
    "Jobs", value = id("table"), icon = shiny::icon("table"),
    htmltools::div(
      class = "table-toolbar",
      htmltools::div(
        style = "display: flex; align-items: center; gap: 8px; flex-wrap: wrap;",
        htmltools::span(style = "font-weight: 600; font-size: 13.5px; color: #334155;", "Filter Postings:"),
        shinyWidgets::radioGroupButtons(
          inputId = id("quick_filter"),
          label = NULL,
          choices = c(
            "All Active Postings" = "all",
            "\U0001F7E2 New This Week" = "new"
          ),
          selected = "all",
          status = "primary",
          size = "sm",
          checkIcon = list(yes = shiny::icon("check"))
        ),
        appointment
      ),
      shiny::uiOutput(id("filter_status"))
    ),
    htmltools::p(class = "text-muted small mb-2",
      "Posted Pay is what the posting itself states; the other pay columns are labeled estimates, not this job's offer. ",
      info_popover("About the pay columns", do.call(htmltools::p, c(list(class = "mb-0"), as.list(pay_note))))),
    DT::DTOutput(id("jobs"), fill = FALSE)
  )
}
