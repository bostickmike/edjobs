# Dashboard UI helpers both states' app.R defined identically (step 7 slice 1, 2026-11-21; the old phase 6 slice 3).
# Code as it was in app.R, with calls written pkg::fn(). shiny and bslib are Suggests (Mike, 11-17 "B"): the apps
# always load them, and jobs that install edjobs without drawing a page don't need them.

need_shiny_ui <- function(fn) {
  for (p in c("shiny", "bslib"))
    if (!requireNamespace(p, quietly = TRUE)) stop(fn, "() needs the ", p, " package.", call. = FALSE)
  invisible(TRUE)
}

#' Listing Duration tabs' footnote
#'
#' What "days listed" means and why it isn't time to fill (Mike, 11-14: one footnote, no "paused" banner).
#' @export
spells_footnote <- function() {
  htmltools::p(class = "text-muted small mt-3 mb-0",
    "Days listed run from the first to the last update that showed a posting, plus 7 days. This is how long a ",
    "posting was listed, not time to fill: a listing ends when it stops appearing, whether it was filled, cancelled, ",
    "moved to another board or missed in one update, and a posting already listed when its board was first checked ",
    "starts late. \u201cNo longer listed\u201d means only that it no longer appears.")
}

#' KPI value box
#'
#' bslib value box with shinydashboard::valueBox()'s argument order. Colors map onto Bootstrap theme names.
#' @param value,subtitle Box value and title.
#' @param icon Optional showcase icon.
#' @param color "blue", "green", "red" or "purple".
#' @param ... Passed to [bslib::value_box()].
#' @export
kpi_value_box <- function(value, subtitle, icon = NULL, color = "blue", ...) {
  need_shiny_ui("kpi_value_box")
  theme <- switch(color, blue = "primary", green = "success", red = "danger",
                  purple = "purple", "primary")
  bslib::value_box(title = subtitle, value = value, showcase = icon, theme = theme, ...)
}

#' Titled card
#'
#' The bslib stand-in for shinydashboard::box().
#' @param title Card header text, or NULL for none.
#' @param ... Card body content.
#' @param full_screen,class Passed to [bslib::card()].
#' @param info Optional popover shown at the header's right.
#' @export
dash_card <- function(title = NULL, ..., full_screen = FALSE, class = NULL, info = NULL) {
  need_shiny_ui("dash_card")
  bslib::card(
    full_screen = full_screen,
    class = class,
    if (!is.null(title)) bslib::card_header(class = "d-flex justify-content-between align-items-center gap-2",
                                     title, if (!is.null(info)) info),
    bslib::card_body(..., fillable = FALSE)
  )
}

#' Help popover
#'
#' A (?) beside a chart, table or download that says what it shows and where the numbers come from (Mike,
#' 2026-11-16).
#' @param title Popover title.
#' @param ... Popover body.
#' @param source Plain-language source line, or NULL.
#' @export
help_popover <- function(title, ..., source = NULL) {
  need_shiny_ui("help_popover")
  bslib::popover(
    htmltools::tags$a(href = "#", class = "help-pop", role = "button", `aria-label` = paste0("About: ", title),
           onclick = "return false;", shiny::icon("circle-question")),
    ...,
    if (!is.null(source)) htmltools::p(class = "mb-0", htmltools::tags$strong("Source: "), source),
    title = title,
    placement = "auto",
    options = list(customClass = "info-popover help-popover")
  )
}

#' Info popover
#'
#' An "(i) label" link that opens explanatory text in a popover.
#' @param label Link text and popover title.
#' @param ... Popover body.
#' @param placement Popover placement.
#' @export
info_popover <- function(label, ..., placement = "auto") {
  need_shiny_ui("info_popover")
  bslib::popover(
    htmltools::tags$a(href = "#", class = "info-pop", role = "button", onclick = "return false;",
           shiny::icon("circle-info"), " ", label),
    ...,
    title = label,
    placement = placement,
    options = list(customClass = "info-popover")
  )
}

#' Download tile on the Data page
#'
#' @param icon_name Font Awesome icon name.
#' @param heading,text Tile heading and text.
#' @param output_id,label The download button's output id and label.
#' @param info Optional popover beside the heading.
#' @export
download_tile <- function(icon_name, heading, text, output_id, label, info = NULL) {
  need_shiny_ui("download_tile")
  htmltools::div(
    class = "download-tile",
    htmltools::h4(class = "d-flex justify-content-between align-items-start gap-2",
       htmltools::tags$span(shiny::icon(icon_name), " ", heading), if (!is.null(info)) info),
    htmltools::p(text),
    shiny::downloadButton(output_id, label, class = "btn-outline-primary btn-sm w-100")
  )
}
