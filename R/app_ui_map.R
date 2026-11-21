# The Map tab's UI (step 7 slice 2b, 2026-11-21). Both states' app.R had this nav_panel with the same filters and
# map; the card title, the "About this map" paragraphs, the "Partial" badge note and the note under the map differ.
# Its server side (the filters, chips and the map's data) is in dashboard_server_shared(); output$combined_map stays
# in each app.R.

#' Map tab
#'
#' The `nav_panel()` for the Map tab (value `"map_tab"`).
#' @param state_name The state's name, for the card title ("Montana Education Job Openings Map").
#' @param about Paragraphs ([htmltools::p()]) at the top of the "About this map" popover, before the shared one on
#'   vacancy rates.
#' @param partial_note Text of the popover's last paragraph: what the red "Partial" badge means in this state.
#' @param map_note The note under the map, before "Click a marker to see its jobs."
#' @param county_choices The County filter's choices.
#' @param pay_bands The app's `PAY_BANDS`, for the Pay filter.
#' @param he_place_note The note on how colleges are placed ("" for none).
#' @export
map_tab_panel <- function(state_name, about, partial_note, map_note, county_choices, pay_bands, he_place_note = "") {
  need_shiny_ui("map_tab_panel")
  bslib::nav_panel(
    "Map", value = "map_tab", icon = shiny::icon("map-location-dot"),
    htmltools::h1(class = "page-title", "Where the Openings Are"),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        paste(state_name, "Education Job Openings Map"),
        do.call(info_popover, c(
          list("About this map"),
          about,
          list(htmltools::p("K-12 and Higher Ed vacancy rates use different staffing sources (CCD vs. IPEDS) and years -- the shared color scale is for a rough at-a-glance read, not a precise cross-type comparison."),
               htmltools::p(class = "mb-0", partial_note))
        ))
      ),
      bslib::layout_sidebar(
        fillable = FALSE,
        sidebar = bslib::sidebar(
          title = "Filters",
          width = 300,
          shiny::textInput(
            "map_search_keyword",
            label = htmltools::tagList(shiny::icon("magnifying-glass"), " Search by keyword or role:"),
            value = "",
            placeholder = "e.g., special ed, coach, bus driver..."
          ),
          htmltools::div(
            class = "map-chips",
            htmltools::span("Popular: "),
            shiny::actionLink("map_chip_sped", "Special Ed"),
            shiny::actionLink("map_chip_math", "Mathematics"),
            shiny::actionLink("map_chip_elem", "Elementary"),
            shiny::actionLink("map_chip_science", "Science"),
            shiny::actionLink("map_chip_music", "Music / Art"),
            shiny::actionLink("map_chip_driver", "Bus Driver"),
            shiny::actionLink("map_chip_clear", "\u2715 Clear", class = "text-danger fw-semibold")
          ),
          shiny::selectInput(
            "map_county", "County:",
            choices = county_choices,
            selected = "all"
          ),
          shiny::checkboxGroupInput(
            "map_types", "Employer type:",
            choices = c("K-12 Districts" = "K-12 District", "Higher Ed" = "Higher Ed Institution"),
            selected = c("K-12 District", "Higher Ed Institution"),
            inline = TRUE
          ),
          shiny::selectInput(
            "map_salary_tier", "Pay:",
            # Built from PAY_BANDS. A posting counts by its posted pay,
            # else its district/institution role figure, else the BLS
            # estimate -- each in its own unit.
            choices = c("All pay" = "all", stats::setNames(names(pay_bands), vapply(pay_bands, function(b) {
              paste0(if (b$unit == "hour") "Hourly: " else "Annual: ", b$label)
            }, character(1)))),
            selected = "all"
          ),
          shiny::selectInput(
            "map_urgency", "Posting volume:",
            choices = c(
              "All Active Locations" = "all",
              "High Volume (5+ openings)" = "5_plus",
              "Multiple Openings (2+)" = "2_plus",
              "New Postings This Week" = "has_new"
            ),
            selected = "all"
          ),
          shiny::actionButton("map_filter_reset", "Reset All Filters", icon = shiny::icon("arrow-rotate-left"),
                       class = "btn-outline-secondary btn-sm w-100")
        ),
        htmltools::div(class = "mb-2", shiny::uiOutput("map_filter_count")),
        shinycssloaders::withSpinner(leaflet::leafletOutput("combined_map", height = 650)),
        htmltools::p(class = "text-muted small mt-2 mb-0",
          map_note,
          "Click a marker to see its jobs.",
          if (nzchar(he_place_note)) htmltools::tags$br(), he_place_note)
      )
    )
  )
}
