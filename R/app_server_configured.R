# Server blocks the two states' app.R had with the same code apart from a setting (step 7 slice 3, 2026-11-21): the
# map's opening view, the Institution Summary table's page length and Wyoming's shared-reporting note, and the HE
# timeline slider. dashboard_server_shared() calls this; the bodies are Montana's app.R blocks with calls written
# pkg::fn(). Montana's slider code generalizes Wyoming's: on Wyoming's data (HE history over a year) it gives
# Wyoming's exact opening range and an empty note.

# USGS's acknowledgment for The National Map tiles, as both states' app.R had it.
USGS_TILE_CREDIT <- paste('<a href="https://www.usgs.gov/programs/national-geospatial-program/national-map"',
                          'target="_blank" rel="noopener">USGS The National Map</a>: map services and data available',
                          "from U.S. Geological Survey, National Geospatial Program")

#' A timeline slider's opening range
#'
#' The last `days` of `dates`, or all of them when the history is shorter.
#' @param dates Archive dates.
#' @param days How far back the opening range reaches.
#' @return Two dates.
#' @export
bounded_timeline_value <- function(dates, days = 365) {
  latest <- max(dates)
  c(max(min(dates), latest - days), latest)
}

#' A note when the archive covers less than a year
#'
#' @param dates Archive dates.
#' @return "Only N days of archived history are currently available." when `dates` span under 365 days, else "".
#' @export
timeline_history_note <- function(dates) {
  span <- as.integer(max(dates) - min(dates))
  if (span < 365) {
    paste0("Only ", span, " days of archived history are currently available.")
  } else {
    ""
  }
}

# The slider label: the history note (when there is one) and the range shown.
slider_label_text <- function(dates, range) {
  parts <- c(timeline_history_note(dates), paste0("Showing: ", range[1], " to ", range[2]))
  paste(parts[nzchar(parts)], collapse = " ")
}

check_map_view <- function(map_view) {
  if (!is.list(map_view) || !all(c("lng", "lat", "zoom") %in% names(map_view)) ||
      !all(vapply(map_view[c("lng", "lat", "zoom")], function(x) is.numeric(x) && length(x) == 1, logical(1)))) {
    stop("map_view must be a list of one number each: lng, lat, zoom", call. = FALSE)
  }
  map_view
}

dashboard_server_configured <- function(input, output, session, combined_map_data, ccdata, k12sum, hesum_he,
                                        vacancy_rate_palette, vacancy_rate_domain, WINDOW_WEEKS, filtered_hesum,
                                        map_view, he_page_length, he_shared_salary_note) {
  # -------- Combined map (Introduction tab) --------
  output$combined_map <- leaflet::renderLeaflet({
    leaflet::leaflet() %>%
      # Basemaps with open terms, OpenStreetMap first (Mike, 2026-11-15; claude-notes/2026-11-15_basemap-options.md).
      # Esri's tiles were dropped: its terms need Esri software or an ArcGIS
      # Online subscription for use like this. USGS The National Map is public
      # domain ("There are no restrictions"), with an acknowledgment requested;
      # OpenStreetMap needs its credit, which addTiles() shows by default.
      # OSM's tile policy: "https://tile.openstreetmap.org/{z}/{x}/{y}.png" exactly
      # (Leaflet's default still uses the old a/b/c subdomains).
      leaflet::addTiles(
        urlTemplate = "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
        attribution = '&copy; <a href="https://www.openstreetmap.org/copyright" target="_blank" rel="noopener">OpenStreetMap</a> contributors',
        options = leaflet::tileOptions(maxZoom = 19),
        group = "OpenStreetMap (Default)"
      ) %>%
      leaflet::addTiles(
        urlTemplate = "https://basemap.nationalmap.gov/arcgis/rest/services/USGSTopo/MapServer/tile/{z}/{y}/{x}",
        attribution = USGS_TILE_CREDIT,
        options = leaflet::tileOptions(maxZoom = 16),
        group = "USGS National Map"
      ) %>%
      leaflet::addTiles(
        urlTemplate = "https://basemap.nationalmap.gov/arcgis/rest/services/USGSShadedReliefOnly/MapServer/tile/{z}/{y}/{x}",
        attribution = USGS_TILE_CREDIT,
        options = leaflet::tileOptions(maxZoom = 16),
        group = "Shaded Relief"
      ) %>%
      leaflet::addTiles(
        urlTemplate = "https://basemap.nationalmap.gov/arcgis/rest/services/USGSHydroCached/MapServer/tile/{z}/{y}/{x}",
        options = leaflet::tileOptions(maxZoom = 16),
        group = "Shaded Relief"
      ) %>%
      leaflet::setView(lng = map_view$lng, lat = map_view$lat, zoom = map_view$zoom) %>%
      leaflet::addLayersControl(
        baseGroups = c("OpenStreetMap (Default)", "USGS National Map", "Shaded Relief"),
        options = leaflet::layersControlOptions(collapsed = TRUE)
      ) %>%
      leaflet::addLegend(position = "bottomright", pal = vacancy_rate_palette, values = vacancy_rate_domain,
                         title = "Vacancy rate", labFormat = leaflet::labelFormat(suffix = "%", transform = function(x) 100 * x),
                         na.label = "N/A")
  })
  # The map lives on its own tab, not the opening one: without this, the state's marker observe() calls
  # leafletProxy() before the browser has rendered the widget, and the markers never appear (WY, 2026-08-05:
  # "Couldn't find map with id combined_map" in the browser console).
  shiny::outputOptions(output, "combined_map", suspendWhenHidden = FALSE)


  shiny::observeEvent(input$map_county, {
    if (is.null(input$map_county) || input$map_county == "all") {
      leaflet::leafletProxy("combined_map") %>%
        leaflet::setView(lng = map_view$lng, lat = map_view$lat, zoom = map_view$zoom)
    } else {
      county_data <- combined_map_data %>%
        dplyr::filter(County == input$map_county, !is.na(Latitude), !is.na(Longitude))
      if (nrow(county_data) > 0) {
        if (nrow(county_data) == 1) {
          leaflet::leafletProxy("combined_map") %>%
            leaflet::setView(lng = county_data$Longitude[1], lat = county_data$Latitude[1], zoom = 9)
        } else {
          leaflet::leafletProxy("combined_map") %>%
            leaflet::fitBounds(
              lng1 = min(county_data$Longitude) - 0.15,
              lat1 = min(county_data$Latitude) - 0.15,
              lng2 = max(county_data$Longitude) + 0.15,
              lat2 = max(county_data$Latitude) + 0.15
            )
        }
      }
    }
  }, ignoreInit = TRUE)


  # Shown above the K-12 timeline slider only where the state's k12_trends_panel(slider_label = TRUE) has the output.
  output$k12_slider_label <- shiny::renderText({
    shiny::req(input$k12_scroll)
    slider_label_text(k12sum$Archive_Date, input$k12_scroll)
  })


  # Institution Summary: the map popups' data as an exportable table, not one marker at a time.
  output$he_summary_table <- DT::renderDT({
    he_rows <- combined_map_data %>% dplyr::filter(Type == "Higher Ed Institution")
    appointment_counts <- ccdata %>%
      dplyr::count(Institution, Appointment, name = "Openings") %>%
      tidyr::complete(
        Institution,
        Appointment = c("Faculty/instructor (non-adjunct)", "Adjunct/part-time faculty"),
        fill = list(Openings = 0L)
      ) %>%
      tidyr::pivot_wider(names_from = Appointment, values_from = Openings, values_fill = 0)
    year <- unique(stats::na.omit(he_rows$Salary_Year))
    year_label <- if (length(year) > 0) year[1] else "current"
    year_int <- suppressWarnings(as.integer(year_label))
    y1_label <- if (!is.na(year_int)) as.character(year_int - 1) else "1 year ago"
    y2_label <- if (!is.na(year_int)) as.character(year_int - 2) else "2 years ago"

    df <- he_rows %>%
      dplyr::left_join(appointment_counts, by = c("Name" = "Institution")) %>%
      dplyr::arrange(dplyr::desc(CurrentCount)) %>%
      dplyr::transmute(
        Institution = Name,
        County,
        `Current Openings` = CurrentCount,
        `Faculty/instructor (non-adjunct)` = dplyr::coalesce(`Faculty/instructor (non-adjunct)`, 0L),
        `Adjunct/part-time faculty` = dplyr::coalesce(`Adjunct/part-time faculty`, 0L),
        `New This Week` = WeeklyNew,
        `Faculty Vacancy Rate` = ifelse(is.na(Vacancy_Rate), NA_character_, scales::percent(Vacancy_Rate, accuracy = 0.1)),
        AvgFacultySalary = ifelse(is.na(Faculty_Avg_Salary), NA_character_, scales::dollar(Faculty_Avg_Salary)),
        AvgFacultySalaryY1 = ifelse(is.na(Faculty_Avg_Salary_Y1Ago), NA_character_, scales::dollar(Faculty_Avg_Salary_Y1Ago)),
        AvgFacultySalaryY2 = ifelse(is.na(Faculty_Avg_Salary_Y2Ago), NA_character_, scales::dollar(Faculty_Avg_Salary_Y2Ago)),
        ProfessorAvgSalary = ifelse(is.na(Faculty_Avg_Salary_Professor), NA_character_, scales::dollar(Faculty_Avg_Salary_Professor)),
        `Faculty Count` = Faculty_Count,
        Enrollment,
        `Students per Faculty` = ifelse(is.na(Students_Per_Teacher), NA_character_, sprintf("%.1f", Students_Per_Teacher)),
        `Enrollment Trend (5yr)` = ifelse(is.na(Enrollment_Change_Pct), NA_character_,
                                           paste0(ifelse(Enrollment_Change_Pct >= 0, "+", ""), scales::percent(Enrollment_Change_Pct, accuracy = 0.1))),
        `Pell Grant Recipient Share` = ifelse(is.na(Pell_Recipient_Share), NA_character_,
                                               paste0(scales::percent(Pell_Recipient_Share, accuracy = 0.1), " (", Pell_Year, ")")),
        `County Median Income` = ifelse(is.na(Median_Household_Income), NA_character_,
                                        paste0(scales::dollar(Median_Household_Income), format_moe_dollars(Median_Household_Income_MOE))),
        `County Median Rent` = ifelse(is.na(Median_Gross_Rent), NA_character_,
                                      paste0(scales::dollar(Median_Gross_Rent), "/mo", format_moe_dollars(Median_Gross_Rent_MOE))),
        `County Mining/Energy Jobs` = ifelse(is.na(Mining_Employment_Share), NA_character_,
                                             paste0(scales::percent(Mining_Employment_Share, accuracy = 0.1), format_moe_points(Mining_Employment_Share_MOE),
                                                    ifelse(indistinct_from_zero(Mining_Employment_Share, Mining_Employment_Share_MOE),
                                                           " (not distinguishable from 0)", ""))),
        `County Population Trend (5yr)` = ifelse(is.na(Population_Change_Pct), NA_character_,
                                                   paste0(ifelse(Population_Change_Pct >= 0, "+", ""), scales::percent(Population_Change_Pct, accuracy = 0.1))),
        # A short label for rows with a Salary_Note (WY: Sheridan and Gillette report salary and vacancy rate
        # together), not the full note: that blew out the row's height (WY, 2026-08-05). Plain text so the copy
        # and CSV buttons export clean text. NULL leaves the column out.
        Note = if (is.null(he_shared_salary_note)) NULL else ifelse(is.na(Salary_Note), "", he_shared_salary_note)
      )
    names(df)[names(df) == "AvgFacultySalary"] <- paste0("Avg. salary, full-time instructional staff (IPEDS, 9\u201312-month contracts), ", year_label)
    names(df)[names(df) == "AvgFacultySalaryY1"] <- paste0("Avg. salary, full-time instructional staff (IPEDS, 9\u201312-month contracts), ", y1_label)
    names(df)[names(df) == "AvgFacultySalaryY2"] <- paste0("Avg. salary, full-time instructional staff (IPEDS, 9\u201312-month contracts), ", y2_label)
    names(df)[names(df) == "ProfessorAvgSalary"] <- paste0("Professor Avg Salary (", year_label, ")")

    DT::datatable(df, filter = "top", extensions = "Buttons",
                  options = list(scrollX = TRUE, dom = "Bfrtip", buttons = c("copy", "csv", "print"), pageLength = he_page_length))
  })


  shiny::observe({
    df <- filtered_hesum()
    shiny::req(nrow(df) > 0)

    df$Archive_Date <- as.Date(df$Archive_Date)
    he_dates <- sort(unique(df$Archive_Date))

    shiny::updateSliderInput(
      session,
      "he_scroll",
      min = min(he_dates),
      max = max(he_dates),
      value = bounded_timeline_value(he_dates, days = WINDOW_WEEKS * 7),
      timeFormat = "%Y-%m-%d"
    )

    session$userData$he_dates <- he_dates
  })


  output$he_slider_label <- shiny::renderText({
    shiny::req(input$he_scroll)
    slider_label_text(hesum_he$Archive_Date, input$he_scroll)
  })

  invisible(NULL)
}
