# The dashboard server code both states share (step 7, slice 1, 2026-11-21). Generated from Montana's app.R by
# Jobs_Dash_repos claude-notes/2026-11-21_step7-slice1/make_server_shared.R: the 101 server blocks that were identical
# in both states' app.R (90) or differed only by the state's name (11), in app.R's order. Changes from app.R: calls
# into packages the app attached are written pkg::fn(); download file names take `state_name`; the app-level data
# objects come in through `data`. The blocks that still differ by state stay in each app.R and read the reactives
# this function returns.

#' Shared dashboard server
#'
#' Registers the outputs and observers both states' dashboards share. Call it first inside the app's `server`
#' function; the state's own outputs then read the reactives it returns.
#'
#' @param input,output,session The Shiny server arguments.
#' @param data Named list of the app-level objects the shared code reads (see `dashboard_server_data_names()`).
#' @param state_name State name, as shown and in download file names ("Montana" gives "montana_...csv").
#' @param feedback_repo GitHub repo the Feedback form files issues in ("owner/name").
#' @return A named list of the reactives and helper functions it defines. The state's own blocks read
#'   `k12_not_marked_only`, `selected_district`, `selected_k12_query`, `selected_k12_type`,
#'   `selected_pay_band`, `map_filtered`, `filtered_hesum`; the state's tests reach the others as
#'   `shared$name`.
#' @export
dashboard_server_shared <- function(input, output, session, data, state_name, feedback_repo) {
  if (!requireNamespace("shiny", quietly = TRUE)) stop("dashboard_server_shared() needs the shiny package.", call. = FALSE)
  missing <- setdiff(dashboard_server_data_names(), names(data))
  if (length(missing)) stop("dashboard_server_shared(): `data` lacks ", paste(missing, collapse = ", "), call. = FALSE)
  file_prefix <- tolower(state_name)
  ccdata <- data[["ccdata"]]
  combined_map_data <- data[["combined_map_data"]]
  combineddata <- data[["combineddata"]]
  data_dictionary <- data[["data_dictionary"]]
  DATA_LOAD_ISSUES <- data[["DATA_LOAD_ISSUES"]]
  HE_CATEGORY_COLORS_AGG <- data[["HE_CATEGORY_COLORS_AGG"]]
  HE_CATEGORY_COLORS_DETAIL <- data[["HE_CATEGORY_COLORS_DETAIL"]]
  he_faculty_counts <- data[["he_faculty_counts"]]
  he_institution_weekly_totals <- data[["he_institution_weekly_totals"]]
  he_job_duration_spells <- data[["he_job_duration_spells"]]
  he_jobtype_weekly_totals <- data[["he_jobtype_weekly_totals"]]
  he_new_this_week <- data[["he_new_this_week"]]
  hesum_he <- data[["hesum_he"]]
  hesum_he_agg <- data[["hesum_he_agg"]]
  HIDDEN_COLS <- data[["HIDDEN_COLS"]]
  IPEDS_SALARY_SOURCE_URL <- data[["IPEDS_SALARY_SOURCE_URL"]]
  job_duration_spells <- data[["job_duration_spells"]]
  K12_CATEGORY_COLORS_AGG <- data[["K12_CATEGORY_COLORS_AGG"]]
  K12_CATEGORY_COLORS_DETAIL <- data[["K12_CATEGORY_COLORS_DETAIL"]]
  k12_district_weekly_totals <- data[["k12_district_weekly_totals"]]
  k12_jobtype_weekly_totals <- data[["k12_jobtype_weekly_totals"]]
  k12sum <- data[["k12sum"]]
  k12sum_agg <- data[["k12sum_agg"]]
  last_refreshed_date <- data[["last_refreshed_date"]]
  NOT_MARKED_SHOWING <- data[["NOT_MARKED_SHOWING"]]
  NOT_MARKED_SHOWING_HE <- data[["NOT_MARKED_SHOWING_HE"]]
  PAY_BANDS <- data[["PAY_BANDS"]]

  feedback_server(input, output, session, repo = feedback_repo, state_name = state_name)

  selected_district <- shiny::reactiveVal(NULL)
  selected_institution <- shiny::reactiveVal(NULL)
  # Map search carried over from a map popup's "View matching jobs" link;
  # NULL whenever the Jobs tab is reached any other way.
  selected_k12_query <- shiny::reactiveVal(NULL)
  selected_he_query <- shiny::reactiveVal(NULL)
  # Pay band carried over from a map popup (PAY_BANDS name), or NULL.
  selected_pay_band <- shiny::reactiveVal(NULL)
  # Job type picked on the Home page's job-type tables (its postings not marked part-time etc.), or NULL.
  selected_k12_type <- shiny::reactiveVal(NULL)
  selected_he_type <- shiny::reactiveVal(NULL)
  # TRUE when a Home link carried the switch (on) to the Jobs table: only postings not marked part-time etc.
  k12_not_marked_only <- shiny::reactiveVal(FALSE)
  he_not_marked_only <- shiny::reactiveVal(FALSE)

  output$data_load_issues_banner <- shiny::renderUI({
    shiny::req(length(DATA_LOAD_ISSUES) > 0)
    bslib::card(class = "border-danger",
         bslib::card_header(class = "bg-danger text-white", "Data issue detected"),
         bslib::card_body(
           htmltools::tags$ul(lapply(DATA_LOAD_ISSUES, htmltools::tags$li)),
           shiny::helpText("This is a schema problem in the underlying data pipeline, not something wrong with your view of the dashboard -- the missing field(s) above need a pipeline fix.")))
  })

  # -------- Intro KPIs --------
  wow_delta_ui <- function(delta) {
    if (is.na(delta)) return(NULL)
    arrow <- if (delta >= 0) "\u25b2" else "\u25bc"
    htmltools::tags$div(style = "font-size: 0.85em; margin-top: 2px;", paste0(arrow, " ", abs(delta), " vs last week"))
  }

  not_marked_line <- function(n, what) {
    htmltools::tags$small(style = "display: block; font-size: 11px; opacity: 0.85;", paste(format(n, big.mark = ","), what))
  }

  statewide_weekly_series <- function(weekly_totals) {
    weekly_totals %>%
      dplyr::group_by(Archive_Date) %>%
      dplyr::summarize(n = sum(n), .groups = "drop") %>%
      dplyr::arrange(Archive_Date) %>%
      utils::tail(12) %>%
      dplyr::pull(n)
  }

  output$kpi_k12_total <- shiny::renderUI({
    kpi_value_box(format(nrow(combineddata), big.mark = ","),
                  "Open K-12 Postings",
                  icon = htmltools::HTML(make_sparkline_svg_light(statewide_weekly_series(k12_district_weekly_totals))),
                  color = "blue",
                  not_marked_line(sum(combineddata$.Not_Marked), "excluding substitutes, coaching and part-time"),
                  wow_delta_ui(compute_wow_delta(k12_district_weekly_totals)))
  })
  output$kpi_he_total <- shiny::renderUI({
    kpi_value_box(format(nrow(ccdata), big.mark = ","),
             "Open Higher Ed Postings",
             icon = htmltools::HTML(make_sparkline_svg_light(statewide_weekly_series(he_institution_weekly_totals))),
             color = "purple",
               htmltools::tags$small(
                 style = "display: block; font-size: 11px; opacity: 0.85;",
                 sprintf(
                   "%s faculty/instructor (non-adjunct) | %s adjunct/part-time faculty",
                   format(he_faculty_counts$faculty_instructor, big.mark = ","),
                   format(he_faculty_counts$adjunct_part_time, big.mark = ",")
                 )
               ),
               not_marked_line(sum(ccdata$.Not_Marked), "excluding adjunct, part-time, student and temporary"),
               wow_delta_ui(compute_wow_delta(he_institution_weekly_totals)))
  })
  output$kpi_last_refreshed <- shiny::renderUI({
    kpi_value_box(last_refreshed_date, "Last Refreshed", icon = shiny::icon("calendar"), color = "green")
  })

  # Home switch: count only postings classify_work_schedule() doesn't mark (on, the default) or every posting. The
  # sparklines read the pipeline's matching per-run column (n_not_marked or n), so a line and its table agree.
  home_not_marked <- shiny::reactive(!isFALSE(input$home_not_marked))
  home_count_col <- shiny::reactive(if (home_not_marked()) "n_not_marked" else "n")

  top_entity_table <- function(df, col, totals, accent) {
    top <- df %>% dplyr::count(.data[[col]], sort = TRUE, name = "Open Postings") %>% utils::head(5)
    top$`Last 12 Weeks` <- vapply(top[[col]], function(e) {
      series <- totals[totals[[col]] %in% e, ] %>% dplyr::arrange(Archive_Date) %>% utils::tail(12)
      make_sparkline_svg(series[[home_count_col()]], accent = accent)
    }, character(1))
    top[[col]] <- paste0(
      '<a href="#" onclick="Shiny.setInputValue(\'popup_view_jobs\', \'',
      gsub("'", "&#39;", top[[col]], fixed = TRUE),
      '\', {priority: \'event\'}); return false;">',
      top[[col]],
      '</a>'
    )
    top
  }
  output$top_k12_districts <- shiny::renderTable({
    df <- if (home_not_marked()) combineddata %>% dplyr::filter(.Not_Marked) else combineddata
    top_entity_table(df, "District", k12_district_weekly_totals, "#2a78d6")
  }, sanitize.text.function = function(x) x)
  output$top_he_institutions <- shiny::renderTable({
    df <- if (home_not_marked()) ccdata %>% dplyr::filter(.Not_Marked) else ccdata
    top_entity_table(df, "Institution", he_institution_weekly_totals, "#4a3aa7")
  }, sanitize.text.function = function(x) x)

  # The five most-posted job types; sparkline from the pipeline's per-run totals by type, counted the same way.
  top_job_types_table <- function(types, totals, accent, input_id) {
    top <- data.frame(Type = types, stringsAsFactors = FALSE) %>% dplyr::count(Type, sort = TRUE, name = "Open Postings") %>% utils::head(5)
    top$Share <- scales::percent(top$`Open Postings` / length(types), accuracy = 1)
    top$`Last 12 Weeks` <- vapply(top$Type, function(t) {
      make_sparkline_svg(totals %>% dplyr::filter(Job_Type == t) %>% dplyr::arrange(Archive_Date) %>% utils::tail(12) %>% dplyr::pull(!!home_count_col()),
                         accent = accent)
    }, character(1))
    top$Type <- paste0('<a href="#" onclick="Shiny.setInputValue(\'', input_id, '\', \'',
                       gsub("'", "&#39;", top$Type, fixed = TRUE), '\', {priority: \'event\'}); return false;">',
                       gsub("/", "/<wbr>", top$Type, fixed = TRUE), '</a>')  # "Instructor/Teacher/Faculty" wraps on phones
    names(top)[1] <- "Job Type"
    top
  }
  output$top_k12_job_types <- shiny::renderTable({
    keep <- !home_not_marked() | combineddata$.Not_Marked
    top_job_types_table(combineddata$Position[keep], k12_jobtype_weekly_totals, "#2a78d6", "jobtype_view_k12")
  }, sanitize.text.function = function(x) x)
  output$top_he_job_types <- shiny::renderTable({
    keep <- !home_not_marked() | ccdata$.Not_Marked
    top_job_types_table(ccdata$.Job_Type[keep], he_jobtype_weekly_totals, "#4a3aa7", "jobtype_view_he")
  }, sanitize.text.function = function(x) x)

  output$he_new_table <- DT::renderDT({
    df <- he_new_this_week
    if (!identical(input$he_new_appointment, "all")) {
      df <- df %>% dplyr::filter(Appointment == input$he_new_appointment)
    }
    DT::datatable(
      df %>% dplyr::select(Title, Institution, Location, Appointment, Category, Link),
      options = list(scrollX = TRUE)
    )
  })

  # ------------------ Open Research Downloads ------------------
  output$download_active_k12 <- shiny::downloadHandler(
    filename = function() { paste0(file_prefix, "_active_k12_jobs_", Sys.Date(), ".csv") },
    content = function(file) {
      utils::write.csv(combineddata %>% dplyr::select(-dplyr::any_of(HIDDEN_COLS)), file, row.names = FALSE, na = "")
    }
  )

  output$download_history_k12 <- shiny::downloadHandler(
    filename = function() { paste0(file_prefix, "_k12_historical_archive_", Sys.Date(), ".csv") },
    content = function(file) {
      file.copy("k12jobanalysis.csv", file)
    }
  )

  output$download_district_benchmarks <- shiny::downloadHandler(
    filename = function() { paste0(file_prefix, "_district_profiles_", Sys.Date(), ".csv") },
    content = function(file) {
      file.copy("salarymap2.csv", file)
    }
  )

  output$download_history_he <- shiny::downloadHandler(
    filename = function() { paste0(file_prefix, "_higher_ed_historical_archive_", Sys.Date(), ".csv") },
    content = function(file) {
      file.copy("facultydata.csv", file)
    }
  )

  output$download_he_benchmarks <- shiny::downloadHandler(
    filename = function() { paste0(file_prefix, "_higher_ed_profiles_", Sys.Date(), ".csv") },
    content = function(file) {
      file.copy("salarymap.csv", file)
    }
  )

  output$download_weekly_totals <- shiny::downloadHandler(
    filename = function() { paste0(file_prefix, "_k12_weekly_vacancy_series_", Sys.Date(), ".csv") },
    content = function(file) {
      utils::write.csv(latest_snapshot_per_week(utils::read.csv("k12_district_weekly_totals.csv", fileEncoding = "UTF-8")), file, row.names = FALSE)
    }
  )

  output$download_he_weekly_totals <- shiny::downloadHandler(
    filename = function() { paste0(file_prefix, "_he_weekly_vacancy_series_", Sys.Date(), ".csv") },
    content = function(file) {
      utils::write.csv(latest_snapshot_per_week(utils::read.csv("he_institution_weekly_totals.csv", fileEncoding = "UTF-8")), file, row.names = FALSE)
    }
  )

  output$download_spells <- shiny::downloadHandler(
    filename = function() { paste0(file_prefix, "_job_duration_spells_", Sys.Date(), ".csv") },
    content = function(file) {
      file.copy("job_duration_spells.csv", file)
    }
  )

  output$download_he_spells <- shiny::downloadHandler(
    filename = function() { paste0(file_prefix, "_he_job_duration_spells_", Sys.Date(), ".csv") },
    content = function(file) {
      if (file.exists("he_job_duration_spells.csv")) {
        file.copy("he_job_duration_spells.csv", file)
      } else {
        utils::write.csv(data.frame(), file)
      }
    }
  )

  output$download_dictionary <- shiny::downloadHandler(
    filename = function() { paste0(file_prefix, "_data_dictionary_", Sys.Date(), ".csv") },
    content = function(file) {
      file.copy("data_dictionary.csv", file)
    }
  )

  output$data_dictionary_table <- DT::renderDT({
    shiny::req(data_dictionary)
    DT::datatable(
      data_dictionary,
      filter = "top",
      escape = FALSE,
      options = list(pageLength = 15, scrollX = TRUE)
    )
  })

  output$k12_filter_status <- shiny::renderUI({
    if (!is.null(selected_k12_type())) {
      return(htmltools::div(style = "margin-bottom: 10px;",
                 htmltools::strong(paste0("Showing: ", selected_k12_type(), " jobs", if (k12_not_marked_only()) paste0(", ", NOT_MARKED_SHOWING))),
                 shiny::actionButton("clear_k12_type", "Show All Jobs", class = "btn-xs", style = "margin-left: 10px;")))
    }
    if (is.null(selected_district()) && !k12_not_marked_only()) return(NULL)
    htmltools::div(style = "margin-bottom: 10px;",
        htmltools::strong(paste0("Showing: ", if (is.null(selected_district())) "all districts" else selected_district(),
                      if (k12_not_marked_only()) paste0(" \u2014 ", NOT_MARKED_SHOWING) else "",
                      if (!is.null(selected_k12_query())) paste0(" \u2014 jobs matching \u201c", selected_k12_query(), "\u201d") else "",
                      if (!is.null(selected_pay_band())) paste0(" \u2014 pay ", PAY_BANDS[[selected_pay_band()]]$label) else "")),
        shiny::actionButton("clear_k12_filter", "Show All Districts", class = "btn-xs", style = "margin-left: 10px;")
    )
  })
  shiny::observeEvent(input$clear_k12_filter, { selected_district(NULL); selected_k12_query(NULL); selected_pay_band(NULL); k12_not_marked_only(FALSE) })
  shiny::observeEvent(input$clear_k12_type, { selected_k12_type(NULL); k12_not_marked_only(FALSE) })

  #--------------------------------------------------
  # K-12 Head-to-Head Comparator
  #--------------------------------------------------
  shiny::observeEvent(input$k12_comp_preset_top, {
    top_districts <- combined_map_data %>%
      dplyr::filter(Type == "K-12 District") %>%
      dplyr::arrange(dplyr::desc(CurrentCount)) %>%
      dplyr::pull(Name) %>%
      utils::head(3)
    shiny::updateSelectizeInput(session, "k12_compare_districts", selected = top_districts)
  })

  shiny::observeEvent(input$k12_comp_preset_largest, {
    largest_districts <- combined_map_data %>%
      dplyr::filter(Type == "K-12 District") %>%
      dplyr::arrange(dplyr::desc(Enrollment)) %>%
      dplyr::pull(Name) %>%
      utils::head(3)
    shiny::updateSelectizeInput(session, "k12_compare_districts", selected = largest_districts)
  })

  shiny::observeEvent(input$k12_comp_preset_clear, {
    shiny::updateSelectizeInput(session, "k12_compare_districts", selected = character(0))
  })

    

    

  # One bar per selected district, largest enrollment first in both charts so
  # a district sits in the same place in each; value labels on the bars.
  k12_compare_bar_plot <- function(value_col, axis_title, color, empty_text, tick = ",.0f") {
    selected <- input$k12_compare_districts
    if (is.null(selected) || length(selected) == 0) {
      return(plotly::plot_ly(type = "scatter", mode = "none") %>% plotly::layout(
        annotations = list(list(text = empty_text, showarrow = FALSE, font = list(size = 14, color = "#666"))),
        xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)
      ))
    }
    df <- combined_map_data %>%
      dplyr::filter(Type == "K-12 District", Name %in% selected) %>%
      dplyr::mutate(Value = .data[[value_col]]) %>%
      dplyr::select(Name, Enrollment, Value) %>%
      dplyr::arrange(dplyr::desc(dplyr::coalesce(Enrollment, -1)), Name) %>%
      dplyr::mutate(Name = factor(Name, levels = Name),
             Label = ifelse(is.na(Value), "n/a", format(Value, big.mark = ",", trim = TRUE)))
    plotly::plot_ly(df, x = ~Name, y = ~Value, type = "bar", marker = list(color = color),
            text = ~Label, textposition = "outside", cliponaxis = FALSE,
            hovertemplate = paste0("%{x}<br>", axis_title, ": %{text}<extra></extra>")) %>%
      plotly::layout(
        xaxis = list(title = "", tickangle = -15),
        yaxis = list(title = axis_title, tickformat = tick, rangemode = "tozero"),
        showlegend = FALSE,
        margin = list(b = 60, l = 60, r = 20, t = 30)
      )
  }
  output$k12_compare_enrollment_plot <- plotly::renderPlotly(
    k12_compare_bar_plot("Enrollment", "Students enrolled", "#34495e", "Select one or more districts to compare enrollment"))
  output$k12_compare_openings_plot <- plotly::renderPlotly(
    k12_compare_bar_plot("CurrentCount", "Active openings", "#e74c3c", "Select one or more districts to compare openings", tick = ",d"))

  output$k12_compare_jobs_table <- DT::renderDT({
    selected <- input$k12_compare_districts
    df <- combineddata
    if (!is.null(selected) && length(selected) > 0) {
      df <- df %>% dplyr::filter(District %in% selected)
    } else {
      df <- df[0, ]
    }
    DT::datatable(df %>% dplyr::select(-dplyr::any_of(HIDDEN_COLS)), filter = "top", escape = FALSE, extensions = "Buttons",
              options = list(scrollX = TRUE, dom = "Bfrtip", buttons = c("copy", "csv", "print"), pageLength = 10))
  })

  # Quick suggestion chips for position search
  shiny::observeEvent(input$map_chip_sped, {
    shiny::updateTextInput(session, "map_search_keyword", value = "special ed")
  })
  shiny::observeEvent(input$map_chip_math, {
    shiny::updateTextInput(session, "map_search_keyword", value = "mathematics")
  })
  shiny::observeEvent(input$map_chip_elem, {
    shiny::updateTextInput(session, "map_search_keyword", value = "elementary")
  })
  shiny::observeEvent(input$map_chip_science, {
    shiny::updateTextInput(session, "map_search_keyword", value = "science")
  })
  shiny::observeEvent(input$map_chip_music, {
    shiny::updateTextInput(session, "map_search_keyword", value = "music")
  })
  shiny::observeEvent(input$map_chip_driver, {
    shiny::updateTextInput(session, "map_search_keyword", value = "bus driver")
  })
  shiny::observeEvent(input$map_chip_clear, {
    shiny::updateTextInput(session, "map_search_keyword", value = "")
  })

  map_filtered <- shiny::reactive({
    df <- combined_map_data %>% dplyr::filter(Type %in% input$map_types, CurrentCount > 0)

    # Search keyword / role / subject query filter, and the pay band. Both
    # narrow each dot to its matching postings (count, samples, popup).
    kw <- if (!is.null(input$map_search_keyword)) trimws(input$map_search_keyword) else ""
    band <- input$map_salary_tier
    band_on <- !is.null(band) && band %in% names(PAY_BANDS)
    if (nzchar(kw) || band_on) {
      searchable_k12 <- paste(dplyr::coalesce(combineddata$Title, ""), dplyr::coalesce(combineddata$Position, ""))
      k12_matches <- combineddata[match_job_query(searchable_k12, kw) &
                                    in_pay_band(combineddata$.Pay_Value, combineddata$.Pay_Unit, if (band_on) band, PAY_BANDS), , drop = FALSE]

      searchable_he <- paste(dplyr::coalesce(ccdata$Title, ""), dplyr::coalesce(ccdata$Appointment, ""))
      he_matches <- ccdata[match_job_query(searchable_he, kw) &
                             in_pay_band(ccdata$.Pay_Value, ccdata$.Pay_Unit, if (band_on) band, PAY_BANDS), , drop = FALSE]

      k12_counts <- if (nrow(k12_matches) > 0) {
        as.data.frame(table(Name = k12_matches$District), stringsAsFactors = FALSE)
      } else {
        data.frame(Name = character(0), Freq = integer(0))
      }
      names(k12_counts) <- c("Name", "MatchingCount")

      k12_samples <- if (nrow(k12_matches) > 0) {
        k12_matches %>%
          dplyr::group_by(District) %>%
          dplyr::summarize(MatchingSample = paste(utils::head(unique(Title), 3), collapse = "; "), .groups = "drop") %>%
          dplyr::rename(Name = District)
      } else {
        data.frame(Name = character(0), MatchingSample = character(0))
      }

      he_counts <- if (nrow(he_matches) > 0) {
        as.data.frame(table(Name = he_matches$.Map_Name), stringsAsFactors = FALSE)
      } else {
        data.frame(Name = character(0), Freq = integer(0))
      }
      names(he_counts) <- c("Name", "MatchingCount")

      he_samples <- if (nrow(he_matches) > 0) {
        he_matches %>%
          dplyr::group_by(Name = .Map_Name) %>%
          dplyr::summarize(MatchingSample = paste(utils::head(unique(Title), 3), collapse = "; "), .groups = "drop")
      } else {
        data.frame(Name = character(0), MatchingSample = character(0))
      }

      all_counts <- dplyr::bind_rows(k12_counts, he_counts)
      all_samples <- dplyr::bind_rows(k12_samples, he_samples)

      df <- df %>%
        dplyr::inner_join(all_counts, by = "Name") %>%
        dplyr::left_join(all_samples, by = "Name") %>%
        dplyr::mutate(
          MatchingCount = dplyr::coalesce(MatchingCount, 0L),
          MatchingSample = dplyr::coalesce(MatchingSample, ""),
          DisplayCount = MatchingCount,
          SearchActive = TRUE,
          SearchQuery = paste(c(if (nzchar(kw)) kw, if (band_on) PAY_BANDS[[band]]$label), collapse = " \u00b7 "),
          SearchKeyword = kw,
          SearchBand = if (band_on) band else ""
        ) %>%
        dplyr::filter(MatchingCount > 0)
    } else {
      df <- df %>%
        dplyr::mutate(
          MatchingCount = CurrentCount,
          MatchingSample = SampleTitles,
          DisplayCount = CurrentCount,
          SearchActive = FALSE,
          SearchQuery = "",
          SearchKeyword = "",
          SearchBand = ""
        )
    }

    # Filter by county
    if (!is.null(input$map_county) && input$map_county != "all") {
      df <- df %>% dplyr::filter(County == input$map_county)
    }

    # Filter by posting volume / freshness
    if (!is.null(input$map_urgency) && input$map_urgency != "all") {
      if (input$map_urgency == "5_plus") {
        df <- df %>% dplyr::filter(DisplayCount >= 5)
      } else if (input$map_urgency == "2_plus") {
        df <- df %>% dplyr::filter(DisplayCount >= 2)
      } else if (input$map_urgency == "has_new") {
        df <- df %>% dplyr::filter(WeeklyNew > 0)
      }
    }

    df
  })

  output$map_filter_count <- shiny::renderUI({
    filtered_df <- map_filtered()
    filtered_n <- nrow(filtered_df)
    kw <- if (!is.null(input$map_search_keyword)) trimws(input$map_search_keyword) else ""
    total_active <- nrow(combined_map_data %>% dplyr::filter(CurrentCount > 0))
    total_postings <- sum(filtered_df$DisplayCount, na.rm = TRUE)

    if (filtered_n == 0) {
      if (nzchar(kw)) {
        htmltools::tags$span(
          class = "badge",
          style = "background-color: #d9534f; font-size: 0.9em; padding: 6px 12px; font-weight: 500; border-radius: 4px;",
          sprintf('No locations currently have open positions matching "%s" under selected filters \u2014 try broadening your search or click Reset.', kw)
        )
      } else {
        htmltools::tags$span(
          class = "badge",
          style = "background-color: #d9534f; font-size: 0.9em; padding: 6px 12px; font-weight: 500; border-radius: 4px;",
          "No locations match all selected filters \u2014 broaden criteria or click Reset Filters."
        )
      }
    } else {
      if (nzchar(kw)) {
        htmltools::tags$span(
          class = "badge",
          style = "background-color: #198754; font-size: 0.9em; padding: 6px 12px; font-weight: 500; border-radius: 4px;",
          sprintf('Showing %d locations with %s matching openings for "%s"',
                  filtered_n, format(total_postings, big.mark = ","), kw)
        )
      } else {
        htmltools::tags$span(
          class = "badge",
          style = "background-color: #2a78d6; font-size: 0.9em; padding: 6px 12px; font-weight: 500; border-radius: 4px;",
          sprintf("Showing %d of %d active locations (%s current openings across filtered locations)",
                  filtered_n, total_active, format(total_postings, big.mark = ","))
        )
      }
    }
  })

  shiny::observeEvent(input$map_filter_reset, {
    shiny::updateTextInput(session, "map_search_keyword", value = "")
    shiny::updateCheckboxGroupInput(session, "map_types", selected = c("K-12 District", "Higher Ed Institution"))
    shiny::updateSelectInput(session, "map_county", selected = "all")
    shiny::updateSelectInput(session, "map_salary_tier", selected = "all")
    shiny::updateSelectInput(session, "map_urgency", selected = "all")
  })

  # Home's top district/college links (the map popups use map_popup_view_jobs): the Jobs table follows the switch.
  shiny::observeEvent(input$popup_view_jobs, {
    navigate_to_entity_jobs(input$popup_view_jobs, not_marked = home_not_marked())
  })
  shiny::observeEvent(input$jobtype_view_k12, {
    selected_district(NULL); selected_k12_query(NULL); selected_pay_band(NULL)
    selected_k12_type(input$jobtype_view_k12)
    k12_not_marked_only(home_not_marked())
    bslib::nav_select("sidebar_tabs", "k12_table", session = session)
  })
  shiny::observeEvent(input$jobtype_view_he, {
    selected_institution(NULL); selected_he_query(NULL); selected_pay_band(NULL)
    selected_he_type(input$jobtype_view_he)
    he_not_marked_only(home_not_marked())
    bslib::nav_select("sidebar_tabs", "he_table", session = session)
  })

  shiny::observeEvent(input$map_popup_view_jobs, {
    ev <- input$map_popup_view_jobs
    query <- trimws(utils::URLdecode(if (is.null(ev$query)) "" else ev$query))
    band <- if (!is.null(ev$band) && ev$band %in% names(PAY_BANDS)) ev$band else NULL
    navigate_to_entity_jobs(ev$entity, if (nzchar(query)) query else NULL, band)
  })

  navigate_to_entity_jobs <- function(entity, query = NULL, band = NULL, not_marked = FALSE) {
    selected_k12_type(NULL)
    selected_he_type(NULL)
    k12_not_marked_only(FALSE)
    he_not_marked_only(FALSE)
    selected_k12_query(query)
    selected_he_query(query)
    selected_pay_band(band)
    row <- combined_map_data %>% dplyr::filter(Name == entity)
    if (nrow(row) > 0) {
      if (row$Type[1] == "K-12 District") {
        selected_district(entity)
        k12_not_marked_only(not_marked)
        bslib::nav_select("sidebar_tabs", "k12_table", session = session)
      } else {
        selected_institution(entity)
        he_not_marked_only(not_marked)
        bslib::nav_select("sidebar_tabs", "he_table", session = session)
      }
    } else if (entity %in% combineddata$District) {
      selected_district(entity)
      k12_not_marked_only(not_marked)
      bslib::nav_select("sidebar_tabs", "k12_table", session = session)
    } else if (entity %in% ccdata$Institution) {
      selected_institution(entity)
      he_not_marked_only(not_marked)
      bslib::nav_select("sidebar_tabs", "he_table", session = session)
    }
  }

  # ---- K-12 longitudinal trends ----
  shiny::observeEvent(input$k12_detail_level_trends, {
    cats <- if (identical(input$k12_detail_level_trends, "detail")) {
      sort(unique(k12sum$Broad_Category))
    } else {
      sort(unique(k12sum_agg$Broad_Category))
    }
    shinyWidgets::updatePickerInput(session, "broad_category", choices = cats, selected = cats)
  }, ignoreInit = TRUE)

  filtered_k12sum <- shiny::reactive({
    shiny::req(input$district_trend, input$broad_category, input$k12_detail_level_trends)

    base <- if (identical(input$k12_detail_level_trends, "detail")) k12sum else k12sum_agg

    base %>%
      dplyr::filter(
        input$district_trend == "Total" |
          District == input$district_trend,
        Broad_Category %in% input$broad_category
      )
  })

  df_windowed <- shiny::reactive({
    df <- filtered_k12sum()
    shiny::req(nrow(df) > 0, input$k12_scroll)

    df <- df %>%
      dplyr::filter(
        Archive_Date >= input$k12_scroll[1],
        Archive_Date <= input$k12_scroll[2]
      )

    df <- df %>%
      dplyr::filter(District != "Total") %>%
      dplyr::group_by(Broad_Category, Archive_Date) %>%
      dplyr::summarise(sum = sum(sum), .groups = "drop") %>%
      dplyr::arrange(Broad_Category, Archive_Date)

    df
  })

  output$k12_longitudinal_plot <- plotly::renderPlotly({
    df <- df_windowed()

    shiny::validate(
      shiny::need(nrow(df) > 0, "No data for selected districts.")
    )

    all_dates <- sort(unique(df$Archive_Date))

    p <- ggplot2::ggplot(df, ggplot2::aes(
      x = Archive_Date,
      y = sum,
      color = Broad_Category,
      group = Broad_Category,
      text = paste0(
        "Date: ", Archive_Date, "<br>",
        "Category: ", Broad_Category, "<br>",
        "Postings: ", sum
      )
    )) +
      ggplot2::geom_line() +
      ggplot2::geom_point(size = 1) +
      ggplot2::scale_color_manual(values = if (identical(input$k12_detail_level_trends, "detail")) K12_CATEGORY_COLORS_DETAIL else K12_CATEGORY_COLORS_AGG) +
      ggplot2::labs(x = "Archive Date", y = "Number of Postings") +
      ggplot2::scale_x_date(
        breaks = all_dates,
        labels = scales::date_format("%b %d")
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
        legend.position = "bottom",
        legend.key.size = grid::unit(0.5, "cm"),
        legend.box.spacing = grid::unit(0.2, "cm"),
        legend.text = ggplot2::element_text(size = 8),
        legend.title = ggplot2::element_text(size = 10)
      )

    plotly::ggplotly(p, height = 500, tooltip = "text")
  })

  output$k12_current_trends_table <- shiny::renderTable({
    shiny::req(input$k12_detail_level_current)
    hist_src <- if (identical(input$k12_detail_level_current, "detail")) k12sum else k12sum_agg
    history <- hist_src %>%
      dplyr::filter(District == input$district_current) %>%
      dplyr::transmute(Category = Broad_Category, Archive_Date, n = sum)

    result <- build_current_trends_table(history, accent = "#2a78d6")
    shiny::req(!is.null(result))
    result
  }, sanitize.text.function = function(x) x)

  # -------- Higher Ed --------
  output$he_jobs <- DT::renderDT({
    df <- ccdata
    if (!is.null(selected_institution())) df <- df %>% dplyr::filter(Institution == selected_institution() | .Map_Name == selected_institution())
    if (!is.null(selected_he_type())) df <- df %>% dplyr::filter(.Job_Type == selected_he_type())
    if (he_not_marked_only()) df <- df %>% dplyr::filter(.Not_Marked)
    if (!is.null(selected_he_query())) {
      df <- df[match_job_query(paste(dplyr::coalesce(df$Title, ""), dplyr::coalesce(df$Appointment, "")), selected_he_query()), , drop = FALSE]
    }
    if (!is.null(selected_pay_band())) df <- df[in_pay_band(df$.Pay_Value, df$.Pay_Unit, selected_pay_band(), PAY_BANDS), , drop = FALSE]
    df <- df %>% dplyr::select(-dplyr::any_of(HIDDEN_COLS))
    if (!identical(input$he_table_appointment, "all")) {
      df <- df %>% dplyr::filter(Appointment == input$he_table_appointment)
    }
    if (isTRUE(identical(input$he_quick_filter, "new"))) {
      new_keys <- paste(he_new_this_week$Title, he_new_this_week$Institution, sep = "@@@")
      current_keys <- paste(df$Title, df$Institution, sep = "@@@")
      df <- df[current_keys %in% new_keys, ]
    }
    DT::datatable(df, filter = "top", escape = FALSE, extensions = "Buttons",
              options = list(scrollX = TRUE, dom = "Bfrtip", buttons = c("copy", "csv", "print")))
  })

  output$he_filter_status <- shiny::renderUI({
    if (!is.null(selected_he_type())) {
      return(htmltools::div(style = "margin-bottom: 10px;",
                 htmltools::strong(paste0("Showing: ", selected_he_type(), " jobs", if (he_not_marked_only()) paste0(", ", NOT_MARKED_SHOWING_HE))),
                 shiny::actionButton("clear_he_type", "Show All Jobs", class = "btn-xs", style = "margin-left: 10px;")))
    }
    if (is.null(selected_institution()) && !he_not_marked_only()) return(NULL)
    htmltools::div(style = "margin-bottom: 10px;",
        htmltools::strong(paste0("Showing: ", if (is.null(selected_institution())) "all institutions" else selected_institution(),
                      if (he_not_marked_only()) paste0(" \u2014 ", NOT_MARKED_SHOWING_HE) else "",
                      if (!is.null(selected_he_query())) paste0(" \u2014 jobs matching \u201c", selected_he_query(), "\u201d") else "",
                      if (!is.null(selected_pay_band())) paste0(" \u2014 pay ", PAY_BANDS[[selected_pay_band()]]$label) else "")),
        shiny::actionButton("clear_he_filter", "Show All Institutions", class = "btn-xs", style = "margin-left: 10px;")
    )
  })
  shiny::observeEvent(input$clear_he_filter, { selected_institution(NULL); selected_he_query(NULL); selected_pay_band(NULL); he_not_marked_only(FALSE) })
  shiny::observeEvent(input$clear_he_type, { selected_he_type(NULL); he_not_marked_only(FALSE) })

  output$he_summary_footnote <- shiny::renderUI({
    year <- unique(stats::na.omit(combined_map_data$Salary_Year[combined_map_data$Type == "Higher Ed Institution"]))
    source <- unique(stats::na.omit(combined_map_data$Salary_Source[combined_map_data$Type == "Higher Ed Institution"]))
    acs_year <- unique(stats::na.omit(combined_map_data$ACS_Year[combined_map_data$Type == "Higher Ed Institution"]))
    pell_year <- unique(stats::na.omit(combined_map_data$Pell_Year[combined_map_data$Type == "Higher Ed Institution"]))
    shiny::req(length(year) > 0, length(source) > 0)
    htmltools::tagList(
      shiny::helpText(
        "Salary data:", source[1], paste0("(", year[1], " data)"), "\u2014",
        htmltools::tags$a(href = IPEDS_SALARY_SOURCE_URL, target = "_blank", "educationdata.urban.org"),
        ". Average salary of full-time instructional staff on 9- to 12-month contracts, all ranks: IPEDS collects no salaries for part-time staff, or for full-time staff on contracts under 9 months."
      ),
      if (length(acs_year) > 0) {
        shiny::helpText(
          "County context (income, rent, mining/energy employment share, population trend): US Census Bureau, American Community Survey 5-Year Estimates",
          paste0("(", acs_year[1], ")"), "\u2014",
          htmltools::tags$a(href = "https://www.census.gov/data/developers/data-sets/acs-5year.html", target = "_blank", "census.gov"),
          ".",
          "\u00b1 values are the Census Bureau's 90% margins of error (the mining share's from Census's formula for a",
          "proportion); a share whose margin is at least as large as the share is marked \"not distinguishable from 0\".",
          "County population totals are set to Census's population estimates, so the 5-year trend has no margin of error."
        )
      },
      if (length(pell_year) > 0) {
        shiny::helpText(
          "Pell Grant recipient share: US Dept. of Education, Federal Student Aid, via Urban Institute Education Data Portal",
          paste0("(", pell_year[1], " data \u2014 usually a year or more older than the salary/enrollment figures above, since FSA's own data lags IPEDS's)"), "\u2014",
          htmltools::tags$a(href = "https://educationdata.urban.org/documentation/colleges.html", target = "_blank", "educationdata.urban.org"),
          ". Recipients \u00f7 that same year's IPEDS FTE enrollment \u2014 a headcount-over-FTE ratio, not two directly comparable counts."
        )
      }
    )
  })

  #--------------------------------------------------
  # Higher Ed Head-to-Head Comparator
  #--------------------------------------------------
  shiny::observeEvent(input$he_comp_preset_top, {
    top_insts <- combined_map_data %>%
      dplyr::filter(Type == "Higher Ed Institution") %>%
      dplyr::arrange(dplyr::desc(CurrentCount)) %>%
      dplyr::pull(Name) %>%
      utils::head(3)
    shiny::updateSelectizeInput(session, "he_compare_insts", selected = top_insts)
  })

  shiny::observeEvent(input$he_comp_preset_clear, {
    shiny::updateSelectizeInput(session, "he_compare_insts", selected = character(0))
  })

  output$he_compare_cards <- shiny::renderUI({
    selected <- input$he_compare_insts
    if (is.null(selected) || length(selected) == 0) {
      return(htmltools::div(class = "alert alert-info", style = "margin-top: 15px;",
                 shiny::icon("circle-info"), " Please select one to four institutions above to display comparative benchmarks."))
    }
    
    col_width <- dplyr::case_when(
      length(selected) == 1 ~ 12,
      length(selected) == 2 ~ 6,
      length(selected) == 3 ~ 4,
      TRUE ~ 3
    )

    he_rows <- combined_map_data %>%
      dplyr::filter(Type == "Higher Ed Institution", Name %in% selected)

    cards <- lapply(selected, function(iname) {
      row <- he_rows %>% dplyr::filter(Name == iname)
      if (nrow(row) == 0) return(NULL)

      avg_sal <- if (!is.na(row$Faculty_Avg_Salary[1])) scales::dollar(row$Faculty_Avg_Salary[1]) else "N/A"
      prof_sal <- if (!is.na(row$Faculty_Avg_Salary_Professor[1])) scales::dollar(row$Faculty_Avg_Salary_Professor[1]) else "N/A"
      vac_rate <- if (!is.na(row$Vacancy_Rate[1])) scales::percent(row$Vacancy_Rate[1], accuracy = 0.1) else "N/A"
      enroll <- if (!is.na(row$Enrollment[1])) scales::comma(row$Enrollment[1]) else "N/A"
      spf <- if (!is.na(row$Students_Per_Teacher[1])) sprintf("%.1f", row$Students_Per_Teacher[1]) else "N/A"
      enroll_trend <- if (!is.na(row$Enrollment_Change_Pct[1])) paste0(ifelse(row$Enrollment_Change_Pct[1] >= 0, "+", ""), scales::percent(row$Enrollment_Change_Pct[1], accuracy = 0.1)) else "N/A"
      pell <- if (!is.na(row$Pell_Recipient_Share[1])) scales::percent(row$Pell_Recipient_Share[1], accuracy = 0.1) else "N/A"

      shiny::column(
        width = col_width,
        htmltools::div(
          class = "box box-solid box-primary",
          style = "border-top: 3px solid #2a78d6; box-shadow: 0 1px 3px rgba(0,0,0,0.08); margin-bottom: 20px;",
          htmltools::div(
            class = "box-header with-border", style = "background: #f7f9fa; padding: 10px 15px;",
            htmltools::h4(style = "margin: 0; font-size: 15px; font-weight: 600; color: #222;", iname),
            htmltools::tags$small(class = "text-muted", paste0(row$County[1], " County"))
          ),
          htmltools::div(
            class = "box-body", style = "padding: 15px;",
            shiny::fluidRow(
              shiny::column(6, htmltools::div(style = "text-align: center; border-right: 1px solid #eee;",
                           htmltools::div(style = "font-size: 22px; font-weight: 700; color: #2a78d6;", row$CurrentCount[1]),
                           htmltools::div(style = "font-size: 11px; text-transform: uppercase; color: #777;", "Active Openings"))),
              shiny::column(6, htmltools::div(style = "text-align: center;",
                           htmltools::div(style = "font-size: 22px; font-weight: 700; color: #e67e22;", vac_rate),
                           htmltools::div(style = "font-size: 11px; text-transform: uppercase; color: #777;", "Vacancy Rate")))
            ),
            htmltools::tags$hr(style = "margin: 12px 0;"),
            htmltools::tags$table(
              style = "width: 100%; font-size: 13px; line-height: 1.8;",
              htmltools::tags$tr(htmltools::tags$td(class = "text-muted", "Avg. salary, full-time instructional staff:"), htmltools::tags$td(style = "text-align: right; font-weight: 600;", avg_sal)),
              htmltools::tags$tr(htmltools::tags$td(class = "text-muted", "Full Professor Avg:"), htmltools::tags$td(style = "text-align: right; font-weight: 600;", prof_sal)),
              htmltools::tags$tr(htmltools::tags$td(class = "text-muted", "FTE Enrollment:"), htmltools::tags$td(style = "text-align: right; font-weight: 600;", enroll)),
              htmltools::tags$tr(htmltools::tags$td(class = "text-muted", "Students / Faculty:"), htmltools::tags$td(style = "text-align: right; font-weight: 600;", spf)),
              htmltools::tags$tr(htmltools::tags$td(class = "text-muted", "5-Yr Enrollment Trend:"), htmltools::tags$td(style = "text-align: right; font-weight: 600;", enroll_trend)),
              htmltools::tags$tr(htmltools::tags$td(class = "text-muted", "Pell Grant Share:"), htmltools::tags$td(style = "text-align: right; font-weight: 600;", pell))
            )
          )
        )
      )
    })
    shiny::fluidRow(cards)
  })

  output$he_compare_salary_plot <- plotly::renderPlotly({
    selected <- input$he_compare_insts
    if (is.null(selected) || length(selected) == 0) {
      return(plotly::plot_ly(type = "scatter", mode = "none") %>% plotly::layout(
        annotations = list(list(text = "Select one or more institutions to display salary comparison",
                                showarrow = FALSE, font = list(size = 14, color = "#666"))),
        xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)
      ))
    }
    df <- combined_map_data %>%
      dplyr::filter(Type == "Higher Ed Institution", Name %in% selected) %>%
      dplyr::select(Name, Faculty_Avg_Salary, Faculty_Avg_Salary_Y1Ago)
    
    plotly::plot_ly(df, x = ~Name) %>%
      plotly::add_bars(y = ~Faculty_Avg_Salary, name = "Latest IPEDS year", marker = list(color = "#2b5876")) %>%
      plotly::add_bars(y = ~Faculty_Avg_Salary_Y1Ago, name = "Year before", marker = list(color = "#4e4376")) %>%
      plotly::layout(
        barmode = "group",
        xaxis = list(title = "", tickangle = -15),
        yaxis = list(title = "Annual Compensation ($)", tickformat = "$,.0f"),
        legend = list(orientation = "h", x = 0, y = 1.15),
        margin = list(b = 60, l = 60, r = 20, t = 40)
      )
  })

  output$he_compare_openings_plot <- plotly::renderPlotly({
    selected <- input$he_compare_insts
    if (is.null(selected) || length(selected) == 0) {
      return(plotly::plot_ly(type = "scatter", mode = "none") %>% plotly::layout(
        annotations = list(list(text = "Select one or more institutions to display openings breakdown",
                                showarrow = FALSE, font = list(size = 14, color = "#666"))),
        xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)
      ))
    }
    df <- ccdata %>%
      dplyr::filter(Institution %in% selected) %>%
      dplyr::count(Institution, Appointment, name = "Count")
    
    if (nrow(df) == 0) {
      return(plotly::plot_ly(type = "scatter", mode = "none") %>% plotly::layout(
        annotations = list(list(text = "No active postings for selected institutions",
                                showarrow = FALSE, font = list(size = 14, color = "#666"))),
        xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)
      ))
    }

    plotly::plot_ly(df, x = ~Institution, y = ~Count, color = ~Appointment, type = "bar") %>%
      plotly::layout(
        barmode = "stack",
        xaxis = list(title = "", tickangle = -15),
        yaxis = list(title = "Active Openings"),
        legend = list(orientation = "h", x = 0, y = 1.15),
        margin = list(b = 60, l = 50, r = 20, t = 40)
      )
  })

  output$he_compare_jobs_table <- DT::renderDT({
    selected <- input$he_compare_insts
    df <- ccdata
    if (!is.null(selected) && length(selected) > 0) {
      df <- df %>% dplyr::filter(Institution %in% selected)
    } else {
      df <- df[0, ]
    }
    DT::datatable(df %>% dplyr::select(-dplyr::any_of(HIDDEN_COLS)), filter = "top", escape = FALSE, extensions = "Buttons",
              options = list(scrollX = TRUE, dom = "Bfrtip", buttons = c("copy", "csv", "print"), pageLength = 10))
  })

  shiny::observeEvent(input$he_detail_level_trends, {
    cats <- if (identical(input$he_detail_level_trends, "detail")) {
      sort(unique(hesum_he$Category))
    } else {
      sort(unique(hesum_he_agg$Category))
    }
    shinyWidgets::updatePickerInput(session, "he_category", choices = cats, selected = cats)
  }, ignoreInit = TRUE)

  filtered_hesum <- shiny::reactive({
    shiny::req(input$inst_trend, input$he_category, input$he_chart_type, input$he_detail_level_trends)

    base <- if (identical(input$he_detail_level_trends, "detail")) hesum_he else hesum_he_agg

    df <- if (input$inst_trend == "Total") {
      base
    } else {
      base %>% dplyr::filter(Institution == input$inst_trend)
    }

    df <- df %>% dplyr::filter(Category %in% input$he_category)

    if (!identical(input$he_chart_type, "all")) {
      df <- df %>% dplyr::filter(Job_Type == input$he_chart_type)
    }

    df
  })

  he_windowed <- shiny::reactive({
    df <- filtered_hesum()
    shiny::req(input$he_scroll)

    df %>%
      dplyr::filter(
        Archive_Date >= as.Date(input$he_scroll[1]),
        Archive_Date <= as.Date(input$he_scroll[2]),
        Institution != "Total"
      ) %>%
      dplyr::arrange(Archive_Date)
  })

  output$he_longitudinal_plot <- plotly::renderPlotly({
    df <- he_windowed()
    shiny::validate(shiny::need(nrow(df) > 0, "No data for selected institution/category/date range."))

    df <- df %>%
      dplyr::group_by(Category, Archive_Date) %>%
      dplyr::summarize(sum = sum(sum), .groups = "drop") %>%
      dplyr::arrange(Category, Archive_Date) %>%
      dplyr::mutate(Category = factor(Category))

    p <- ggplot2::ggplot(df, ggplot2::aes(
      x = Archive_Date,
      y = sum,
      color = Category,
      group = Category,
      text = paste0(
        "Date: ", Archive_Date, "<br>",
        "Category: ", Category, "<br>",
        "Postings: ", sum
      )
    )) +
      ggplot2::geom_line() +
      ggplot2::geom_point(size = 1) +
      ggplot2::scale_color_manual(values = if (identical(input$he_detail_level_trends, "detail")) HE_CATEGORY_COLORS_DETAIL else HE_CATEGORY_COLORS_AGG) +
      ggplot2::labs(x = "Archive Date", y = "Number of Postings") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
        legend.position = "bottom",
        legend.key.size = grid::unit(0.5, "cm"),
        legend.box.spacing = grid::unit(0.2, "cm"),
        legend.text = ggplot2::element_text(size = 8),
        legend.title = ggplot2::element_text(size = 10)
      )

    plotly::ggplotly(p, height = 500, tooltip = "text")
  })

  output$he_current_trends_table <- shiny::renderTable({
    shiny::req(input$he_detail_level_current, input$he_current_appointment)
    hist_src <- if (identical(input$he_detail_level_current, "detail")) hesum_he else hesum_he_agg
    history <- hist_src %>%
      dplyr::filter(
        Institution == input$inst_current,
        identical(input$he_current_appointment, "all") | Job_Type == input$he_current_appointment
      ) %>%
      dplyr::transmute(Category, Archive_Date, n = sum)

    result <- build_current_trends_table(history, accent = "#4a3aa7")
    shiny::req(!is.null(result))
    result
  }, sanitize.text.function = function(x) x)

  # ------------------ K-12 Vacancy Spells Server Logic ------------------
  shiny::observe({
    if (nrow(job_duration_spells) > 0) {
      dists <- sort(unique(job_duration_spells$District))
      shiny::updateSelectInput(session, "k12_spell_district",
                        choices = c("All Districts" = "all", stats::setNames(dists, dists)),
                        selected = "all")
    }
  })

  filtered_spells <- shiny::reactive({
    shiny::req(nrow(job_duration_spells) > 0)
    df <- job_duration_spells

    if (!is.null(input$k12_spell_status) && !identical(input$k12_spell_status, "all")) {
      df <- df %>% dplyr::filter(Status == input$k12_spell_status)
    }

    if (!is.null(input$k12_spell_position) && !identical(input$k12_spell_position, "all")) {
      if (identical(input$k12_spell_position, "Support")) {
        df <- df %>% dplyr::filter(Position != "Teacher" & Position != "Administration")
      } else {
        df <- df %>% dplyr::filter(Position == input$k12_spell_position)
      }
    }

    if (!is.null(input$k12_spell_discipline) && !identical(input$k12_spell_discipline, "all")) {
      disc <- input$k12_spell_discipline
      if (disc == "stem" && "is_stem" %in% names(df)) df <- df %>% dplyr::filter(is_stem == 1)
      else if (disc == "sped" && "is_sped" %in% names(df)) df <- df %>% dplyr::filter(is_sped == 1)
      else if (disc == "elementary" && "is_elementary" %in% names(df)) df <- df %>% dplyr::filter(is_elementary == 1)
      else if (disc == "secondary" && "is_secondary" %in% names(df)) df <- df %>% dplyr::filter(is_secondary == 1)
      else if (disc == "admin" && "is_admin" %in% names(df)) df <- df %>% dplyr::filter(is_admin == 1)
      else if (disc == "support" && "is_support_staff" %in% names(df)) df <- df %>% dplyr::filter(is_support_staff == 1)
    }

    if (!is.null(input$k12_spell_district) && !identical(input$k12_spell_district, "all")) {
      df <- df %>% dplyr::filter(District == input$k12_spell_district)
    }

    df
  })

  output$k12_spell_median_box <- shiny::renderUI({
    df <- filtered_spells()
    med <- if (nrow(df) > 0) round(stats::median(df$Estimated_Days_Open, na.rm = TRUE)) else 0
    kpi_value_box(
      paste0(med, " Days"),
      "Median Vacancy Duration",
      icon = shiny::icon("stopwatch"),
      color = "blue"
    )
  })

  output$k12_spell_active_box <- shiny::renderUI({
    df <- filtered_spells()
    active_n <- if (nrow(df) > 0) sum(df$Status == "Still Active", na.rm = TRUE) else 0
    kpi_value_box(
      scales::comma(active_n),
      "Currently Open Spells",
      icon = shiny::icon("door-open"),
      color = "green"
    )
  })

  output$k12_spell_persistent_box <- shiny::renderUI({
    df <- filtered_spells()
    persist_n <- if (nrow(df) > 0) sum(df$Status == "Still Active" & df$Estimated_Days_Open >= 60, na.rm = TRUE) else 0
    kpi_value_box(
      scales::comma(persist_n),
      "Persistent Spells (\u226560 Days)",
      icon = shiny::icon("triangle-exclamation"),
      color = "red"
    )
  })

  output$k12_spell_hardest_box <- shiny::renderUI({
    df <- filtered_spells()
    hardest <- "None"
    if (nrow(df) >= 1 && "Broad_Category" %in% names(df)) {
      cat_dur <- df %>%
        dplyr::group_by(Broad_Category) %>%
        dplyr::summarize(med = stats::median(Estimated_Days_Open, na.rm = TRUE), n = dplyr::n(), .groups = "drop") %>%
        dplyr::filter(n >= 1) %>%
        dplyr::arrange(dplyr::desc(med))
      if (nrow(cat_dur) > 0) hardest <- cat_dur$Broad_Category[1]
    }
    kpi_value_box(
      hardest,
      "Longest Duration Category",
      icon = shiny::icon("graduation-cap"),
      color = "purple"
    )
  })

  output$k12_spell_duration_plot <- plotly::renderPlotly({
    df <- filtered_spells()
    if (nrow(df) == 0) {
      p <- ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No vacancy spells match the selected filter criteria", size = 4, color = "#666666") +
        ggplot2::theme_void()
      return(plotly::ggplotly(p) %>% plotly::config(displayModeBar = FALSE))
    }

    cat_summary <- df %>%
      dplyr::group_by(Broad_Category) %>%
      dplyr::summarize(
        Median_Days = round(stats::median(Estimated_Days_Open, na.rm = TRUE)),
        Postings = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::filter(!is.na(Broad_Category) & Broad_Category != "" & Postings >= 1) %>%
      dplyr::arrange(dplyr::desc(Median_Days)) %>%
      utils::head(12)

    if (nrow(cat_summary) == 0) {
      p <- ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No category data available for selected filter", size = 4, color = "#666666") +
        ggplot2::theme_void()
      return(plotly::ggplotly(p) %>% plotly::config(displayModeBar = FALSE))
    }

    p <- ggplot2::ggplot(cat_summary, ggplot2::aes(
      x = stats::reorder(Broad_Category, Median_Days),
      y = Median_Days,
      text = paste0("<b>", Broad_Category, "</b><br>Median Open: ", Median_Days, " days<br>Total Spells: ", Postings)
    )) +
      ggplot2::geom_col(fill = "#2a78d6", width = 0.65) +
      ggplot2::coord_flip() +
      ggplot2::labs(x = NULL, y = "Median Days Open") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(size = 9),
        panel.grid.major.y = ggplot2::element_blank()
      )

    plotly::ggplotly(p, tooltip = "text") %>% plotly::config(displayModeBar = FALSE)
  })

  output$k12_spell_distrib_plot <- plotly::renderPlotly({
    df <- filtered_spells()
    bin_levels <- c("1\u201314 Days", "15\u201330 Days", "31\u201360 Days", "61\u201390 Days", "90+ Days")
    if (nrow(df) == 0) {
      p <- ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No vacancy spells match the selected filter criteria", size = 4, color = "#666666") +
        ggplot2::theme_void()
      return(plotly::ggplotly(p) %>% plotly::config(displayModeBar = FALSE))
    }

    df <- df %>%
      dplyr::mutate(Duration_Bin = dplyr::case_when(
        Estimated_Days_Open <= 14 ~ "1\u201314 Days",
        Estimated_Days_Open <= 30 ~ "15\u201330 Days",
        Estimated_Days_Open <= 60 ~ "31\u201360 Days",
        Estimated_Days_Open <= 90 ~ "61\u201390 Days",
        TRUE ~ "90+ Days"
      )) %>%
      dplyr::mutate(Duration_Bin = factor(Duration_Bin, levels = bin_levels))

    bin_counts <- df %>%
      dplyr::count(Duration_Bin, name = "Count") %>%
      tidyr::complete(Duration_Bin = factor(bin_levels, levels = bin_levels), fill = list(Count = 0)) %>%
      dplyr::filter(!is.na(Duration_Bin))

    p <- ggplot2::ggplot(bin_counts, ggplot2::aes(
      x = Duration_Bin, y = Count,
      text = paste0("<b>", Duration_Bin, "</b><br>Spells: ", Count)
    )) +
      ggplot2::geom_col(fill = "#e67e22", width = 0.6) +
      ggplot2::labs(x = NULL, y = "Number of Vacancies") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 25, hjust = 1, size = 8),
        panel.grid.major.x = ggplot2::element_blank()
      )

    plotly::ggplotly(p, tooltip = "text") %>% plotly::config(displayModeBar = FALSE)
  })

  output$k12_spell_table <- DT::renderDT({
    df <- filtered_spells()
    cols_to_show <- c("District", "Title", "Position", "Broad_Category",
                      "First_Seen_Date", "Last_Seen_Date", "Estimated_Days_Open", "Status")
    existing_cols <- intersect(cols_to_show, names(df))

    df_display <- if (nrow(df) > 0) {
      df %>% dplyr::select(dplyr::all_of(existing_cols)) %>% dplyr::arrange(dplyr::desc(Estimated_Days_Open))
    } else {
      df[0, existing_cols, drop = FALSE]
    }

    DT::datatable(
      df_display %>% dplyr::rename(dplyr::any_of(c(`Days listed` = "Estimated_Days_Open", `First seen` = "First_Seen_Date",
                                     `Last seen` = "Last_Seen_Date", Listing = "Status",
                                     Category = "Broad_Category"))),
      options = list(
        pageLength = 15,
        lengthMenu = c(10, 15, 25, 50, 100),
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel")
      ),
      extensions = "Buttons",
      rownames = FALSE,
      escape = FALSE
    )
  })

  # ------------------ Higher Ed Vacancy Spells ------------------
  shiny::observe({
    if (nrow(he_job_duration_spells) > 0) {
      insts <- sort(unique(he_job_duration_spells$Institution))
      shiny::updateSelectInput(session, "he_spell_institution",
                        choices = c("All Institutions" = "all", stats::setNames(insts, insts)),
                        selected = "all")
    }
  })

  filtered_he_spells <- shiny::reactive({
    shiny::req(nrow(he_job_duration_spells) > 0)
    df <- he_job_duration_spells

    if (!is.null(input$he_spell_status) && !identical(input$he_spell_status, "all")) {
      df <- df %>% dplyr::filter(Status == input$he_spell_status)
    }

    if (!is.null(input$he_spell_job_type) && !identical(input$he_spell_job_type, "all")) {
      df <- df %>% dplyr::filter(Job_Type == input$he_spell_job_type)
    }

    if (!is.null(input$he_spell_discipline) && !identical(input$he_spell_discipline, "all")) {
      disc <- input$he_spell_discipline
      if (disc == "stem" && "is_stem" %in% names(df)) df <- df %>% dplyr::filter(is_stem == 1)
      else if (disc == "health" && "is_health" %in% names(df)) df <- df %>% dplyr::filter(is_health == 1)
      else if (disc == "cte" && "is_cte" %in% names(df)) df <- df %>% dplyr::filter(is_cte == 1)
    }

    if (!is.null(input$he_spell_institution) && !identical(input$he_spell_institution, "all")) {
      df <- df %>% dplyr::filter(Institution == input$he_spell_institution)
    }

    df
  })

  output$he_spell_median_box <- shiny::renderUI({
    df <- filtered_he_spells()
    med <- if (nrow(df) > 0) round(stats::median(df$Estimated_Days_Open, na.rm = TRUE)) else 0
    kpi_value_box(
      paste0(med, " Days"),
      "Median Vacancy Duration",
      icon = shiny::icon("stopwatch"),
      color = "blue"
    )
  })

  output$he_spell_active_box <- shiny::renderUI({
    df <- filtered_he_spells()
    active_n <- if (nrow(df) > 0) sum(df$Status == "Still Active", na.rm = TRUE) else 0
    kpi_value_box(
      scales::comma(active_n),
      "Currently Open Searches",
      icon = shiny::icon("door-open"),
      color = "green"
    )
  })

  output$he_spell_persistent_box <- shiny::renderUI({
    df <- filtered_he_spells()
    persist_n <- if (nrow(df) > 0) sum(df$Status == "Still Active" & df$Estimated_Days_Open >= 90, na.rm = TRUE) else 0
    kpi_value_box(
      scales::comma(persist_n),
      "Persistent Searches (\u226590 Days)",
      icon = shiny::icon("triangle-exclamation"),
      color = "red"
    )
  })

  output$he_spell_hardest_box <- shiny::renderUI({
    df <- filtered_he_spells()
    hardest <- "None"
    if (nrow(df) >= 1 && "Category" %in% names(df)) {
      cat_dur <- df %>%
        dplyr::group_by(Category) %>%
        dplyr::summarize(med = stats::median(Estimated_Days_Open, na.rm = TRUE), n = dplyr::n(), .groups = "drop") %>%
        dplyr::filter(n >= 1) %>%
        dplyr::arrange(dplyr::desc(med))
      if (nrow(cat_dur) > 0) hardest <- cat_dur$Category[1]
    }
    kpi_value_box(
      hardest,
      "Longest Duration Discipline",
      icon = shiny::icon("building-columns"),
      color = "purple"
    )
  })

  output$he_spell_duration_plot <- plotly::renderPlotly({
    df <- filtered_he_spells()
    if (nrow(df) == 0) {
      p <- ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No vacancy spells match the selected filter criteria", size = 4, color = "#666666") +
        ggplot2::theme_void()
      return(plotly::ggplotly(p) %>% plotly::config(displayModeBar = FALSE))
    }

    cat_summary <- df %>%
      dplyr::group_by(Category) %>%
      dplyr::summarize(
        Median_Days = round(stats::median(Estimated_Days_Open, na.rm = TRUE)),
        Postings = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::filter(!is.na(Category) & Category != "" & Postings >= 1) %>%
      dplyr::arrange(dplyr::desc(Median_Days)) %>%
      utils::head(12)

    if (nrow(cat_summary) == 0) {
      p <- ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No discipline data available for selected filter", size = 4, color = "#666666") +
        ggplot2::theme_void()
      return(plotly::ggplotly(p) %>% plotly::config(displayModeBar = FALSE))
    }

    p <- ggplot2::ggplot(cat_summary, ggplot2::aes(
      x = stats::reorder(Category, Median_Days),
      y = Median_Days,
      text = paste0("<b>", Category, "</b><br>Median Open: ", Median_Days, " days<br>Total Spells: ", Postings)
    )) +
      ggplot2::geom_col(fill = "#2a78d6", width = 0.65) +
      ggplot2::coord_flip() +
      ggplot2::labs(x = NULL, y = "Median Days Open") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(size = 9),
        panel.grid.major.y = ggplot2::element_blank()
      )

    plotly::ggplotly(p, tooltip = "text") %>% plotly::config(displayModeBar = FALSE)
  })

  output$he_spell_distrib_plot <- plotly::renderPlotly({
    df <- filtered_he_spells()
    bin_levels <- c("1\u201330 Days", "31\u201360 Days", "61\u201390 Days", "91\u2013180 Days", "180+ Days")
    if (nrow(df) == 0) {
      p <- ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No vacancy spells match the selected filter criteria", size = 4, color = "#666666") +
        ggplot2::theme_void()
      return(plotly::ggplotly(p) %>% plotly::config(displayModeBar = FALSE))
    }

    df <- df %>%
      dplyr::mutate(Duration_Bin = dplyr::case_when(
        Estimated_Days_Open <= 30 ~ "1\u201330 Days",
        Estimated_Days_Open <= 60 ~ "31\u201360 Days",
        Estimated_Days_Open <= 90 ~ "61\u201390 Days",
        Estimated_Days_Open <= 180 ~ "91\u2013180 Days",
        TRUE ~ "180+ Days"
      )) %>%
      dplyr::mutate(Duration_Bin = factor(Duration_Bin, levels = bin_levels))

    bin_counts <- df %>%
      dplyr::count(Duration_Bin, name = "Count") %>%
      tidyr::complete(Duration_Bin = factor(bin_levels, levels = bin_levels), fill = list(Count = 0)) %>%
      dplyr::filter(!is.na(Duration_Bin))

    p <- ggplot2::ggplot(bin_counts, ggplot2::aes(
      x = Duration_Bin, y = Count,
      text = paste0("<b>", Duration_Bin, "</b><br>Spells: ", Count)
    )) +
      ggplot2::geom_col(fill = "#9b59b6", width = 0.6) +
      ggplot2::labs(x = NULL, y = "Number of Vacancies") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 25, hjust = 1, size = 8),
        panel.grid.major.x = ggplot2::element_blank()
      )

    plotly::ggplotly(p, tooltip = "text") %>% plotly::config(displayModeBar = FALSE)
  })

  output$he_spell_table <- DT::renderDT({
    df <- filtered_he_spells()
    cols_to_show <- c("Institution", "Title", "Job_Type", "Category",
                      "First_Seen_Date", "Last_Seen_Date", "Estimated_Days_Open", "Status")
    existing_cols <- intersect(cols_to_show, names(df))

    df_display <- if (nrow(df) > 0) {
      df %>% dplyr::select(dplyr::all_of(existing_cols)) %>% dplyr::arrange(dplyr::desc(Estimated_Days_Open))
    } else {
      df[0, existing_cols, drop = FALSE]
    }

    DT::datatable(
      df_display %>% dplyr::rename(dplyr::any_of(c(`Days listed` = "Estimated_Days_Open", `First seen` = "First_Seen_Date",
                                     `Last seen` = "Last_Seen_Date", Listing = "Status",
                                     Category = "Broad_Category"))),
      options = list(
        pageLength = 15,
        lengthMenu = c(10, 15, 25, 50, 100),
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel")
      ),
      extensions = "Buttons",
      rownames = FALSE,
      escape = FALSE
    )
  })

  list(selected_district = selected_district, selected_institution = selected_institution,
       selected_k12_query = selected_k12_query, selected_he_query = selected_he_query,
       selected_pay_band = selected_pay_band, selected_k12_type = selected_k12_type,
       selected_he_type = selected_he_type, k12_not_marked_only = k12_not_marked_only,
       he_not_marked_only = he_not_marked_only, wow_delta_ui = wow_delta_ui,
       not_marked_line = not_marked_line, statewide_weekly_series = statewide_weekly_series,
       home_not_marked = home_not_marked, home_count_col = home_count_col,
       top_entity_table = top_entity_table, top_job_types_table = top_job_types_table,
       k12_compare_bar_plot = k12_compare_bar_plot, map_filtered = map_filtered,
       navigate_to_entity_jobs = navigate_to_entity_jobs, filtered_k12sum = filtered_k12sum,
       df_windowed = df_windowed, filtered_hesum = filtered_hesum, he_windowed = he_windowed,
       filtered_spells = filtered_spells, filtered_he_spells = filtered_he_spells)
}

#' Names of the app-level objects `dashboard_server_shared()` reads from `data`
#' @export
dashboard_server_data_names <- function() {
  c("ccdata", "combined_map_data", "combineddata", "data_dictionary", "DATA_LOAD_ISSUES",
    "HE_CATEGORY_COLORS_AGG", "HE_CATEGORY_COLORS_DETAIL", "he_faculty_counts", "he_institution_weekly_totals",
    "he_job_duration_spells", "he_jobtype_weekly_totals", "he_new_this_week", "hesum_he", "hesum_he_agg",
    "HIDDEN_COLS", "IPEDS_SALARY_SOURCE_URL", "job_duration_spells", "K12_CATEGORY_COLORS_AGG",
    "K12_CATEGORY_COLORS_DETAIL", "k12_district_weekly_totals", "k12_jobtype_weekly_totals", "k12sum",
    "k12sum_agg", "last_refreshed_date", "NOT_MARKED_SHOWING", "NOT_MARKED_SHOWING_HE", "PAY_BANDS")
}

