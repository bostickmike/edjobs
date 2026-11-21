# The Districts tab's state-specific blocks and the map markers (step 7 slice 3 part 3, 2026-11-21): the District
# Summary table, its source footnote, the compare cards and compare salary plot, and the map marker observer. The two
# states' app.R had the same code apart from their pay and budget columns (one state: a state survey's average and
# 10th-90th percentile range plus the General Fund budget; the other: a base salary and its prior year, the state
# department's average and the superintendent's salary) and those columns' source notes. A `district_spec()` from
# the state now names those columns; everything else is the code both app.R files had.

#' A pay or budget column of the Districts tab
#'
#' One column of the District Summary table, and optionally a row of the compare cards.
#' @param header Column header. With `year_col`, `"%s"` in it takes the year label.
#' @param value Column of `combined_map_data` holding the amount (shown as dollars). Give `value` or `range`.
#' @param range Two columns (low, high), shown as `"$low - $high"` (blank when either is missing).
#' @param year_col Column whose first non-missing K-12 value is the year label.
#' @param year_fallback Label used when `year_col` has no value. Required with `year_col`.
#' @param prior_year TRUE: the label is the school year before `year_col`'s ("2025-2026" gives "2024-2025"; see
#'   `prior_school_year_label()`).
#' @param card Label of this column's row on the compare cards, or NULL for no row.
#' @param card_section Where the card row goes: `"pay"` above Enrollment, `"budget"` below Students / Teacher.
#' @return A list of class `edjobs_district_column`.
#' @export
district_column <- function(header, value = NULL, range = NULL, year_col = NULL, year_fallback = NULL,
                            prior_year = FALSE, card = NULL, card_section = c("pay", "budget")) {
  card_section <- match.arg(card_section)
  if (is.null(value) == is.null(range)) stop("district_column(): give `value` or `range`, not both", call. = FALSE)
  if (!is.null(range) && length(range) != 2) stop("district_column(): `range` names two columns (low, high)", call. = FALSE)
  if (!is.null(year_col)) {
    if (!grepl("%s", header, fixed = TRUE)) stop("district_column(): with `year_col`, `header` needs \"%s\"", call. = FALSE)
    if (is.null(year_fallback)) stop("district_column(): `year_col` needs a `year_fallback`", call. = FALSE)
  }
  if (prior_year && is.null(year_col)) stop("district_column(): `prior_year` needs `year_col`", call. = FALSE)
  structure(list(header = header, value = value, range = range, year_col = year_col, year_fallback = year_fallback,
                 prior_year = prior_year, card = card, card_section = card_section),
            class = "edjobs_district_column")
}

#' A pay line of the map popups
#'
#' Shown on a marker whose `value` is not missing: `"<label>: $value"`, then `" ($low - $high)"` (`range`) or
#' `" ($also<also_label>)"` (`also`) when present, `" &middot; <year>"` when `year_col` has a value, then `suffix`.
#' @param label Line label.
#' @param value Column of `combined_map_data` with the amount.
#' @param range Two columns (low, high) shown after the amount.
#' @param also A second amount's column shown after the amount, with `also_label` after it.
#' @param also_label Text after `also`'s amount (e.g. `" prior year"`).
#' @param year_col Column with the line's year.
#' @param suffix Text at the end of the line.
#' @return A list of class `edjobs_district_popup_line`.
#' @export
district_popup_line <- function(label, value, range = NULL, also = NULL, also_label = "", year_col = NULL, suffix = "") {
  if (!is.null(range) && !is.null(also)) stop("district_popup_line(): give `range` or `also`, not both", call. = FALSE)
  if (!is.null(range) && length(range) != 2) stop("district_popup_line(): `range` names two columns (low, high)", call. = FALSE)
  structure(list(label = label, value = value, range = range, also = also, also_label = also_label,
                 year_col = year_col, suffix = suffix),
            class = "edjobs_district_popup_line")
}

#' A source paragraph of the District Summary footnote
#'
#' `"<lead> <source> <year> — <link> <after>"`, from the first non-missing K-12 values of `source_col` and
#' `year_col`. Left out when the source is missing, or the year is and `require_year` is TRUE.
#' @param lead Opening words ("Salary data:").
#' @param source_col Column naming the source document.
#' @param year_col Column with its year, or NULL.
#' @param year_format `sprintf()` format for the year (`"(%s school year)"`).
#' @param url,url_text The source's page and the link text; NULL for no link.
#' @param after Text after the link (or the year, without one).
#' @param require_year Leave the paragraph out when the year is missing (FALSE: show it without the year).
#' @return A list of class `edjobs_district_source_note`.
#' @export
district_source_note <- function(lead, source_col, year_col = NULL, year_format = "(%s school year)", url = NULL,
                                 url_text = NULL, after = NULL, require_year = TRUE) {
  if (is.null(url) != is.null(url_text)) stop("district_source_note(): give `url` and `url_text` together", call. = FALSE)
  structure(list(lead = lead, source_col = source_col, year_col = year_col, year_format = year_format, url = url,
                 url_text = url_text, after = after, require_year = require_year),
            class = "edjobs_district_source_note")
}

#' A state's Districts tab: pay columns, popup pay lines, compare plot and footnote text
#'
#' @param columns `district_column()`s, in table order (between Students per Teacher and the county figures).
#' @param popup_lines `district_popup_line()`s, in popup order (before the higher-ed salary line).
#' @param compare_bars Named character vector: legend name = column, one bar per district each (at most two).
#' @param compare_y_title The compare plot's y-axis title.
#' @param source_notes `district_source_note()`s, the footnote's first paragraphs. The footnote shows only when K-12
#'   rows have a `Salary_Year` and a `Salary_Source`.
#' @param saipe_scope_note Sentence after the poverty source's link (how district-level the figure is).
#' @param saipe_standin_note The poverty paragraph's last sentence (which schools show a surrounding district's figure).
#' @param other_employers_note A footnote paragraph (a tag) after the lunch paragraph, or NULL.
#' @param page_length Rows per page of the District Summary table.
#' @param shared_vacancy_note Popup remark after a vacancy rate whose `Vacancy_Rate_Shared` is TRUE (a rate reported
#'   for two colleges together), or NULL.
#' @return A list of class `edjobs_district_spec`.
#' @export
district_spec <- function(columns, popup_lines, compare_bars, compare_y_title, source_notes, saipe_scope_note,
                          saipe_standin_note, other_employers_note = NULL, page_length = 25,
                          shared_vacancy_note = NULL) {
  is_all <- function(x, cls) is.list(x) && length(x) > 0 && all(vapply(x, inherits, logical(1), cls))
  if (!is_all(columns, "edjobs_district_column")) stop("district_spec(): `columns` must be district_column()s", call. = FALSE)
  if (!is_all(popup_lines, "edjobs_district_popup_line")) stop("district_spec(): `popup_lines` must be district_popup_line()s", call. = FALSE)
  if (!is_all(source_notes, "edjobs_district_source_note")) stop("district_spec(): `source_notes` must be district_source_note()s", call. = FALSE)
  if (!is.character(compare_bars) || is.null(names(compare_bars)) || !length(compare_bars) || length(compare_bars) > 2) {
    stop("district_spec(): `compare_bars` is a named character vector of one or two columns", call. = FALSE)
  }
  structure(list(columns = columns, popup_lines = popup_lines, compare_bars = compare_bars,
                 compare_y_title = compare_y_title, source_notes = source_notes, saipe_scope_note = saipe_scope_note,
                 saipe_standin_note = saipe_standin_note, other_employers_note = other_employers_note,
                 page_length = page_length, shared_vacancy_note = shared_vacancy_note),
            class = "edjobs_district_spec")
}

# Every column a spec names, for the check against combined_map_data.
district_spec_columns <- function(spec) {
  unique(c(
    unlist(lapply(spec$columns, function(x) c(x$value, x$range, x$year_col))),
    unlist(lapply(spec$popup_lines, function(x) c(x$value, x$range, x$also, x$year_col))),
    unname(spec$compare_bars),
    unlist(lapply(spec$source_notes, function(x) c(x$source_col, x$year_col)))
  ))
}

check_district_spec <- function(spec, combined_map_data) {
  if (!inherits(spec, "edjobs_district_spec")) stop("district_spec must be a district_spec()", call. = FALSE)
  missing <- setdiff(district_spec_columns(spec), names(combined_map_data))
  if (length(missing)) stop("district_spec names columns combined_map_data lacks: ", paste(missing, collapse = ", "), call. = FALSE)
  spec
}

#' The school year before a "2025-2026"-style label
#'
#' @param year_label A school-year label with two four-digit years.
#' @return "2024-2025" for "2025-2026"; NA when the label lacks two four-digit years.
#' @export
prior_school_year_label <- function(year_label) {
  parts <- regmatches(year_label, gregexpr("[0-9]{4}", year_label))[[1]]
  if (length(parts) != 2) return(NA_character_)
  paste0(as.integer(parts[1]) - 1, "-", as.integer(parts[2]) - 1)
}

first_value <- function(x) {
  x <- unique(stats::na.omit(x))
  if (length(x)) x[1] else NULL
}

district_column_header <- function(col, k12_rows) {
  if (is.null(col$year_col)) return(col$header)
  year <- first_value(k12_rows[[col$year_col]])
  label <- if (is.null(year)) col$year_fallback else if (col$prior_year) prior_school_year_label(year) else year
  sprintf(col$header, label)
}

dollar_range <- function(low, high) {
  ifelse(is.na(low) | is.na(high), NA_character_, paste0(scales::dollar(low), " - ", scales::dollar(high)))
}

district_column_values <- function(col, rows) {
  if (!is.null(col$range)) dollar_range(rows[[col$range[1]]], rows[[col$range[2]]]) else rows[[col$value]]
}

#' The District Summary table's data
#'
#' @param combined_map_data The map data (K-12 rows are `Type == "K-12 District"`).
#' @param spec A `district_spec()`.
#' @return A data frame, attribute `currency_cols`: the columns shown as dollars.
#' @export
district_summary_df <- function(combined_map_data, spec) {
  k12_rows <- combined_map_data %>% filter(Type == "K-12 District") %>% arrange(desc(CurrentCount))
  front <- k12_rows %>%
    dplyr::transmute(
      District = Name,
      County,
      `Current Openings` = CurrentCount,
      # Real, sortable/filterable column, so "why does this district look quiet" is answerable from the exportable
      # table, not just the map. "Full" is every district on a real job-board platform; the Map tab's helpText says
      # what the partial tiers mean.
      `Posting Data` = Data_Coverage,
      `New This Week` = WeeklyNew,
      `Teacher Vacancy Rate` = Vacancy_Rate,
      Enrollment,
      `Students per Teacher` = round(Students_Per_Teacher, 1)
    )
  pay <- stats::setNames(lapply(spec$columns, district_column_values, rows = k12_rows),
                         vapply(spec$columns, district_column_header, character(1), k12_rows = k12_rows))
  back <- k12_rows %>%
    dplyr::transmute(
      # County-level context (Census ACS 5-Year), beside the pay columns: "$46k pay in a county where median rent
      # is $650/mo" is the comparison a prospective teacher makes.
      `County Median Income` = Median_Household_Income,
      `County Median Rent (per month)` = Median_Gross_Rent,
      `County Mining/Energy Jobs` = Mining_Employment_Share,
      `County Figures: 90% Margin of Error` = county_moe_summary(Median_Household_Income_MOE, Median_Gross_Rent_MOE,
                                                                 Mining_Employment_Share, Mining_Employment_Share_MOE),
      `County Population Trend (5yr)` = Population_Change_Pct,
      # Numbers, formatted by DT, so the column sorts as numbers (the table sorts on the server, in R, by the
      # underlying value); where the rate is a stand-in (a school with no SAIPE geography of its own) the next
      # column says so.
      `School-Age Children in Poverty (Census est.)` = Child_Poverty_Rate,
      `Approx. 90% Range` = format_poverty_range(Child_Poverty_Low, Child_Poverty_High),
      `Poverty Estimate Basis` = ifelse(is.na(Child_Poverty_Rate), NA_character_,
                                        ifelse(is.na(Child_Poverty_Basis), "District", "Surrounding district")),
      # School-based count beside the Census estimate (CCD school directory).
      `Free/Reduced Lunch` = Free_Reduced_Lunch_Share,
      `Lunch Data Year` = ifelse(is.na(Free_Reduced_Lunch_Share), NA_character_, FRL_Year)
    )
  if (anyDuplicated(names(pay))) stop("district_spec(): column headers repeat: ", paste(names(pay)[duplicated(names(pay))], collapse = ", "), call. = FALSE)
  df <- dplyr::bind_cols(front, dplyr::as_tibble(pay), back)
  df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
  pay_currency <- names(pay)[vapply(spec$columns, function(x) is.null(x$range), logical(1))]
  attr(df, "currency_cols") <- names(df)[names(df) %in% c(pay_currency, "County Median Income", "County Median Rent (per month)")]
  df
}

#' The District Summary footnote
#'
#' @inheritParams district_summary_df
#' @return A `tagList`, or NULL when K-12 rows have no `Salary_Year` or `Salary_Source`.
#' @export
district_summary_footnote <- function(combined_map_data, spec) {
  k12 <- combined_map_data[combined_map_data$Type == "K-12 District", , drop = FALSE]
  if (is.null(first_value(k12$Salary_Year)) || is.null(first_value(k12$Salary_Source))) return(NULL)
  acs_year <- first_value(k12$ACS_Year)
  saipe_year <- first_value(k12$SAIPE_Year)
  source_notes <- lapply(spec$source_notes, function(n) {
    source <- first_value(k12[[n$source_col]])
    year <- if (is.null(n$year_col)) NULL else first_value(k12[[n$year_col]])
    if (is.null(source) || (n$require_year && !is.null(n$year_col) && is.null(year))) return(NULL)
    year_part <- if (is.null(n$year_col)) NULL else if (is.null(year)) "" else sprintf(n$year_format, year)
    link <- if (is.null(n$url)) NULL else list("—", htmltools::tags$a(href = n$url, target = "_blank", n$url_text))
    do.call(shiny::helpText, c(list(n$lead, source), as.list(year_part), link, as.list(n$after)))
  })
  do.call(htmltools::tagList, c(source_notes, list(
    if (!is.null(acs_year)) {
      shiny::helpText(
        "County context (income, rent, mining/energy employment share, population trend): US Census Bureau, American Community Survey 5-Year Estimates",
        paste0("(", acs_year, ")"), "—",
        htmltools::tags$a(href = "https://www.census.gov/data/developers/data-sets/acs-5year.html", target = "_blank", "census.gov"),
        ". County-level, not district-level -- sibling districts in the same county share the same figures.",
        "± values are the Census Bureau's 90% margins of error (the mining share's from Census's formula for a",
        "proportion); a share whose margin is at least as large as the share is marked \"not distinguishable from 0\".",
        "County population totals are set to Census's population estimates, so the 5-year trend has no margin of error."
      )
    },
    shiny::helpText(
      "Free/reduced lunch: share of students eligible for free or reduced-price lunch, NCES Common Core of Data",
      "school directory (Lunch Data Year: that district's newest reporting year) --",
      htmltools::tags$a(href = "https://educationdata.urban.org/documentation/", target = "_blank", "educationdata.urban.org"),
      ". A school-based count, not a poverty estimate: schools serving free meals to all (community eligibility)",
      "may count every student; a share of 100% is marked \"every student counted (likely free meals for all)\".",
      "Left blank where schools holding under 80% of a district's students reported."
    ),
    spec$other_employers_note,
    if (!is.null(saipe_year)) {
      shiny::helpText(
        "School-age children in poverty: US Census Bureau, Small Area Income and Poverty Estimates (SAIPE)",
        paste0("(", saipe_year, ")"), "—",
        htmltools::tags$a(href = "https://www.census.gov/programs-surveys/saipe/data/datasets.html", target = "_blank", "census.gov/saipe"),
        spec$saipe_scope_note,
        # Census: "A true poverty \"rate\" for children cannot be determined from these figures, because the
        # numerator and denominator refer to slightly different universes" (SAIPE school district overview, revised
        # 2026-01-12; Mike chose this label 2026-11-15).
        "Census's estimate of related children 5-17 in families in poverty, divided by all children 5-17 in the district.",
        "Census notes this is not a true poverty rate: foster children, other children not related to the householder and",
        "children in group quarters are counted in the second figure only.",
        # "Quantifying Relative Error in the School District Estimates" (census.gov, revised 2025-12-17).
        "The range beside each estimate is Census's approximate 90% range for a district of that size: Census publishes",
        "no margin of error for one district, only median relative errors by district size, which it calls an approximate",
        "upper bound. Under 2,500 people an estimate can be off by about 110% of itself; at 65,000 or more, by about 25%.",
        spec$saipe_standin_note
      )
    }
  )))
}

# One compare card's rows: label, value text, and whether the value is a long range (smaller type).
district_card_rows <- function(row, spec) {
  card_value <- function(col) {
    if (!is.null(col$range)) {
      lo <- row[[col$range[1]]][1]; hi <- row[[col$range[2]]][1]
      if (!is.na(lo) && !is.na(hi)) paste0(scales::dollar(lo), " - ", scales::dollar(hi)) else "N/A"
    } else {
      v <- row[[col$value]][1]
      if (!is.na(v)) scales::dollar(v) else "N/A"
    }
  }
  spec_rows <- function(section) {
    cols <- Filter(function(x) !is.null(x$card) && x$card_section == section, spec$columns)
    lapply(cols, function(x) list(label = x$card, value = card_value(x), small = !is.null(x$range)))
  }
  enroll <- if (!is.na(row$Enrollment[1])) scales::comma(row$Enrollment[1]) else "N/A"
  spt <- if (!is.na(row$Students_Per_Teacher[1])) sprintf("%.1f", row$Students_Per_Teacher[1]) else "N/A"
  rent <- if (!is.na(row$Median_Gross_Rent[1])) paste0(scales::dollar(row$Median_Gross_Rent[1]), "/mo", format_moe_dollars(row$Median_Gross_Rent_MOE[1])) else "N/A"
  poverty <- if (!is.na(row$Child_Poverty_Rate[1])) scales::percent(row$Child_Poverty_Rate[1], accuracy = 0.1) else "N/A"
  if (!is.na(row$Child_Poverty_Rate[1]) && !is.na(row$Child_Poverty_Basis[1])) poverty <- paste(poverty, "(surrounding district)")
  poverty_range <- format_poverty_range(row$Child_Poverty_Low[1], row$Child_Poverty_High[1])
  if (!is.na(row$Child_Poverty_Rate[1]) && !is.na(poverty_range)) poverty <- paste0(poverty, " (", poverty_range, ")")
  frl <- if (!is.na(row$Free_Reduced_Lunch_Share[1])) paste0(scales::percent(row$Free_Reduced_Lunch_Share[1], accuracy = 1), " (", row$FRL_Year[1], ")") else "N/A"
  if (!is.na(row$Free_Reduced_Lunch_Share[1]) && row$Free_Reduced_Lunch_Share[1] >= 0.995) frl <- paste(frl, "-- every student counted (likely free meals for all)")
  plain <- function(label, value) list(label = label, value = value, small = FALSE)
  c(spec_rows("pay"),
    list(plain("Enrollment:", enroll), plain("Students / Teacher:", spt)),
    spec_rows("budget"),
    list(plain("County Median Rent:", rent), plain("Children in Poverty (Census est.):", poverty),
         plain("Free/Reduced Lunch:", frl)))
}

#' The district compare cards
#'
#' @param selected Selected district names (one to four).
#' @inheritParams district_summary_df
#' @return A `fluidRow` of cards, or an info box when none is selected.
#' @export
district_compare_cards <- function(selected, combined_map_data, spec) {
  tags <- htmltools::tags
  div <- htmltools::div
  if (is.null(selected) || length(selected) == 0) {
    return(div(class = "alert alert-info", style = "margin-top: 15px;",
               shiny::icon("circle-info"), " Please select one to four school districts above to display comparative benchmarks."))
  }
  col_width <- dplyr::case_when(
    length(selected) == 1 ~ 12,
    length(selected) == 2 ~ 6,
    length(selected) == 3 ~ 4,
    TRUE ~ 3
  )
  k12_rows <- combined_map_data %>% filter(Type == "K-12 District", Name %in% selected)
  cards <- lapply(selected, function(dname) {
    row <- k12_rows %>% filter(Name == dname)
    if (nrow(row) == 0) return(NULL)
    vac_rate <- if (!is.na(row$Vacancy_Rate[1])) scales::percent(row$Vacancy_Rate[1], accuracy = 0.1) else "N/A"
    table_rows <- lapply(district_card_rows(row, spec), function(r) {
      tags$tr(tags$td(class = "text-muted", r$label),
              tags$td(style = if (r$small) "text-align: right; font-weight: 600; font-size: 11.5px;" else "text-align: right; font-weight: 600;",
                      r$value))
    })
    shiny::column(
      width = col_width,
      div(
        class = "box box-solid box-primary",
        style = "border-top: 3px solid #2a78d6; box-shadow: 0 1px 3px rgba(0,0,0,0.08); margin-bottom: 20px;",
        div(
          class = "box-header with-border", style = "background: #f7f9fa; padding: 10px 15px;",
          htmltools::h4(style = "margin: 0; font-size: 15px; font-weight: 600; color: #222;", dname),
          tags$small(class = "text-muted", paste0(row$County[1], " County"))
        ),
        div(
          class = "box-body", style = "padding: 15px;",
          shiny::fluidRow(
            shiny::column(6, div(style = "text-align: center; border-right: 1px solid #eee;",
                                 div(style = "font-size: 22px; font-weight: 700; color: #2a78d6;", row$CurrentCount[1]),
                                 div(style = "font-size: 11px; text-transform: uppercase; color: #777;", "Active Openings"))),
            shiny::column(6, div(style = "text-align: center;",
                                 div(style = "font-size: 22px; font-weight: 700; color: #e67e22;", vac_rate),
                                 div(style = "font-size: 11px; text-transform: uppercase; color: #777;", "Vacancy Rate")))
          ),
          tags$hr(style = "margin: 12px 0;"),
          do.call(tags$table, c(list(style = "width: 100%; font-size: 13px; line-height: 1.8;"), table_rows))
        )
      )
    )
  })
  shiny::fluidRow(cards)
}

#' The district compare salary plot
#'
#' @inheritParams district_compare_cards
#' @return A plotly bar chart (grouped, one bar per `spec$compare_bars` column), or an empty plot with a prompt.
#' @export
district_compare_salary_plot <- function(selected, combined_map_data, spec) {
  if (is.null(selected) || length(selected) == 0) {
    return(plotly::plot_ly(type = "scatter", mode = "none") %>% plotly::layout(
      annotations = list(list(text = "Select one or more districts to display salary comparison",
                              showarrow = FALSE, font = list(size = 14, color = "#666"))),
      xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)
    ))
  }
  bars <- spec$compare_bars
  df <- combined_map_data %>%
    filter(Type == "K-12 District", Name %in% selected) %>%
    dplyr::select(dplyr::all_of(c("Name", "Enrollment", unname(bars)))) %>%
    # Same district order as the enrollment and openings charts.
    arrange(desc(coalesce(Enrollment, -1)), Name) %>%
    dplyr::mutate(Name = factor(Name, levels = Name))
  colors <- c("#4a90e2", "#50e3c2")
  p <- plotly::plot_ly(df, x = ~Name)
  for (i in seq_along(bars)) {
    p <- plotly::add_bars(p, y = stats::as.formula(paste0("~`", bars[[i]], "`")), name = names(bars)[i],
                          marker = list(color = colors[i]))
  }
  p %>% plotly::layout(
    barmode = "group",
    xaxis = list(title = "", tickangle = -15),
    yaxis = list(title = spec$compare_y_title, tickformat = "$,.0f"),
    legend = list(orientation = "h", x = 0, y = 1.15),
    margin = list(b = 60, l = 60, r = 20, t = 40)
  )
}

# The popup pay lines for every row of `df` (empty strings where a line's amount is missing).
district_popup_pay_html <- function(df, spec) {
  out <- rep("", nrow(df))
  for (l in spec$popup_lines) {
    v <- df[[l$value]]
    detail <- if (!is.null(l$range)) {
      lo <- df[[l$range[1]]]; hi <- df[[l$range[2]]]
      ifelse(!is.na(lo) & !is.na(hi), paste0(" (", scales::dollar(lo), " - ", scales::dollar(hi), ")"), "")
    } else if (!is.null(l$also)) {
      a <- df[[l$also]]
      ifelse(!is.na(a), paste0(" (", scales::dollar(a), l$also_label, ")"), "")
    } else ""
    year <- if (is.null(l$year_col)) "" else ifelse(!is.na(df[[l$year_col]]), paste0(" &middot; ", df[[l$year_col]]), "")
    out <- paste0(out, ifelse(!is.na(v), paste0("<div>", l$label, ": ", scales::dollar(v), detail, year, l$suffix, "</div>"), ""))
  }
  out
}

# The map markers' hover labels and popups (the map observe() both app.R files had; pay lines from the spec).
# Optional columns: Vacancy_Rate_Shared (with the spec's shared_vacancy_note) and Salary_Note.
district_map_labels <- function(df) {
  with(df, sprintf(
    "<strong>%s</strong><br/>%s%s%s",
    Name,
    ifelse(SearchActive,
           sprintf("<span style='color:#198754;font-weight:600;'>\U0001F3AF %d matching opening%s for \"%s\"</span><br/>",
                   DisplayCount, ifelse(DisplayCount == 1, "", "s"), SearchQuery),
           sprintf("%d active opening%s<br/>", CurrentCount, ifelse(CurrentCount == 1, "", "s"))),
    ifelse(!is.na(Vacancy_Rate), sprintf("Vacancy rate: %.1f%%<br/>", Vacancy_Rate * 100), ""),
    "<span style='color:#666;font-size:11px;'>Click marker for details & jobs</span>"
  )) %>% lapply(htmltools::HTML)
}

district_map_popups <- function(df, spec) {
  shared_rate <- if ("Vacancy_Rate_Shared" %in% names(df) && !is.null(spec$shared_vacancy_note)) {
    ifelse(df$Vacancy_Rate_Shared %in% TRUE, paste0(" <span style='color:#666;'>(", spec$shared_vacancy_note, ")</span>"), "")
  } else ""
  salary_note <- if ("Salary_Note" %in% names(df)) {
    ifelse(!is.na(df$Salary_Note), paste0("<div style='font-size:0.85em;color:#666;'>", df$Salary_Note, "</div>"), "")
  } else ""
  pay_lines <- district_popup_pay_html(df, spec)
  with(df, paste0(
    "<div><strong>", Name, "</strong><br/>", ifelse(!is.na(Campus_Of), "Higher Ed campus or site", Type), "</div>",
    ifelse(SearchActive,
           paste0("<div style='margin:4px 0;background:#e8f5e9;color:#2e7d32;padding:3px 8px;border-radius:4px;font-weight:600;font-size:0.9em;'>",
                  "\U0001F3AF ", DisplayCount, " matching opening", ifelse(DisplayCount == 1, "", "s"),
                  " for &ldquo;", SearchQuery, "&rdquo; (out of ", CurrentCount, " total)</div>",
                  ifelse(nzchar(MatchingSample), paste0("<div style='margin-bottom:4px;font-size:0.85em;'><strong>Matching:</strong> ", MatchingSample, "</div>"), "")),
           ""),
    # Visible badge: Data_Coverage is "Full" for every district on a real structured job-board platform; the
    # partial tiers come from the state's coverage-tier code. HE institutions carry NA (guarded: NA != "Full" is
    # NA, which would print a literal "NA" badge).
    ifelse(!SearchActive & !is.na(Data_Coverage) & Data_Coverage != "Full",
           paste0("<div style='margin:2px 0;'><span style='background:#fdecea;color:#a92f1e;",
                  "font-size:0.78em;font-weight:bold;padding:1px 6px;border-radius:8px;'>",
                  Data_Coverage, "</span></div>"),
           ""),
    "<div>Current openings: <strong>", CurrentCount, "</strong></div>",
    ifelse(!is.na(Campus_Note), paste0("<div style='font-size:0.85em;color:#666;'>", Campus_Note, "</div>"), ""),
    "<div>New this week: ", WeeklyNew, "</div>",
    ifelse(!is.na(Vacancy_Rate),
           paste0("<div>", ifelse(Type == "K-12 District", "Teacher", "Faculty"),
                  " vacancy rate: ", scales::percent(Vacancy_Rate, accuracy = 0.1), shared_rate, "</div>"),
           ""),
    ifelse(!SearchActive & nzchar(SampleTitles), paste0("<div>Recent postings: ", SampleTitles, "</div>"), ""),
    ifelse(!is.na(Students_Per_Teacher),
           paste0("<div>Students per ", ifelse(Type == "K-12 District", "teacher", "faculty member"), ": ",
                  sprintf("%.1f", Students_Per_Teacher),
                  " (", format(Enrollment, big.mark = ","), " students)</div>"),
           ""),
    ifelse(!is.na(Enrollment_Change_Pct),
           paste0("<div>Enrollment trend (5yr): ", ifelse(Enrollment_Change_Pct >= 0, "+", ""),
                  scales::percent(Enrollment_Change_Pct, accuracy = 0.1), "</div>"),
           ""),
    ifelse(!is.na(Pell_Recipient_Share),
           paste0("<div>Pell Grant recipients: ", scales::percent(Pell_Recipient_Share, accuracy = 0.1),
                  " of students (", Pell_Year, ")</div>"),
           ""),
    pay_lines,
    ifelse(!is.na(Faculty_Avg_Salary),
           paste0("<div>Avg. salary, full-time instructional staff (IPEDS, 9–12-month contracts): ", scales::dollar(Faculty_Avg_Salary),
                  ifelse(!is.na(Faculty_Avg_Salary_Professor),
                         paste0(" (Professor rank: ", scales::dollar(Faculty_Avg_Salary_Professor), ")"),
                         ""),
                  " &middot; ", Salary_Year, "</div>"),
           ""),
    salary_note,
    ifelse(!is.na(Salary_Source),
           paste0("<div style='font-size:0.85em;color:#666;'>Salary source: ", Salary_Source, "</div>"),
           ""),
    ifelse(!is.na(Median_Household_Income),
           paste0("<div style='font-size:0.85em;color:#666;margin-top:4px;'>County: median income ",
                  scales::dollar(Median_Household_Income), format_moe_dollars(Median_Household_Income_MOE),
                  ", median rent ", scales::dollar(Median_Gross_Rent), "/mo", format_moe_dollars(Median_Gross_Rent_MOE),
                  ifelse(!is.na(Mining_Employment_Share) & Mining_Employment_Share >= 0.05,
                         paste0(", ", scales::percent(Mining_Employment_Share, accuracy = 1), format_moe_points(Mining_Employment_Share_MOE),
                                " of jobs in mining/energy"),
                         ""),
                  "</div>"),
           ""),
    ifelse(!is.na(Child_Poverty_Rate),
           paste0("<div style='font-size:0.85em;color:#666;'>",
                  ifelse(is.na(Child_Poverty_Basis), "District: ", ""),
                  scales::percent(Child_Poverty_Rate, accuracy = 0.1), " of school-age children in poverty (Census est.",
                  ifelse(is.na(Child_Poverty_Low) | is.na(Child_Poverty_High), "",
                         paste0("; ", format_poverty_range(Child_Poverty_Low, Child_Poverty_High))), ")",
                  ifelse(is.na(Child_Poverty_Basis), "", paste0(" (", sub("^Surrounding", "surrounding", Child_Poverty_Basis), ")")),
                  "</div>"),
           ""),
    ifelse(!is.na(Free_Reduced_Lunch_Share),
           paste0("<div style='font-size:0.85em;color:#666;'>",
                  scales::percent(Free_Reduced_Lunch_Share, accuracy = 1),
                  " of students eligible for free/reduced-price lunch (", FRL_Year, ")",
                  ifelse(Free_Reduced_Lunch_Share >= 0.995, " -- every student counted (likely free meals for all)", ""),
                  "</div>"),
           ""),
    ifelse(!is.na(School_Pay_County),
           paste0("<div style='font-size:0.85em;color:#666;'>", sub(",.*$", "", County),
                  " public school employees averaged ", scales::dollar(School_Pay_County, accuracy = 1),
                  " in ", School_Pay_Year, " (all roles; BLS QCEW)</div>"),
           ""),
    "<div style='margin-top:6px;'>",
    "<a href='", Link, "' target='_blank'>Careers page</a>",
    " &nbsp;|&nbsp; ",
    # Carries the active map search with it, so the Jobs tab opens on just the postings this popup counted.
    # URL-encoded so quotes in the query can't break out of the JS string.
    "<a href='#' onclick=\"Shiny.setInputValue('map_popup_view_jobs', {entity: '",
    gsub("'", "&#39;", Name, fixed = TRUE),
    "', query: '",
    ifelse(SearchActive,
           vapply(coalesce(SearchKeyword, ""), utils::URLencode, character(1), reserved = TRUE, USE.NAMES = FALSE),
           ""),
    "', band: '", ifelse(SearchActive, SearchBand, ""),
    "'}, {priority: 'event'}); return false;\">",
    ifelse(SearchActive,
           paste0("View ", DisplayCount, " matching job", ifelse(DisplayCount == 1, "", "s")),
           "View all jobs"),
    " &rarr;</a>",
    "</div>"
  ))
}

dashboard_server_districts <- function(input, output, session, combined_map_data, qcew_school_pay,
                                       vacancy_rate_palette, map_filtered, district_spec) {
  output$k12_summary_table <- DT::renderDT({
    df <- district_summary_df(combined_map_data, district_spec)
    # Numeric columns stay numbers so the server-side sort is numeric (2026-11-09 review: "9%" sorted after "92%");
    # DT formats them for display.
    DT::datatable(df, filter = "top", extensions = "Buttons",
                  options = list(scrollX = TRUE, dom = "Bfrtip", buttons = c("copy", "csv", "print"),
                                 pageLength = district_spec$page_length)) %>%
      DT::formatPercentage(intersect(names(df), c("Teacher Vacancy Rate", "County Mining/Energy Jobs", "County Population Trend (5yr)",
                                                  "School-Age Children in Poverty (Census est.)")), digits = 1) %>%
      DT::formatPercentage("Free/Reduced Lunch", digits = 0) %>%
      DT::formatCurrency(attr(df, "currency_cols"), digits = 0) %>%
      DT::formatRound("Students per Teacher", digits = 1)
  })

  output$k12_summary_footnote <- shiny::renderUI({
    note <- district_summary_footnote(combined_map_data, district_spec)
    shiny::req(note)
    note
  })

  output$k12_compare_cards <- shiny::renderUI({
    district_compare_cards(input$k12_compare_districts, combined_map_data, district_spec)
  })

  output$k12_compare_salary_plot <- plotly::renderPlotly({
    district_compare_salary_plot(input$k12_compare_districts, combined_map_data, district_spec)
  })

  shiny::observe({
    df <- map_filtered()
    # Clear unconditionally so unchecking every map-type box (or a search with no matches) empties the map, then
    # stop before building popups for a 0-row frame.
    proxy <- leaflet::leafletProxy("combined_map", data = df) %>% leaflet::clearMarkers()
    qcew_row <- match(sub(",.*$", "", trimws(as.character(df$County))), qcew_school_pay$County)
    df$School_Pay_County <- ifelse(df$Type == "K-12 District", qcew_school_pay$Avg_Annual_Pay[qcew_row], NA_real_)
    df$School_Pay_Year <- qcew_school_pay$Year[qcew_row]
    if (nrow(df) == 0) return()
    # A marker click only opens its popup (leaflet's default); its "View all jobs" link fires a Shiny input, so
    # leaving the map is a second, deliberate action.
    proxy %>%
      leaflet::addCircleMarkers(
        lng = ~Longitude, lat = ~Latitude,
        radius = ~map_marker_radius(DisplayCount),
        fillColor = ~vacancy_rate_palette(Vacancy_Rate),
        color = "#333333", weight = 1,
        fillOpacity = 0.85,
        layerId = ~Name,
        label = district_map_labels(df),
        labelOptions = leaflet::labelOptions(
          style = list("font-weight" = "normal", padding = "4px 8px"),
          textsize = "12px",
          direction = "auto"
        ),
        popup = district_map_popups(df, district_spec)
      )
  })
}
