# Plain-language explanations and HTML builders for the Shiny UI.

# Build a plain-English explanation of the modelling approach used for the
# current forecast run so non-technical users can understand what happened.
build_modelling_explanation <- function(
  ts_data,
  period_type,
  holiday_country = NULL
) {
  n_periods <- nrow(ts_data)
  include_public_holidays <- !is.null(holiday_country) &&
    !identical(holiday_country, "")
  model_profile <- forecast_model_profile(period_type, n_periods)
  model_names <- model_profile$model_names
  seasonal_variables <- model_profile$seasonal_variables

  variables <- c(
    "the past crime counts",
    "the passage of time so the models can pick up any general upward or downward trend",
    seasonal_variables
  )

  if (include_public_holidays) {
    variables <- c(
      variables,
      "the number of public holidays falling in each period"
    )
  }

  model_sentence <- str_glue(
    "For this dataset, the ensemble combines {oxford_comma(model_names)}."
  )

  variable_sentence <- str_glue(
    "These models are based on {oxford_comma(variables)}."
  )

  holiday_sentence <- if (include_public_holidays) {
    " Public holidays were included because they can affect crime levels in some periods."
  } else {
    ""
  }

  str_glue(
    "<p>These forecasts were produced using a statistical technique called an ",
    "<em>ensemble model</em>, in which multiple different forecasting models ",
    "are generated and then the final forecast is produced by combining the ",
    "forecasts produced by the different models. Testing has shown this ",
    "approach usually produces more-accurate forecasts than relying on any ",
    "single model, because it allows the strengths of different models to be ",
    "combined and the weaknesses of individual models to be balanced out.</p>",
    "<p>In this case, the ensemble combined the results of ",
    "{length(model_names)} models, each of which identifies trends and ",
    "patterns in the historical crime counts that can be projected into the ",
    "future. {model_sentence} {variable_sentence}{holiday_sentence}</p>",
    "<p><strong>It is very important to remember that these forecasts are ",
    "based on the assumption that the factors that drove this type of crime ",
    "in this area in the past will continue to be the factors that drive ",
    "crime in this area in the future</strong>. If the factors driving crime ",
    "in this area change, for example because of a change in policing tactics ",
    "or a major change in the local environment (e.g. a new housing ",
    "development being built) it is possible that the frequency of crime will ",
    "change.</p>"
  )
}


# Build the Step 5 HTML that explains how the full forecast window compares
# with the most recent matching span from the uploaded historical data.
build_forecast_comparison_html <- function(comparison, period_type) {
  if (!isTRUE(comparison$available)) {
    return(
      bootstrap_alert(
        "danger",
        htmltools::p(comparison$reason, class = "mb-0")
      )
    )
  }

  # Format comparison probabilities as whole percentages for easier reading,
  # while avoiding a misleading impression of certainty at either extreme.
  format_comparison_probability <- function(probability) {
    if (is.na(probability) || !is.finite(probability)) {
      return("less than 1%")
    }

    if (probability < 0.005) {
      return("less than 1%")
    }

    if (probability >= 0.995) {
      return("more than 99%")
    }

    sprintf("%s%%", round(probability * 100))
  }

  # Format crime-total cutoffs for the Step 5 cards in plain language.
  format_crime_total <- function(value) {
    scales::comma(round(value, 1))
  }

  make_probability_card <- function(
    title,
    probability,
    description,
    border_colour,
    fill_colour
  ) {
    htmltools::div(
      style = paste(
        "border-top:",
        sprintf("4px solid %s;", border_colour),
        "background-color:",
        fill_colour,
        "; border-radius: 0.75rem;",
        "padding: 1rem;"
      ),
      htmltools::p(
        title,
        style = "margin-bottom: 0.25rem; font-weight: 700;"
      ),
      htmltools::p(
        format_comparison_probability(probability),
        style = "margin-bottom: 0; font-size: 1.6rem; font-weight: 700;"
      ),
      htmltools::p(
        description,
        style = "margin-bottom: 0; margin-top: 0.35rem; font-size: 0.9rem; color: #5b6470;"
      )
    )
  }

  config <- period_config(period_type)

  htmltools::tagList(
    htmltools::p(
      sprintf(
        paste(
          "These comparisons summarise the chance that the total number of crimes",
          "across the next %s %s will be higher, lower, or about the same as the",
          "total from the most recent %s %s in the uploaded data."
        ),
        scales::comma(comparison$forecast_periods),
        config$plural,
        scales::comma(comparison$forecast_periods),
        config$plural
      )
    ),
    htmltools::p(
      sprintf(
        paste(
          "The number of crimes in the forecast is considered \"about the same\" as the number of crimes in the comparison period if the range of forecast crimes is within the larger of %s or %s crimes",
          "of the historical comparison total.",
          "The recent comparison window runs from %s, and the forecast window",
          "runs from %s."
        ),
        scales::percent(comparison$same_threshold),
        scales::comma(comparison$same_minimum_crimes),
        format_display_date_range(
          comparison$comparison_period_start,
          comparison$comparison_period_end
        ),
        format_display_date_range(
          comparison$forecast_period_start,
          comparison$forecast_period_end
        )
      )
    ),
    bslib::layout_columns(
      make_probability_card(
        title = "Chance of being higher",
        probability = comparison$probability_higher,
        description = sprintf(
          "%s or more crimes from %s",
          format_crime_total(comparison$upper_same_cutoff),
          format_display_date_range(
            comparison$forecast_period_start,
            comparison$forecast_period_end
          )
        ),
        border_colour = "#b42318",
        fill_colour = "#fff5f5"
      ),
      make_probability_card(
        title = "Chance of being about the same",
        probability = comparison$probability_same,
        description = sprintf(
          "%s to %s crimes from %s",
          format_crime_total(comparison$lower_same_cutoff),
          format_crime_total(comparison$upper_same_cutoff),
          format_display_date_range(
            comparison$forecast_period_start,
            comparison$forecast_period_end
          )
        ),
        border_colour = "#1d4ed8",
        fill_colour = "#eff6ff"
      ),
      make_probability_card(
        title = "Chance of being lower",
        probability = comparison$probability_lower,
        description = sprintf(
          "%s or fewer crimes from %s",
          format_crime_total(comparison$lower_same_cutoff),
          format_display_date_range(
            comparison$forecast_period_start,
            comparison$forecast_period_end
          )
        ),
        border_colour = "#047857",
        fill_colour = "#ecfdf5"
      ),
      col_widths = c(4, 4, 4)
    ),
    bootstrap_alert(
      "info",
      htmltools::HTML(
        sprintf(
          paste(
            "<p>The most recent %s %s in the uploaded data contain %s crimes in total.</p>",
            "<p class='mb-0'>The most-likely forecast number of crimes across the next %s %s is %s crimes in total.</p>"
          ),
          scales::comma(comparison$forecast_periods),
          config$plural,
          scales::comma(round(comparison$comparison_total, 1)),
          scales::comma(comparison$forecast_periods),
          config$plural,
          scales::comma(round(comparison$point_forecast_total, 1))
        )
      )
    )
  )
}


# Explain why Step 5 comparison cards are hidden when Step 2 found forecast
# reliability warnings that make those comparisons too uncertain to trust.
build_step5_unavailable_html <- function(reliability) {
  warnings <- reliability$warnings %||% character()

  warning_list <- if (length(warnings) > 0) {
    htmltools::tags$ul(
      class = "mb-3",
      lapply(warnings, function(warning) {
        htmltools::tags$li(warning)
      })
    )
  } else {
    NULL
  }

  bootstrap_alert(
    "danger",
    htmltools::p(
      "Step 5 comparisons are not shown because the current forecasts are too uncertain for those comparisons to be reliable."
    ),
    warning_list,
    htmltools::p(
      "Step 2 identified reliability warnings in this forecast run. When those warnings are present, the app cannot reliably judge whether the forecast total is higher, lower, or about the same as recent history.",
      class = "mb-0"
    )
  )
}


# Build the HTML shown in Step 2, including data summaries, aggregation notes,
# partial-period notes, and warning boxes when reliability concerns are present.
build_reliability_html <- function(
  ts_data,
  reliability,
  period_type,
  horizon,
  forecast_tbl,
  prep_metadata = NULL
) {
  config <- period_config(period_type)
  summary <- reliability$summary

  intro <- sprintf(
    "<p>The uploaded series contains %s %s observations covering %s, with an average of %.1f crimes per %s.</p>",
    scales::comma(summary$n_periods),
    config$adjective,
    describe_series_range(ts_data, period_type),
    summary$mean_count,
    config$singular
  )

  fit_line <- if (!is.na(reliability$mape)) {
    if (is.finite(reliability$mape)) {
      sprintf(
        "<p>When the model is applied to the historical data, the mean absolute percentage error (MAPE) is %.1f%%. You should decide whether forecasts that are typically wrong by about %.1f%% are acceptable in your circumstances.</p>",
        reliability$mape,
        reliability$mape
      )
    } else {
      paste(
        "<p>When the model is applied to the historical data, the mean absolute percentage error (MAPE) is not finite.",
        "This usually happens when the historical series contains very low counts or many zero-count periods,",
        "so percentage-based accuracy measures become unstable. In this situation, you should treat the forecasts",
        "as especially uncertain and rely more on the other warnings shown here.</p>"
      )
    }
  } else {
    "<p>Model fit metrics were not available.</p>"
  }

  horizon_line <- sprintf(
    "<p>%s %s forecasts were generated, covering %s.</p>",
    scales::comma(horizon),
    config$adjective,
    format_display_date_range(
      forecast_tbl$period_start[[1]],
      period_end_date(
        forecast_tbl$period_start[[nrow(forecast_tbl)]],
        period_type
      )
    )
  )
  summary_html <- htmltools::HTML(paste0(intro, fit_line, horizon_line))

  info_notes <- character()
  if (!is.null(prep_metadata) && isTRUE(prep_metadata$is_aggregated)) {
    info_notes <- c(
      info_notes,
      sprintf(
        "<p>The uploaded %s data were aggregated to %s periods before forecasting.</p>",
        period_config(prep_metadata$source_period_type)$adjective,
        period_config(prep_metadata$target_period_type)$adjective
      )
    )
  }

  if (
    !is.null(prep_metadata) &&
      isTRUE(prep_metadata$partial_initial_period_removed)
  ) {
    info_notes <- c(
      info_notes,
      sprintf(
        "<p>The first %s period (%s) contained only partial data because the uploaded series begins part-way through that %s, so it was removed before forecasting.</p>",
        period_config(prep_metadata$target_period_type)$singular,
        prep_metadata$removed_initial_period_label,
        period_config(prep_metadata$target_period_type)$singular
      )
    )
  }

  if (
    !is.null(prep_metadata) &&
      isTRUE(prep_metadata$partial_final_period_removed)
  ) {
    info_notes <- c(
      info_notes,
      sprintf(
        "<p>The final %s period (%s) contained only partial data because the uploaded series ends part-way through that %s, so it was removed and the forecasts now start from the last complete period.</p>",
        period_config(prep_metadata$target_period_type)$singular,
        prep_metadata$removed_final_period_label,
        period_config(prep_metadata$target_period_type)$singular
      )
    )
  }

  info_html <- if (length(info_notes) > 0) {
    bootstrap_alert(
      "info",
      htmltools::HTML(paste0(info_notes, collapse = ""))
    )
  } else {
    NULL
  }

  if (length(reliability$warnings) == 0) {
    return(
      htmltools::tagList(
        summary_html,
        info_html,
        bootstrap_alert(
          "info",
          htmltools::p(
            "No major reliability warnings were triggered.",
            class = "mb-0 fw-semibold"
          )
        )
      )
    )
  }

  items <- paste(sprintf("<li>%s</li>", reliability$warnings), collapse = "")
  htmltools::tagList(
    summary_html,
    info_html,
    bootstrap_alert(
      "danger",
      htmltools::HTML(
        paste0(
          "<p class='fw-bold mb-2'>Warnings about reliability:</p><ul class='mb-0'>",
          items,
          "</ul>"
        )
      )
    )
  )
}
