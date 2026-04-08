suppressPackageStartupMessages({
  library(bslib)
  library(dplyr)
  library(fable)
  library(fabletools)
  library(feasts)
  library(ggplot2)
  library(htmltools)
  library(lubridate)
  library(purrr)
  library(readr)
  library(scales)
  library(stringr)
  library(tsibble)
  library(shiny)
  library(tibble)
})

# Load the helper modules through a single entry point so the UI and server can
# share parsing, validation, modelling, and UI text helpers without keeping all
# of that code in one file.
source("R/forecast_utils.R", local = TRUE)

# Store app-wide settings in one place so default behaviour can be adjusted
# without searching through the UI and server code.
app_settings <- list(
  comparison_same_threshold = 0.05,
  comparison_same_minimum_crimes = 5,
  comparison_simulations = 1000L,
  max_upload_bytes = 5 * 1024^2
)

options(shiny.maxRequestSize = app_settings$max_upload_bytes)

# Wrap outputs in a spinner only when shinycssloaders is installed.
with_spinner <- function(x) {
  if (requireNamespace("shinycssloaders", quietly = TRUE)) {
    shinycssloaders::withSpinner(x)
  } else {
    x
  }
}


format_user_facing_error <- function(
  error,
  fallback = "Something went wrong while generating the forecasts."
) {
  message <- conditionMessage(error) %||% ""
  message <- trimws(message)

  if (!nzchar(message) || identical(message, "[Object object]")) {
    fallback
  } else {
    message
  }
}


run_with_user_facing_errors <- function(expr, fallback) {
  tryCatch(
    expr,
    error = function(error) {
      stop(shiny::safeError(format_user_facing_error(error, fallback)))
    }
  )
}


render_ui_with_alert_errors <- function(expr, fallback = NULL) {
  expr_quo <- substitute(expr)
  expr_env <- parent.frame()

  renderUI({
    tryCatch(
      eval(expr_quo, envir = expr_env),
      error = function(error) {
        message <- format_user_facing_error(error, fallback %||% "")

        if (!nzchar(message)) {
          return(NULL)
        }

        bootstrap_alert(
          "danger",
          p(message, class = "mb-0")
        )
      }
    )
  })
}

# Build the Shiny user interface. Step 1 collects inputs, while Steps 2 to 4
# display model diagnostics, forecasts, and downloadable outputs.
app_ui <- page_fixed(
  title = "Crime count forecasting",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  tags$style(HTML(
    "
    .results-stale {
      opacity: 0.3;
      filter: grayscale(0.25);
    }
  "
  )),
  card(
    card_body(
      h2("Forecast the future frequency of crime"),
      p(strong(
        "Support strategic crime analysis with forecasts based on historical",
        "crime trends"
      )),
      p(HTML(
        "This app forecasts future crime counts based on patterns in",
        "historical crime data. The forecasts indicate how many crimes are",
        "likely to happen in future if recent patterns in crime continue. The ",
        "forecasts are created using an ensemble forecasting model that ",
        "<a href=\"https://doi.org/10.21428/cb6ab371.8c79f146\">Ashby (2023)</a> ",
        "found to be most accurate for forecasting crime."
      )),
      p(
        strong(
          "Just like weather forecasts, these crime forecasts are not perfect ",
          "predictions of the future."
        ),
        "They are best used as a guide to what ",
        "might happen if recent patterns continue, rather than a precise ",
        "prediction of what will happen. The forecast uncertainty intervals ",
        "shown at Step 3 indicate the range of crime counts that would not be ",
        "surprising based on recent patterns."
      ),
    )
  ),
  layout_columns(
    card(
      card_header("Step 1. Upload and configure data"),
      p(HTML(
        "Upload a <abbr title=\"Comma-Separated Values\">CSV</abbr> file of",
        "existing crime counts. The file should have one row for each time",
        "period. Each row should have two columns, one containing the date of",
        "the start of each forecast period and one column containing the",
        "number of crimes for that period."
      )),
      fileInput(
        inputId = "datafile",
        label = NULL,
        accept = ".csv",
        width = "100%"
      ),
      helpText(
        sprintf(
          "Maximum upload size: %s.",
          format_file_size(app_settings$max_upload_bytes)
        )
      ),
      uiOutput("upload_feedback"),
      radioButtons(
        inputId = "period_type",
        label = "Time frequency",
        choices = c(
          "daily" = "day",
          "weekly" = "week",
          "monthly" = "month",
          "annual" = "year"
        ),
        selected = character(0),
        inline = TRUE
      ),
      layout_columns(
        selectInput(
          inputId = "time_col",
          label = "Time-period column",
          choices = NULL
        ),
        selectInput(
          inputId = "count_col",
          label = "Crime-count column",
          choices = NULL
        ),
        col_widths = c(6, 6)
      ),
      uiOutput("column_selection_warning"),
      helpText(
        "If there is only one valid time column or one numeric count column, it will be selected automatically."
      ),
      div(
        class = "d-flex align-items-end gap-2",
        div(
          style = "flex: 1;",
          numericInput(
            inputId = "horizon",
            label = "How many periods into the future do you want to forecast?",
            value = 12,
            min = 1,
            step = 1,
            width = "100%"
          )
        ),
        div(
          class = "mb-3 fw-semibold text-muted",
          uiOutput("horizon_suffix")
        )
      ),
      checkboxInput(
        inputId = "include_public_holidays",
        label = "Include public holidays in the forecasts",
        value = FALSE
      ),
      conditionalPanel(
        condition = "input.include_public_holidays",
        selectInput(
          inputId = "holiday_country",
          label = "Holiday calendar",
          choices = public_holiday_country_choices(),
          selected = ""
        )
      ),
      uiOutput("run_forecast_ui")
    ),
    card(
      card_header("Step 2. Check data suitability"),
      uiOutput("step2_panel")
    ),
    col_widths = c(6, 6)
  ),
  layout_columns(
    card(
      card_header("Step 3. Check forecasts and uncertainty"),
      uiOutput("step3_panel")
    ),
    card(
      card_header("Step 4. Download forecasts"),
      uiOutput("step4_panel")
    ),
    col_widths = c(8, 4)
  ),
  card(
    card_header("Step 5. Compare forecasts with recent history"),
    uiOutput("step5_panel")
  ),
  card(
    card_header("Step 6. Understand how these forecasts are produced"),
    uiOutput("step6_panel")
  )
)


server <- function(input, output, session) {
  update_column_selector <- function(input_id, choices, selected = "") {
    updateSelectInput(
      session,
      input_id,
      choices = safe_choice_vector(choices, placeholder = "Choose a column"),
      selected = selected
    )
  }

  clear_column_selectors <- function() {
    update_column_selector("time_col", character(0))
    update_column_selector("count_col", character(0))
  }

  selected_holiday_country <- function(settings) {
    if (isTRUE(settings$include_public_holidays)) {
      settings$holiday_country
    } else {
      NULL
    }
  }

  current_settings <- reactive({
    list(
      data_path = input$datafile$datapath %||% NULL,
      period_type = input$period_type %||% "",
      time_col = input$time_col %||% "",
      count_col = input$count_col %||% "",
      horizon = input$horizon %||% NA_real_,
      include_public_holidays = isTRUE(input$include_public_holidays),
      holiday_country = input$holiday_country %||% "",
      comparison_same_threshold = app_settings$comparison_same_threshold,
      comparison_same_minimum_crimes = app_settings$comparison_same_minimum_crimes,
      comparison_simulations = app_settings$comparison_simulations
    )
  })

  latest_run_settings <- reactiveVal(NULL)

  panel_class <- reactive({
    if (
      !is.null(latest_run_settings()) &&
        !identical(latest_run_settings(), current_settings())
    ) {
      "results-stale"
    } else {
      NULL
    }
  })

  wrap_panel <- function(...) {
    div(class = panel_class(), ...)
  }

  results_stale <- reactive({
    identical(panel_class(), "results-stale")
  })

  duplicate_column_selected <- reactive({
    time_col <- input$time_col %||% ""
    count_col <- input$count_col %||% ""

    nzchar(time_col) &&
      nzchar(count_col) &&
      identical(time_col, count_col)
  })

  holiday_country_required <- reactive({
    isTRUE(input$include_public_holidays) &&
      !nzchar(input$holiday_country %||% "")
  })

  forecast_ready <- reactive({
    result <- uploaded_data_result()

    !is.null(result$data) &&
      is.null(result$error) &&
      nzchar(input$period_type %||% "") &&
      nzchar(input$time_col %||% "") &&
      nzchar(input$count_col %||% "") &&
      !isTRUE(duplicate_column_selected()) &&
      !isTRUE(holiday_country_required())
  })

  uploaded_data_result <- reactive({
    if (is.null(input$datafile)) {
      return(list(data = NULL, error = NULL, metadata = NULL))
    }

    if (!identical(tolower(tools::file_ext(input$datafile$name)), "csv")) {
      return(list(
        data = NULL,
        error = "Please upload a CSV file.",
        metadata = NULL
      ))
    }

    if (
      !is.null(input$datafile$size) &&
        input$datafile$size > app_settings$max_upload_bytes
    ) {
      return(list(
        data = NULL,
        error = sprintf(
          "The uploaded CSV file is too large. Please upload a file smaller than %s.",
          format_file_size(app_settings$max_upload_bytes)
        ),
        metadata = NULL
      ))
    }

    parsed_data <- tryCatch(
      read_crime_data(
        input$datafile$datapath,
        max_size_bytes = app_settings$max_upload_bytes
      ),
      error = function(error) error
    )

    if (inherits(parsed_data, "error")) {
      return(list(
        data = NULL,
        error = format_user_facing_error(
          parsed_data,
          "The uploaded file could not be read as CSV data."
        ),
        metadata = NULL
      ))
    }

    list(
      data = parsed_data,
      error = NULL,
      metadata = attr(parsed_data, "upload_metadata", exact = TRUE)
    )
  })

  uploaded_data <- reactive({
    result <- uploaded_data_result()
    req(is.null(result$error))
    req(!is.null(result$data))
    result$data
  })

  uploaded_data_profile <- reactive({
    result <- uploaded_data_result()
    req(is.null(result$error))
    req(!is.null(result$data))
    profile_data_columns(result$data)
  })

  output$upload_feedback <- renderUI({
    result <- uploaded_data_result()

    if (is.null(input$datafile)) {
      return(NULL)
    }

    if (!is.null(result$error)) {
      return(
        bootstrap_alert(
          "danger",
          p(result$error, class = "mb-0")
        )
      )
    }

    if (isTRUE(result$metadata$generated_column_names)) {
      return(
        bootstrap_alert(
          "info",
          p(
            "This CSV file appears to be missing a header row, so generic column names were generated automatically.",
            class = "mb-0"
          )
        )
      )
    }

    NULL
  })

  output$run_forecast_ui <- renderUI({
    button <- actionButton(
      "run_forecast",
      "Generate forecasts",
      class = "btn-primary"
    )

    if (isTRUE(forecast_ready())) {
      return(button)
    }

    tagList(
      htmltools::tagAppendAttributes(
        button,
        disabled = "disabled",
        `aria-disabled` = "true"
      ),
      p(
        if (isTRUE(duplicate_column_selected())) {
          "Choose different columns for the time period and crime count before generating forecasts."
        } else if (isTRUE(holiday_country_required())) {
          "Choose a holiday calendar before generating forecasts with public holidays included."
        } else {
          "Upload a file and choose the time frequency and both columns before generating forecasts."
        },
        class = "mt-2 mb-0 small text-muted"
      )
    )
  })

  output$column_selection_warning <- renderUI({
    if (!isTRUE(duplicate_column_selected())) {
      return(NULL)
    }

    bootstrap_alert(
      "warning",
      p(
        "The time-period column and crime-count column must be different. Please choose separate columns.",
        class = "mb-0"
      )
    )
  })

  observeEvent(
    uploaded_data_result(),
    {
      result <- uploaded_data_result()

      if (is.null(result$data) || !is.null(result$error)) {
        clear_column_selectors()
        updateRadioButtons(
          session,
          "period_type",
          selected = character(0)
        )
        return()
      }

      data <- result$data
      data_profile <- uploaded_data_profile()
      numeric_cols <- detect_count_columns(data, profile = data_profile)
      detected_frequency <- detect_frequency_from_data(
        data,
        profile = data_profile
      )

      update_column_selector(
        "count_col",
        names(data),
        selected = if (length(numeric_cols) == 1) numeric_cols else ""
      )

      updateRadioButtons(
        session,
        "period_type",
        selected = detected_frequency %||% character(0)
      )
    },
    ignoreNULL = FALSE
  )

  observeEvent(
    list(uploaded_data_result(), input$period_type),
    {
      result <- uploaded_data_result()

      if (is.null(result$data) || !is.null(result$error)) {
        update_column_selector("time_col", character(0))
        return()
      }

      data <- result$data
      data_profile <- uploaded_data_profile()

      if (is.null(input$period_type) || identical(input$period_type, "")) {
        update_column_selector("time_col", names(data))
        return()
      }

      time_candidates <- detect_time_columns(
        data,
        input$period_type,
        profile = data_profile
      )
      time_choices <- if (length(time_candidates) > 0) {
        time_candidates
      } else {
        names(data)
      }

      update_column_selector(
        "time_col",
        time_choices,
        selected = if (length(time_candidates) == 1) time_candidates else ""
      )
    },
    ignoreNULL = FALSE
  )

  observeEvent(
    input$period_type,
    {
      if (is.null(input$period_type) || identical(input$period_type, "")) {
        return()
      }

      updateNumericInput(
        session,
        "horizon",
        value = default_horizon(input$period_type)
      )
    },
    ignoreInit = TRUE
  )

  forecast_run <- eventReactive(
    input$run_forecast,
    {
      settings <- isolate(current_settings())
      data <- isolate(uploaded_data())
      data_profile <- isolate(uploaded_data_profile())
      selected_profile <- data_profile$columns[[settings$time_col]] %||% NULL
      parsed_index <- selected_profile$parsed_by_period[[
        settings$period_type
      ]] %||%
        NULL
      source_period_type <- selected_profile$detected_frequency %||%
        tryCatch(
          detect_frequency_from_column(data[[settings$time_col]]),
          error = function(e) NULL
        )
      parsed_source_index <- if (
        !is.null(selected_profile) &&
          !is.null(source_period_type) &&
          nzchar(source_period_type)
      ) {
        selected_profile$parsed_by_period[[source_period_type]] %||% NULL
      } else {
        NULL
      }

      req(
        nzchar(settings$period_type),
        nzchar(settings$time_col),
        nzchar(settings$count_col)
      )

      validate(
        need(
          settings$horizon >= 1,
          "The forecast horizon must be at least 1 period."
        ),
        need(
          !identical(settings$time_col, settings$count_col),
          "Choose different columns for the time period and crime count."
        ),
        need(
          !isTRUE(settings$include_public_holidays) ||
            nzchar(settings$holiday_country),
          "Please choose a country if you want to include public holidays."
        )
      )

      prepared <- prepare_crime_input(
        data = data,
        time_col = settings$time_col,
        count_col = settings$count_col,
        period_type = settings$period_type,
        source_period_type = source_period_type,
        parsed_index = parsed_index,
        parsed_source_index = parsed_source_index
      )

      forecast <- run_with_user_facing_errors(
        generate_forecast(
          ts_data = prepared$ts_data,
          period_type = settings$period_type,
          horizon = settings$horizon,
          holiday_country = selected_holiday_country(settings)
        ),
        fallback = paste(
          "The forecasts could not be generated with the current settings.",
          "Please check the selected holiday country and try again."
        )
      )

      comparison <- if (length(forecast$reliability$warnings) == 0) {
        run_with_user_facing_errors(
          build_forecast_comparison(
            ts_data = prepared$ts_data,
            forecast_tbl = forecast$forecast,
            model_tbl = forecast$models,
            period_type = settings$period_type,
            horizon = settings$horizon,
            holiday_country = selected_holiday_country(settings),
            same_threshold = settings$comparison_same_threshold,
            same_minimum_crimes = settings$comparison_same_minimum_crimes,
            n_simulations = settings$comparison_simulations
          ),
          fallback = paste(
            "The forecast comparison could not be generated.",
            "Please try regenerating the forecasts."
          )
        )
      } else {
        NULL
      }

      latest_run_settings(settings)

      list(
        settings = settings,
        prepared = prepared,
        forecast = forecast,
        comparison = comparison
      )
    },
    ignoreInit = TRUE
  )

  run_settings <- reactive({
    req(forecast_run())
    forecast_run()$settings
  })

  forecast_result <- reactive({
    req(forecast_run())
    forecast_run()$forecast
  })

  forecast_comparison <- reactive({
    req(forecast_run())
    forecast_run()$comparison
  })

  step5_inference_available <- reactive({
    req(forecast_result())
    length(forecast_result()$reliability$warnings) == 0
  })

  output$horizon_suffix <- renderUI({
    suffix <- format_period_suffix(input$period_type, input$horizon)

    if (identical(suffix, "")) {
      return(NULL)
    }

    span(suffix)
  })

  output$step2_panel <- render_ui_with_alert_errors({
    run <- forecast_run()
    req(run)

    wrap_panel(
      build_reliability_html(
        ts_data = run$prepared$ts_data,
        reliability = run$forecast$reliability,
        period_type = run$settings$period_type,
        horizon = run$settings$horizon,
        forecast_tbl = run$forecast$forecast,
        prep_metadata = run$prepared$metadata
      )
    )
  })

  output$forecast_plot <- renderPlot({
    run <- forecast_run()
    req(run)

    config <- period_config(run$settings$period_type)
    history_tbl <- run$prepared$ts_data |>
      tibble::as_tibble() |>
      mutate(period_start = index_to_date(index))

    recent_n <- min(config$recent_points, nrow(history_tbl))
    history_tbl <- dplyr::slice_tail(history_tbl, n = recent_n)

    forecast_tbl <- run$forecast$forecast
    bridge_tbl <- history_tbl |>
      slice_tail(n = 1) |>
      transmute(period_start, value = count, series = "Observed")
    forecast_line_tbl <- dplyr::bind_rows(
      bridge_tbl |>
        transmute(period_start, value, series = "Forecast"),
      forecast_tbl |>
        transmute(period_start, value = forecast, series = "Forecast")
    )
    forecast_interval_tbl <- dplyr::bind_rows(
      history_tbl |>
        slice_tail(n = 1) |>
        transmute(
          period_start,
          lower_50 = count,
          upper_50 = count,
          lower_80 = count,
          upper_80 = count,
          lower_95 = count,
          upper_95 = count
        ),
      forecast_tbl |>
        transmute(
          period_start,
          lower_50,
          upper_50,
          lower_80,
          upper_80,
          lower_95,
          upper_95
        )
    )
    plot_dates <- c(history_tbl$period_start, forecast_tbl$period_start)
    axis_breaks <- plot_date_breaks(plot_dates, n_labels = 5)

    ggplot() +
      geom_ribbon(
        data = forecast_interval_tbl,
        aes(x = period_start, ymin = lower_95, ymax = upper_95),
        fill = "#c6dbef",
        alpha = 0.6
      ) +
      geom_ribbon(
        data = forecast_interval_tbl,
        aes(x = period_start, ymin = lower_80, ymax = upper_80),
        fill = "#6baed6",
        alpha = 0.55
      ) +
      geom_ribbon(
        data = forecast_interval_tbl,
        aes(x = period_start, ymin = lower_50, ymax = upper_50),
        fill = "#2171b5",
        alpha = 0.45
      ) +
      geom_line(
        data = forecast_line_tbl,
        aes(x = period_start, y = value, colour = series),
        linewidth = 1
      ) +
      geom_point(
        data = forecast_tbl,
        aes(x = period_start, y = forecast, colour = "Forecast"),
        size = 2
      ) +
      geom_line(
        data = history_tbl,
        aes(x = period_start, y = count, colour = "Observed"),
        linewidth = 0.9
      ) +
      geom_point(
        data = history_tbl,
        aes(x = period_start, y = count, colour = "Observed"),
        size = 2
      ) +
      scale_colour_manual(
        values = c("Observed" = "#1b4965", "Forecast" = "#d94841"),
        breaks = c("Observed", "Forecast"),
        labels = c(
          "Observed" = "last few periods of uploaded crime counts",
          "Forecast" = "forecasts"
        )
      ) +
      scale_x_date(
        breaks = axis_breaks,
        labels = function(x) format_plot_axis_date(x, run$settings$period_type)
      ) +
      labs(
        x = NULL,
        y = "Crime count",
        colour = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  })

  output$step3_panel <- render_ui_with_alert_errors({
    req(forecast_run())

    wrap_panel(
      with_spinner(plotOutput("forecast_plot", height = 420)),
      div(
        class = "mt-2 small text-muted",
        p(
          "The shaded bands show three levels of forecast uncertainty, known as confidence intervals."
        ),
        p(
          strong("50% band:"),
          "the narrower inner range where the future crime count is most likely to fall."
        ),
        p(
          strong("80% band:"),
          "a wider range covering outcomes that are still fairly plausible."
        ),
        p(
          strong("95% band:"),
          "the widest range, showing the full spread of outcomes that would not be surprising if recent patterns continue."
        )
      )
    )
  })

  output$forecast_preview <- renderTable(
    {
      run <- forecast_run()
      req(run)

      run$forecast$forecast |>
        slice_head(n = 10) |>
        transmute(
          period_start = format_display_date(period_start),
          forecast = round(forecast, 1)
        )
    },
    striped = TRUE,
    bordered = TRUE,
    spacing = "s",
    width = "100%"
  )

  output$step4_panel <- render_ui_with_alert_errors({
    run <- forecast_run()
    req(run)
    n_preview <- min(10, nrow(run$forecast$forecast))
    preview_message <- if (nrow(run$forecast$forecast) > 10) {
      "The first 10 forecast periods are shown below."
    } else {
      sprintf(
        "All %s forecast %s %s shown below.",
        scales::comma(n_preview),
        if (n_preview == 1) "period" else "periods",
        if (n_preview == 1) "is" else "are"
      )
    }

    wrap_panel(
      downloadButton("download_forecast", "Download complete forecasts"),
      p(preview_message),
      tableOutput("forecast_preview"),
      p(
        "The downloaded data also include the 50%, 80%, and 95% confidence intervals.",
        class = "mt-2 mb-0 small text-muted"
      )
    )
  })

  output$step5_panel <- render_ui_with_alert_errors({
    run <- forecast_run()
    req(run)

    wrap_panel(
      if (length(run$forecast$reliability$warnings) == 0) {
        req(run$comparison)
        build_forecast_comparison_html(
          comparison = run$comparison,
          period_type = run$settings$period_type
        )
      } else {
        build_step5_unavailable_html(run$forecast$reliability)
      }
    )
  })

  output$step6_panel <- render_ui_with_alert_errors({
    run <- forecast_run()
    req(run)

    wrap_panel(
      HTML(
        build_modelling_explanation(
          ts_data = run$prepared$ts_data,
          period_type = run$settings$period_type,
          holiday_country = selected_holiday_country(run$settings)
        )
      )
    )
  })

  output$download_forecast <- downloadHandler(
    filename = function() {
      paste0("crime-forecasts-", Sys.Date(), ".csv")
    },
    content = function(file) {
      run <- forecast_run()
      req(run)
      readr::write_csv(run$forecast$forecast, file)
    }
  )
}


# Create the Shiny app object that can be launched by shiny::runApp().
app <- shinyApp(ui = app_ui, server = server)
app
