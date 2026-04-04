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

# Load the forecasting helpers so the UI and server can share the same parsing,
# validation, aggregation, and modelling logic.
source("R/forecast_utils.R", local = TRUE)

# Wrap outputs in a spinner only when shinycssloaders is installed.
with_spinner <- function(x) {
  if (requireNamespace("shinycssloaders", quietly = TRUE)) {
    shinycssloaders::withSpinner(x)
  } else {
    x
  }
}

# Build the Shiny user interface. Step 1 collects inputs, while Steps 2 to 4
# display model diagnostics, forecasts, and downloadable outputs.
app_ui <- page_fixed(
  title = "Crime count forecasting",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  tags$style(HTML("
    .results-stale {
      opacity: 0.45;
      filter: grayscale(0.25);
    }
  ")),
  card(
    card_body(
      h2("Forecast crime counts"),
      p(
        "This app forecasts future crime counts based on patterns in",
        "historical crime data. The forecasts indicate how many crimes are",
        "likely to happen in future if recent patterns in crime continue."
      )
    )
  ),
  layout_columns(
    card(
      card_header("Step 1. Upload and configure data"),
      p(
        "Upload a CSV file of existing crime counts. The file should have one",
        "row for each time period. Each row should have two columns, one",
        "containing the date of the start of each forecast period and one",
        "column containing the number of crimes for that period."
      ),
      fileInput(
        inputId = "datafile",
        label = NULL,
        accept = ".csv",
        width = "100%"
      ),
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
      actionButton("run_forecast", "Generate forecasts", class = "btn-primary")
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
  )
)


server <- function(input, output, session) {
  # Track the current inputs so the app can tell when displayed results no
  # longer match the settings in Step 1.
  current_settings <- reactive({
    list(
      data_path = input$datafile$datapath %||% NULL,
      period_type = input$period_type %||% "",
      time_col = input$time_col %||% "",
      count_col = input$count_col %||% "",
      horizon = input$horizon %||% NA_real_
    )
  })

  applied_settings <- reactiveVal(NULL)

  # Mark the result panels as stale whenever the user changes Step 1 after
  # generating forecasts.
  results_stale <- reactive({
    !is.null(applied_settings()) && !identical(applied_settings(), current_settings())
  })

  uploaded_data <- reactive({
    req(input$datafile)
    validate(
      need(
        identical(tolower(tools::file_ext(input$datafile$name)), "csv"),
        "Please upload a CSV file."
      )
    )
    read_crime_data(input$datafile$datapath)
  })

  # After a file is uploaded, auto-populate the numeric count column and
  # auto-select the time frequency if the data have a clearly detectable cadence.
  observeEvent(
    uploaded_data(),
    {
      data <- uploaded_data()
      numeric_cols <- detect_count_columns(data)
      detected_frequency <- detect_frequency_from_data(data)

      updateSelectInput(
        session,
        "count_col",
        choices = names(data),
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

  # When the selected frequency changes, refresh the candidate time column list
  # because the same column may parse differently for days, weeks, months, or years.
  observeEvent(
    list(uploaded_data(), input$period_type),
    {
      data <- uploaded_data()
      if (is.null(input$period_type) || identical(input$period_type, "")) {
        updateSelectInput(
          session,
          "time_col",
          choices = names(data),
          selected = ""
        )
        return()
      }

      time_candidates <- detect_time_columns(data, input$period_type)
      time_choices <- if (length(time_candidates) > 0) time_candidates else names(data)

      updateSelectInput(
        session,
        "time_col",
        choices = time_choices,
        selected = if (length(time_candidates) == 1) time_candidates else ""
      )
    },
    ignoreNULL = FALSE
  )

  # Update the default forecast horizon to match the selected time frequency.
  observeEvent(input$period_type, {
    if (is.null(input$period_type) || identical(input$period_type, "")) {
      return()
    }

    updateNumericInput(
      session,
      "horizon",
      value = default_horizon(input$period_type)
    )
  }, ignoreInit = TRUE)

  # Freeze the prepared data only when the user explicitly requests new
  # forecasts. This is the version used by Steps 2 to 4 until the next run.
  prepared_input <- eventReactive(input$run_forecast, {
    req(uploaded_data(), input$time_col, input$count_col, input$period_type)
    applied_settings(isolate(current_settings()))
    prepare_crime_input(
      data = uploaded_data(),
      time_col = input$time_col,
      count_col = input$count_col,
      period_type = input$period_type,
      source_period_type = tryCatch(
        detect_frequency_from_column(uploaded_data()[[input$time_col]]),
        error = function(e) NULL
      )
    )
  })

  # Expose the prepared time series and the preparation metadata as separate
  # reactives so the rest of the server code can use them directly.
  prepared_series <- reactive({
    req(prepared_input())
    prepared_input()$ts_data
  })

  preparation_metadata <- reactive({
    req(prepared_input())
    prepared_input()$metadata
  })

  suitability <- reactive({
    req(prepared_series(), input$period_type, input$horizon)
    assess_series(prepared_series(), input$period_type, input$horizon)
  })

  forecast_result <- eventReactive(input$run_forecast, {
    req(prepared_series(), input$horizon)
    validate(
      need(
        input$horizon >= 1,
        "The forecast horizon must be at least 1 period."
      )
    )
    generate_forecast(
      ts_data = prepared_series(),
      period_type = input$period_type,
      horizon = input$horizon
    )
  })

  # Step 2 shows data checks, aggregation notes, and forecast reliability
  # information based on the latest generated results.
  output$reliability_ui <- renderUI({
    req(forecast_result())
    build_reliability_html(
      ts_data = prepared_series(),
      reliability = forecast_result()$reliability,
      period_type = input$period_type,
      horizon = input$horizon,
      forecast_tbl = forecast_result()$forecast,
      prep_metadata = preparation_metadata()
    )
  })

  # Gray out Step 2 whenever Step 1 changes after the last run.
  output$step2_panel <- renderUI({
    div(
      class = if (results_stale()) "results-stale" else NULL,
      with_spinner(uiOutput("reliability_ui"))
    )
  })

  # Add the unit label beside the horizon input, e.g. "weeks" or "years".
  output$horizon_suffix <- renderUI({
    suffix <- format_period_suffix(input$period_type, input$horizon)

    if (identical(suffix, "")) {
      return(NULL)
    }

    span(suffix)
  })

  # Draw the forecast chart with the last few observed points, the forecast
  # path, and nested uncertainty bands.
  output$forecast_plot <- renderPlot({
    req(forecast_result(), prepared_series(), input$period_type)
    config <- period_config(input$period_type)
    history_tbl <- prepared_series() |>
      tibble::as_tibble() |>
      mutate(period_start = index_to_date(index))

    recent_n <- min(config$recent_points, nrow(history_tbl))
    history_tbl <- dplyr::slice_tail(history_tbl, n = recent_n)

    forecast_tbl <- forecast_result()$forecast
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
      scale_x_date(labels = format_display_date) +
      labs(
        x = NULL,
        y = "Crime count",
        colour = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "bottom"
      )
  })

  # Gray out Step 3 whenever Step 1 changes after the last run.
  output$step3_panel <- renderUI({
    div(
      class = if (results_stale()) "results-stale" else NULL,
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

  # Show the first few forecast rows in a simple table for quick inspection.
  output$forecast_preview <- renderTable(
    {
      req(forecast_result())
      forecast_result()$forecast |>
        slice_head(n = 10) |>
        select(
          period_start,
          forecast,
          lower_50,
          upper_50,
          lower_80,
          upper_80,
          lower_95,
          upper_95
        ) |>
        mutate(
          period_start = format_display_date(period_start),
          forecast = round(forecast, 1),
          lower_50 = round(lower_50, 1),
          upper_50 = round(upper_50, 1),
          lower_80 = round(lower_80, 1),
          upper_80 = round(upper_80, 1),
          lower_95 = round(lower_95, 1),
          upper_95 = round(upper_95, 1)
        )
    },
    striped = TRUE,
    bordered = TRUE,
    spacing = "s",
    width = "100%"
  )

  # Gray out Step 4 whenever Step 1 changes after the last run.
  output$step4_panel <- renderUI({
    div(
      class = if (results_stale()) "results-stale" else NULL,
      downloadButton("download_forecast", "Download CSV"),
      p("The first 10 forecast periods are shown below."),
      tableOutput("forecast_preview")
    )
  })

  # Download the full forecast table as CSV.
  output$download_forecast <- downloadHandler(
    filename = function() {
      paste0("crime-forecasts-", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(forecast_result())
      readr::write_csv(forecast_result()$forecast, file)
    }
  )
}


# Create the Shiny app object that can be launched by shiny::runApp().
app <- shinyApp(ui = app_ui, server = server)
app
