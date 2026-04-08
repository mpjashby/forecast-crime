app_env <- new.env(parent = globalenv())
project_root <- normalizePath(testthat::test_path("../.."))
old_wd <- setwd(project_root)
on.exit(setwd(old_wd), add = TRUE)
source(file.path(project_root, "app.R"), local = app_env)


test_that("a new forecast run replaces stale warnings and re-enables Step 5", {
  shiny::testServer(app_env$server, {
    run_forecast_for <- function(path, time_col, count_col, period_type, horizon) {
      path <- normalizePath(path, mustWork = TRUE)
      suppressWarnings(
        session$setInputs(
          datafile = list(
            name = basename(path),
            datapath = path,
            size = unname(file.info(path)$size),
            type = "text/csv"
          )
        )
      )
      suppressWarnings(session$flushReact())

      suppressWarnings(
        session$setInputs(
          period_type = period_type,
          time_col = time_col,
          count_col = count_col,
          horizon = horizon
        )
      )
      suppressWarnings(session$flushReact())

      suppressWarnings(session$setInputs(run_forecast = input$run_forecast + 1))
      suppressWarnings(session$flushReact())
    }

    noisy_path <- file.path(
      project_root,
      "sample-data",
      "daily-noisy-high-uncertainty.csv"
    )
    clean_path <- file.path(
      project_root,
      "sample-data",
      "daily-burglary-counts.csv"
    )

    session$setInputs(run_forecast = 0)

    run_forecast_for(
      path = noisy_path,
      time_col = "day_start",
      count_col = "crime_count",
      period_type = "day",
      horizon = 28
    )

    expect_gte(length(forecast_result()$reliability$warnings), 1)
    expect_false(step5_inference_available())
    expect_match(run_settings()$count_col, "crime_count")

    run_forecast_for(
      path = clean_path,
      time_col = "period_start",
      count_col = "burglary_count",
      period_type = "day",
      horizon = 28
    )

    expect_length(forecast_result()$reliability$warnings, 0)
    expect_true(step5_inference_available())
    expect_equal(run_settings()$count_col, "burglary_count")
    expect_equal(run_settings()$time_col, "period_start")
    expect_false(results_stale())
    expect_true(is.list(forecast_comparison()))
  })
})


test_that("invalid text uploads surface an in-app error and clear selectors", {
  shiny::testServer(app_env$server, {
    path <- file.path(
      project_root,
      "sample-data",
      "manual-test-xml-disguised-as-csv.csv"
    )

    suppressWarnings(
      session$setInputs(
        datafile = list(
          name = basename(path),
          datapath = path,
          size = unname(file.info(path)$size),
          type = "text/csv"
        )
      )
    )
    suppressWarnings(session$flushReact())

    expect_match(
      paste(as.character(output$upload_feedback), collapse = " "),
      "does not look like CSV data"
    )
    expect_identical(input$time_col %||% "", "")
    expect_identical(input$count_col %||% "", "")
  })
})


test_that("headerless CSV uploads show an info alert and generated selectors", {
  shiny::testServer(app_env$server, {
    path <- file.path(
      project_root,
      "sample-data",
      "manual-test-no-header.csv"
    )

    suppressWarnings(
      session$setInputs(
        datafile = list(
          name = basename(path),
          datapath = path,
          size = unname(file.info(path)$size),
          type = "text/csv"
        )
      )
    )
    suppressWarnings(session$flushReact())

    expect_match(
      paste(as.character(output$upload_feedback), collapse = " "),
      "missing a header row"
    )
    expect_true(isTRUE(uploaded_data_result()$metadata$generated_column_names))
    expect_equal(names(uploaded_data_result()$data), c("date_column_1", "numeric_column_2"))
  })
})


test_that("generate forecasts button stays disabled until required inputs are ready", {
  shiny::testServer(app_env$server, {
    expect_match(
      paste(as.character(output$run_forecast_ui), collapse = " "),
      "disabled"
    )

    path <- file.path(
      project_root,
      "sample-data",
      "daily-burglary-counts.csv"
    )

    suppressWarnings(
      session$setInputs(
        datafile = list(
          name = basename(path),
          datapath = path,
          size = unname(file.info(path)$size),
          type = "text/csv"
        )
      )
    )
    suppressWarnings(session$flushReact())

    expect_false(forecast_ready())
    expect_match(
      paste(as.character(output$run_forecast_ui), collapse = " "),
      "disabled"
    )

    suppressWarnings(
      session$setInputs(
        period_type = "day",
        time_col = "period_start",
        count_col = "burglary_count"
      )
    )
    suppressWarnings(session$flushReact())

    expect_true(forecast_ready())
    expect_no_match(
      paste(as.character(output$run_forecast_ui), collapse = " "),
      "disabled"
    )
  })
})


test_that("choosing the same column for time and counts shows a warning and blocks forecasting", {
  shiny::testServer(app_env$server, {
    path <- file.path(
      project_root,
      "sample-data",
      "louisville-annual-homicides.csv"
    )

    suppressWarnings(
      session$setInputs(
        datafile = list(
          name = basename(path),
          datapath = path,
          size = unname(file.info(path)$size),
          type = "text/csv"
        )
      )
    )
    suppressWarnings(session$flushReact())

    suppressWarnings(
      session$setInputs(
        period_type = "year",
        time_col = "year",
        count_col = "year"
      )
    )
    suppressWarnings(session$flushReact())

    expect_true(duplicate_column_selected())
    expect_false(forecast_ready())
    expect_match(
      paste(as.character(output$column_selection_warning), collapse = " "),
      "must be different"
    )
    expect_match(
      paste(as.character(output$run_forecast_ui), collapse = " "),
      "Choose different columns"
    )
    expect_match(
      paste(as.character(output$run_forecast_ui), collapse = " "),
      "disabled"
    )
  })
})


test_that("holiday forecasts stay blocked until a holiday calendar is chosen", {
  shiny::testServer(app_env$server, {
    path <- file.path(
      project_root,
      "sample-data",
      "daily-burglary-counts.csv"
    )

    suppressWarnings(
      session$setInputs(
        datafile = list(
          name = basename(path),
          datapath = path,
          size = unname(file.info(path)$size),
          type = "text/csv"
        )
      )
    )
    suppressWarnings(session$flushReact())

    suppressWarnings(
      session$setInputs(
        period_type = "day",
        time_col = "period_start",
        count_col = "burglary_count",
        include_public_holidays = TRUE,
        holiday_country = ""
      )
    )
    suppressWarnings(session$flushReact())

    expect_true(holiday_country_required())
    expect_false(forecast_ready())
    expect_match(
      paste(as.character(output$run_forecast_ui), collapse = " "),
      "Choose a holiday calendar"
    )
    expect_match(
      paste(as.character(output$run_forecast_ui), collapse = " "),
      "disabled"
    )

    suppressWarnings(session$setInputs(holiday_country = "uk"))
    suppressWarnings(session$flushReact())

    expect_false(holiday_country_required())
    expect_true(forecast_ready())
    expect_no_match(
      paste(as.character(output$run_forecast_ui), collapse = " "),
      "disabled"
    )
  })
})
