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
