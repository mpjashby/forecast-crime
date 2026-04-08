source(testthat::test_path("../../R/forecast_utils.R"), local = TRUE)

test_that("time and count columns are detected correctly", {
  data <- tibble::tibble(
    period = c("2024-01-01", "2024-01-02"),
    crime_count = c(12, 15),
    neighbourhood = c("A", "B")
  )

  expect_equal(detect_time_columns(data, "day"), "period")
  expect_equal(detect_count_columns(data), "crime_count")
})


test_that("uploaded text content is HTML-escaped before use", {
  data <- tibble::tibble(
    `<script>alert(1)</script>` = c("2024-01-01", "2024-01-02"),
    category = c("<img src=x onerror=alert(1)>", "plain")
  )

  sanitized <- sanitize_uploaded_data(data)

  expect_equal(names(sanitized)[[1]], "&lt;script&gt;alert(1)&lt;/script&gt;")
  expect_equal(
    sanitized$category[[1]],
    "&lt;img src=x onerror=alert(1)&gt;"
  )
})


test_that("non-text uploads are rejected before parsing as CSV", {
  path <- tempfile(fileext = ".csv")
  writeBin(as.raw(c(0x00, 0x01, 0x02, 0x03)), path)

  expect_error(
    read_crime_data(path),
    "does not look like plain-text CSV"
  )
})


test_that("midnight-only datetime columns are treated as date columns", {
  data <- tibble::tibble(
    period = as.POSIXct(
      c("2024-01-01 00:00:00", "2024-01-02 00:00:00"),
      tz = "UTC"
    ),
    crime_count = c(12, 15)
  )

  expect_true(datetime_is_midnight_only(data$period))
  expect_equal(detect_time_columns(data, "day"), "period")
  expect_equal(detect_frequency_from_data(data), "day")
})


test_that("multiple date and datetime formats are parsed successfully", {
  iso_dates <- parse_day_values(c("2026-01-01", "2026-01-02"))
  long_dates <- parse_day_values(c("01 Jan 2026", "02 Jan 2026"))
  slash_dmy <- parse_day_values(c("13/01/2026", "14/01/2026"))
  slash_mdy <- parse_day_values(c("01/13/2026", "01/14/2026"))
  iso_datetimes <- parse_day_values(c("2026-01-01 00:00:00", "2026-01-02 00:00:00"))
  tz_datetimes <- parse_day_values(c(
    "2026-01-01 00:00:00 UTC",
    "2026-01-02 00:00:00 UTC"
  ))
  nonstandard_datetimes <- parse_day_values(c(
    "01-Jan-2026 00:00:00",
    "02-Jan-2026 00:00:00"
  ))

  expected <- as.Date(c("2026-01-01", "2026-01-02"))
  expected_slash <- as.Date(c("2026-01-13", "2026-01-14"))

  expect_equal(iso_dates, expected)
  expect_equal(long_dates, expected)
  expect_equal(slash_dmy, expected_slash)
  expect_equal(slash_mdy, expected_slash)
  expect_equal(iso_datetimes, expected)
  expect_equal(tz_datetimes, expected)
  expect_equal(nonstandard_datetimes, expected)
})


test_that("datetime columns with non-zero times are ignored when valid date columns exist", {
  data <- tibble::tibble(
    event_time = as.POSIXct(
      c("2024-01-01 08:30:00", "2024-01-02 09:45:00"),
      tz = "UTC"
    ),
    period = as.Date(c("2024-01-01", "2024-01-02")),
    crime_count = c(12, 15)
  )

  expect_false(datetime_is_midnight_only(data$event_time))
  expect_equal(detect_time_columns(data, "day"), "period")
  expect_equal(detect_frequency_from_data(data), "day")
})


test_that("datetime columns with non-zero times produce a user-facing error when selected", {
  data <- tibble::tibble(
    event_time = as.POSIXct(
      c("2024-01-01 08:30:00", "2024-01-02 09:45:00"),
      tz = "UTC"
    ),
    crime_count = c(12, 15)
  )

  expect_error(
    prepare_crime_input(
      data = data,
      time_col = "event_time",
      count_col = "crime_count",
      period_type = "day"
    ),
    "can only handle daily, weekly, monthly, or annual data"
  )
})


test_that("reliability HTML uses bootstrap alerts for info, warning, and danger states", {
  ts_data <- tsibble::as_tsibble(
    tibble::tibble(
      index = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 24),
      count = rep(10, 24)
    ),
    index = index
  )

  forecast_tbl <- tibble::tibble(
    period_start = seq.Date(as.Date("2026-01-01"), by = "month", length.out = 3)
  )

  reliability_ok <- list(
    summary = list(n_periods = 24, mean_count = 10),
    mape = 12.5,
    warnings = character()
  )

  prep_metadata <- list(
    is_aggregated = TRUE,
    source_period_type = "day",
    target_period_type = "month",
    partial_initial_period_removed = TRUE,
    removed_initial_period_label = "Jan 2024",
    partial_final_period_removed = FALSE
  )

  ok_html <- htmltools::renderTags(
    build_reliability_html(
      ts_data = ts_data,
      reliability = reliability_ok,
      period_type = "month",
      horizon = 3,
      forecast_tbl = forecast_tbl,
      prep_metadata = prep_metadata
    )
  )$html

  expect_match(ok_html, "alert alert-info")
  expect_match(ok_html, "alert alert-warning")

  reliability_bad <- modifyList(
    reliability_ok,
    list(warnings = "Forecast horizon is too long for the available history.")
  )

  bad_html <- htmltools::renderTags(
    build_reliability_html(
      ts_data = ts_data,
      reliability = reliability_bad,
      period_type = "month",
      horizon = 3,
      forecast_tbl = forecast_tbl,
      prep_metadata = prep_metadata
    )
  )$html

  expect_match(bad_html, "alert alert-danger")
})


test_that("unavailable comparison HTML uses a bootstrap danger alert", {
  html <- htmltools::renderTags(
    build_forecast_comparison_html(
      comparison = list(
        available = FALSE,
        reason = "The comparison could not be produced."
      ),
      period_type = "month"
    )
  )$html

  expect_match(html, "alert alert-danger")
})


test_that("Step 5 unavailable HTML explains that warnings make comparisons unreliable", {
  html <- htmltools::renderTags(
    build_step5_unavailable_html(
      reliability = list(
        warnings = c(
          "The 95% intervals are very wide relative to the forecast level, indicating high uncertainty.",
          "Historical fit is weak (MAPE about 35%), so future forecasts may be unreliable."
        )
      )
    )
  )$html

  expect_match(html, "alert alert-danger")
  expect_match(html, "too uncertain")
  expect_match(html, "high uncertainty")
  expect_match(html, "cannot reliably judge")
})


test_that("time frequency can be detected from regular input data", {
  daily_data <- tibble::tibble(
    day_start = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 10),
    crimes = seq(10, 19)
  )

  weekly_data <- tibble::tibble(
    week_start = seq.Date(as.Date("2024-01-01"), by = "week", length.out = 8),
    crimes = seq(10, 17)
  )

  monthly_data <- tibble::tibble(
    month_start = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 8),
    crimes = seq(10, 17)
  )

  annual_data <- tibble::tibble(
    year_start = 2015:2024,
    crimes = seq(10, 19)
  )

  irregular_data <- tibble::tibble(
    when = as.Date(c("2024-01-01", "2024-01-05", "2024-01-12")),
    crimes = c(4, 6, 8)
  )

  expect_equal(detect_frequency_from_data(daily_data), "day")
  expect_equal(detect_frequency_from_data(weekly_data), "week")
  expect_equal(detect_frequency_from_data(monthly_data), "month")
  expect_equal(detect_frequency_from_data(annual_data), "year")
  expect_null(detect_frequency_from_data(irregular_data))
})


test_that("supported public-holiday countries are exposed for the UI", {
  choices <- public_holiday_country_choices(include_placeholder = FALSE)

  expect_true(any(grepl("United Kingdom", names(choices), fixed = TRUE)))
  expect_true(any(grepl("United States", names(choices), fixed = TRUE)))
  expect_true("uk" %in% unname(choices))
  expect_true("us" %in% unname(choices))
})


test_that("public-holiday regressors count holidays within modelled periods", {
  daily_index <- seq.Date(as.Date("2024-12-24"), by = "day", length.out = 4)
  weekly_index <- tsibble::yearweek(as.Date("2024-12-23"))

  expect_equal(
    build_public_holiday_regressor(daily_index, "day", "uk"),
    c(0L, 1L, 1L, 0L)
  )
  expect_equal(
    build_public_holiday_regressor(weekly_index, "week", "uk"),
    2L
  )
})


test_that("US public holidays can be generated without attaching timeDate", {
  us_holidays <- public_holiday_dates(2020:2021, "us")

  expect_true(as.Date("2020-01-01") %in% us_holidays)
  expect_true(as.Date("2020-07-04") %in% us_holidays)
  expect_true(as.Date("2021-12-25") %in% us_holidays)
})


test_that("individual regular date columns are not misclassified across frequencies", {
  daily_values <- seq.Date(as.Date("2024-01-01"), by = "day", length.out = 10)
  weekly_values <- seq.Date(as.Date("2024-01-01"), by = "week", length.out = 8)
  monthly_values <- seq.Date(as.Date("2024-01-01"), by = "month", length.out = 8)
  annual_values <- 2015:2024

  expect_equal(detect_frequency_from_column(daily_values), "day")
  expect_equal(detect_frequency_from_column(weekly_values), "week")
  expect_equal(detect_frequency_from_column(monthly_values), "month")
  expect_equal(detect_frequency_from_column(annual_values), "year")
})


test_that("numeric annual columns are detected only when they are strict year sequences", {
  expect_equal(detect_frequency_from_column(2015:2024), "year")
  expect_null(detect_frequency_from_column(c(2015, 2016, 2018, 2019)))
  expect_null(detect_frequency_from_column(c(2015, 2016.5, 2017.0, 2018)))
  expect_null(detect_frequency_from_column(c(999, 1000, 1001, 1002)))
  expect_null(detect_frequency_from_column(c(10000, 10001, 10002)))
})


test_that("missing weekly periods in the middle of the series produce an error", {
  data <- tibble::tibble(
    week = c("2024-W01", "2024-W03"),
    crimes = c(20, 35)
  )

  expect_error(
    prepare_crime_ts(data, "week", "crimes", "week"),
    "missing weeks within the series"
  )
})


test_that("finer data are aggregated and partial final aggregated periods are removed", {
  data <- tibble::tibble(
    day_start = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 10),
    crimes = c(5, 6, 7, 4, 5, 8, 9, 3, 2, 4)
  )

  prepared <- prepare_crime_input(
    data = data,
    time_col = "day_start",
    count_col = "crimes",
    period_type = "week",
    source_period_type = "day"
  )

  expect_true(prepared$metadata$is_aggregated)
  expect_false(prepared$metadata$partial_initial_period_removed)
  expect_true(prepared$metadata$partial_final_period_removed)
  expect_null(prepared$metadata$removed_initial_period_label)
  expect_equal(prepared$metadata$removed_final_period_label, "08 Jan 2024")
  expect_equal(nrow(prepared$ts_data), 1)
  expect_equal(prepared$ts_data$count, sum(data$crimes[1:7]))
})


test_that("finer data can drop partial aggregated periods at both the start and end", {
  data <- tibble::tibble(
    day_start = seq.Date(as.Date("2010-01-01"), as.Date("2019-12-31"), by = "day"),
    crimes = rep(1, 3652)
  )

  prepared <- prepare_crime_input(
    data = data,
    time_col = "day_start",
    count_col = "crimes",
    period_type = "week",
    source_period_type = "day"
  )

  expect_true(prepared$metadata$is_aggregated)
  expect_true(prepared$metadata$partial_initial_period_removed)
  expect_true(prepared$metadata$partial_final_period_removed)
  expect_equal(prepared$metadata$removed_initial_period_label, "28 Dec 2009")
  expect_equal(prepared$metadata$removed_final_period_label, "30 Dec 2019")
  expect_equal(tsibble::yearweek(as.Date("2010-01-04")), prepared$ts_data$index[[1]])
  expect_equal(tsibble::yearweek(as.Date("2019-12-23")), prepared$ts_data$index[[nrow(prepared$ts_data)]])
  expect_true(all(prepared$ts_data$count == 7))
})


test_that("only nested aggregations are allowed", {
  weekly_data <- tibble::tibble(
    week_start = seq.Date(as.Date("2024-01-01"), by = "week", length.out = 53),
    crimes = seq(20, 72)
  )

  annual_prepared <- prepare_crime_input(
    data = weekly_data,
    time_col = "week_start",
    count_col = "crimes",
    period_type = "year",
    source_period_type = "week"
  )

  expect_true(annual_prepared$metadata$is_aggregated)

  expect_error(
    prepare_crime_input(
      data = weekly_data,
      time_col = "week_start",
      count_col = "crimes",
      period_type = "month",
      source_period_type = "week"
    ),
    "cannot be created safely"
  )
})


test_that("irregular dates cannot be aggregated to a coarser frequency", {
  irregular_data <- tibble::tibble(
    day_start = as.Date(c(
      "2024-01-01", "2024-01-03", "2024-01-04", "2024-01-10", "2024-01-15"
    )),
    crimes = c(10, 11, 12, 9, 8)
  )

  expect_error(
    prepare_crime_input(
      data = irregular_data,
      time_col = "day_start",
      count_col = "crimes",
      period_type = "week"
    ),
    "does not imply a regular daily, weekly, monthly, or annual frequency"
  )
})


test_that("irregular daily dates cannot be forecast as daily data", {
  irregular_data <- tibble::tibble(
    day_start = as.Date(c(
      "2024-01-01", "2024-01-05", "2024-01-12", "2024-01-20"
    )),
    crimes = c(12, 14, 11, 10)
  )

  expect_error(
    prepare_crime_input(
      data = irregular_data,
      time_col = "day_start",
      count_col = "crimes",
      period_type = "day"
    ),
    "does not imply a regular daily, weekly, monthly, or annual frequency"
  )
})


test_that("daily data cannot be aggregated when a middle month is incomplete", {
  full_january <- seq.Date(as.Date("2024-01-01"), as.Date("2024-01-31"), by = "day")
  incomplete_february <- setdiff(
    seq.Date(as.Date("2024-02-01"), as.Date("2024-02-29"), by = "day"),
    as.Date("2024-02-14")
  )
  full_march <- seq.Date(as.Date("2024-03-01"), as.Date("2024-03-31"), by = "day")

  data <- tibble::tibble(
    day_start = c(full_january, incomplete_february, full_march),
    crimes = rep(5, length(c(full_january, incomplete_february, full_march)))
  )

  expect_error(
    prepare_crime_input(
      data = data,
      time_col = "day_start",
      count_col = "crimes",
      period_type = "month",
      source_period_type = "day"
    ),
    "missing days within the series"
  )
})


test_that("daily data can aggregate to month when only the final month is partial", {
  january <- seq.Date(as.Date("2024-01-01"), as.Date("2024-01-31"), by = "day")
  february <- seq.Date(as.Date("2024-02-01"), as.Date("2024-02-29"), by = "day")
  partial_march <- seq.Date(as.Date("2024-03-01"), as.Date("2024-03-12"), by = "day")

  data <- tibble::tibble(
    day_start = c(january, february, partial_march),
    crimes = rep(3, length(c(january, february, partial_march)))
  )

  prepared <- prepare_crime_input(
    data = data,
    time_col = "day_start",
    count_col = "crimes",
    period_type = "month",
    source_period_type = "day"
  )

  expect_true(prepared$metadata$is_aggregated)
  expect_false(prepared$metadata$partial_initial_period_removed)
  expect_true(prepared$metadata$partial_final_period_removed)
  expect_equal(nrow(prepared$ts_data), 2)
})


test_that("monthly strings are parsed", {
  parsed <- parse_period_column(c("2024-01", "2024-02"), "month")

  expect_s3_class(parsed, "yearmonth")
  expect_equal(as.character(parsed), c("2024 Jan", "2024 Feb"))
})


test_that("annual values are parsed and annual forecasts can be generated", {
  parsed <- parse_period_column(c("2018", "2019", "2020"), "year")
  expect_equal(parsed, as.Date(c("2018-01-01", "2019-01-01", "2020-01-01")))

  data <- tibble::tibble(
    year = 2012:2023,
    crimes = c(150, 155, 162, 168, 171, 175, 179, 184, 188, 193, 197, 201)
  )

  ts_data <- prepare_crime_ts(data, "year", "crimes", "year")
  expect_equal(nrow(ts_data), nrow(data))
  expect_equal(ts_data$count, data$crimes)
  expect_equal(ts_data$index, data$year)
  result <- generate_forecast(ts_data, "year", horizon = 2)

  expect_equal(nrow(result$forecast), 2)
  expect_true(all(c("lower_50", "upper_50", "lower_80", "upper_80") %in% names(result$forecast)))
})


test_that("series assessment flags short and sparse data", {
  ts_data <- tsibble::as_tsibble(
    tibble::tibble(
      index = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 10),
      count = c(rep(0, 8), 1, 2)
    ),
    index = index
  )

  assessment <- assess_series(ts_data, "day", horizon = 8)

  expect_true(assessment$insufficient_history)
  expect_true(assessment$sparse_counts)
  expect_true(length(assessment$warnings) >= 2)
})


test_that("default horizon and suffix match the selected period type", {
  expect_equal(default_horizon("day"), 28)
  expect_equal(default_horizon("week"), 12)
  expect_equal(default_horizon("month"), 12)
  expect_equal(default_horizon("year"), 3)

  expect_equal(format_period_suffix("week", 12), "weeks")
  expect_equal(format_period_suffix("year", 1), "year")
  expect_equal(format_period_suffix("", 5), "")
})


test_that("no aggregation metadata is added when source and target frequencies match", {
  data <- tibble::tibble(
    week_start = seq.Date(as.Date("2024-01-01"), by = "week", length.out = 8),
    crimes = seq(20, 27)
  )

  prepared <- prepare_crime_input(
    data = data,
    time_col = "week_start",
    count_col = "crimes",
    period_type = "week",
    source_period_type = "week"
  )

  expect_false(prepared$metadata$is_aggregated)
  expect_false(prepared$metadata$partial_initial_period_removed)
  expect_false(prepared$metadata$partial_final_period_removed)
})


test_that("Step 2 explains when partial aggregated periods are removed at either end", {
  ts_data <- tsibble::as_tsibble(
    tibble::tibble(
      index = tsibble::yearweek(seq.Date(as.Date("2010-01-04"), by = "week", length.out = 4)),
      count = c(7, 7, 7, 7)
    ),
    index = index
  )

  reliability <- list(
    summary = list(
      n_periods = 4,
      mean_count = 7
    ),
    mape = 12.5,
    warnings = character(),
    insufficient_history = FALSE,
    sparse_counts = FALSE,
    excessive_horizon = FALSE
  )

  forecast_tbl <- tibble::tibble(
    period_start = tsibble::yearweek(seq.Date(as.Date("2010-02-01"), by = "week", length.out = 2)),
    forecast = c(7, 7)
  )

  html <- build_reliability_html(
    ts_data = ts_data,
    reliability = reliability,
    period_type = "week",
    horizon = 2,
    forecast_tbl = forecast_tbl,
    prep_metadata = list(
      source_period_type = "day",
      target_period_type = "week",
      is_aggregated = TRUE,
      partial_initial_period_removed = TRUE,
      partial_final_period_removed = TRUE,
      removed_initial_period_label = "2009 W53",
      removed_final_period_label = "2020 W01"
    )
  )

  html_text <- as.character(html)

  expect_match(html_text, "aggregated to weekly periods")
  expect_match(html_text, "first week period \\(2009 W53\\) contained only partial data")
  expect_match(html_text, "begins part-way through that week")
  expect_match(html_text, "final week period \\(2020 W01\\) contained only partial data")
  expect_match(html_text, "ends part-way through that week")
})


test_that("forecast generation returns forecast intervals", {
  data <- tibble::tibble(
    period = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 30),
    crimes = c(40, 42, 45, 50, 48, 51, 55, 57, 58, 60, 62, 64, 61, 63, 67,
               69, 70, 72, 71, 74, 78, 80, 82, 79, 83, 86, 88, 90, 93, 95)
  )

  ts_data <- prepare_crime_ts(data, "period", "crimes", "month")
  result <- generate_forecast(ts_data, "month", horizon = 3)

  expect_equal(nrow(result$forecast), 3)
  expect_true(all(c("forecast", "lower_95", "upper_95") %in% names(result$forecast)))
  expect_true(all(result$forecast$upper_95 >= result$forecast$lower_95))
})


test_that("daily forecasts with exactly 14 observations do not return all-NA output", {
  ts_data <- tibble::tibble(
    index = seq.Date(as.Date("2025-01-01"), by = "day", length.out = 14),
    count = c(12, 15, 11, 14, 13, 10, 9, 12, 16, 14, 13, 11, 12, 15)
  ) |>
    tsibble::as_tsibble(index = index)

  result <- generate_forecast(ts_data, "day", horizon = 7)

  expect_equal(nrow(result$forecast), 7)
  expect_false(any(is.na(result$forecast$forecast)))
  expect_false(any(is.na(result$forecast$lower_95)))
  expect_false(any(is.na(result$forecast$upper_95)))
})


test_that("monthly forecasts at the two-year boundary still produce valid values", {
  data <- tibble::tibble(
    period = seq.Date(as.Date("2022-01-01"), by = "month", length.out = 24),
    crimes = rep(c(1, 2, 1, 2), length.out = 24)
  )

  ts_data <- prepare_crime_ts(data, "period", "crimes", "month")
  result <- generate_forecast(ts_data, "month", horizon = 6)

  expect_equal(nrow(result$forecast), 6)
  expect_false(any(is.na(result$forecast$forecast)))
})


test_that("weekly forecasts at the annual boundary still produce valid values", {
  data <- tibble::tibble(
    period = seq.Date(as.Date("2020-01-06"), by = "week", length.out = 52),
    crimes = rep(c(1, 2, 1, 2), length.out = 52)
  )

  ts_data <- prepare_crime_ts(data, "period", "crimes", "week")
  result <- generate_forecast(ts_data, "week", horizon = 6)

  expect_equal(nrow(result$forecast), 6)
  expect_false(any(is.na(result$forecast$forecast)))
})


test_that("forecast generation works with public-holiday regressors", {
  ts_data <- tibble::tibble(
    index = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 400),
    count = rep(c(8, 9, 10, 11, 12, 13, 14), length.out = 400)
  ) |>
    tsibble::as_tsibble(index = index)

  result <- generate_forecast(
    ts_data = ts_data,
    period_type = "day",
    horizon = 7,
    holiday_country = "uk"
  )

  expect_equal(nrow(result$forecast), 7)
  expect_false(any(is.na(result$forecast$forecast)))
  expect_false(any(is.na(result$forecast$lower_95)))
  expect_false(any(is.na(result$forecast$upper_95)))
})


test_that("aggregated daily data can still produce weekly forecasts", {
  data <- tibble::tibble(
    period_start = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 91),
    burglary_count = c(
      18, 20, 22, 21, 19, 17, 16,
      19, 21, 23, 22, 20, 18, 17,
      20, 22, 24, 23, 21, 19, 18,
      21, 23, 25, 24, 22, 20, 19,
      22, 24, 26, 25, 23, 21, 20,
      23, 25, 27, 26, 24, 22, 21,
      24, 26, 28, 27, 25, 23, 22,
      25, 27, 29, 28, 26, 24, 23,
      26, 28, 30, 29, 27, 25, 24,
      27, 29, 31, 30, 28, 26, 25,
      28, 30, 32, 31, 29, 27, 26,
      29, 31, 33, 32, 30, 28, 27,
      30, 32, 34, 33, 31, 29, 28
    )
  )

  prepared <- prepare_crime_input(
    data = data,
    time_col = "period_start",
    count_col = "burglary_count",
    period_type = "week",
    source_period_type = "day"
  )

  result <- generate_forecast(prepared$ts_data, "week", horizon = 6)

  expect_equal(nrow(result$forecast), 6)
  expect_false(any(is.na(result$forecast$forecast)))
  expect_false(any(is.na(result$forecast$lower_95)))
  expect_false(any(is.na(result$forecast$upper_95)))
})


test_that("forecast comparisons use the most recent matching historical window", {
  data <- tibble::tibble(
    period = seq.Date(as.Date("2022-01-01"), by = "month", length.out = 36),
    crimes = seq(50, 85)
  )

  ts_data <- prepare_crime_ts(data, "period", "crimes", "month")
  result <- generate_forecast(ts_data, "month", horizon = 6)
  comparison <- build_forecast_comparison(
    ts_data = ts_data,
    forecast_tbl = result$forecast,
    model_tbl = result$models,
    period_type = "month",
    horizon = 6,
    same_threshold = 0.05,
    same_minimum_crimes = 5,
    n_simulations = 200
  )

  expected_recent_total <- sum(tail(ts_data$count, 6))
  expected_band <- max(expected_recent_total * 0.05, 5)

  expect_true(comparison$available)
  expect_equal(comparison$comparison_total, expected_recent_total)
  expect_equal(comparison$comparison_period_start, as.Date("2024-07-01"))
  expect_equal(comparison$comparison_period_end, as.Date("2024-12-31"))
  expect_equal(comparison$same_absolute_band, expected_band)
  expect_equal(comparison$lower_same_cutoff, expected_recent_total - expected_band)
  expect_equal(comparison$upper_same_cutoff, expected_recent_total + expected_band)
  expect_equal(
    comparison$probability_higher +
      comparison$probability_same +
      comparison$probability_lower,
    1,
    tolerance = 1e-8
  )
})


test_that("forecast comparisons report when there is not enough history", {
  ts_data <- tibble::tibble(
    index = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 4),
    count = c(20, 22, 24, 23)
  ) |>
    tsibble::as_tsibble(index = index)

  forecast_tbl <- tibble::tibble(
    period_start = seq.Date(as.Date("2024-05-01"), by = "month", length.out = 6),
    forecast = rep(25, 6)
  )

  comparison <- build_forecast_comparison(
    ts_data = ts_data,
    forecast_tbl = forecast_tbl,
    model_tbl = NULL,
    period_type = "month",
    horizon = 6,
    same_threshold = 0.05,
    same_minimum_crimes = 5,
    n_simulations = 100
  )

  expect_false(comparison$available)
  expect_match(comparison$reason, "needs at least 6 historical months")
})


test_that("forecast comparisons use the minimum-crimes threshold when it is larger", {
  data <- tibble::tibble(
    period = seq.Date(as.Date("2022-01-01"), by = "month", length.out = 24),
    crimes = rep(c(1, 2, 1, 2), length.out = 24)
  )

  ts_data <- prepare_crime_ts(data, "period", "crimes", "month")
  result <- generate_forecast(ts_data, "month", horizon = 6)
  comparison <- build_forecast_comparison(
    ts_data = ts_data,
    forecast_tbl = result$forecast,
    model_tbl = result$models,
    period_type = "month",
    horizon = 6,
    same_threshold = 0.05,
    same_minimum_crimes = 5,
    n_simulations = 200
  )

  expect_equal(comparison$comparison_total, 9)
  expect_equal(comparison$same_absolute_band, 5)
  expect_equal(comparison$lower_same_cutoff, 4)
  expect_equal(comparison$upper_same_cutoff, 14)
})


test_that("forecast comparison HTML avoids 0% and 100% labels", {
  comparison <- list(
    available = TRUE,
    forecast_periods = 6,
    same_threshold = 0.05,
    same_minimum_crimes = 5,
    comparison_period_start = as.Date("2024-07-01"),
    comparison_period_end = as.Date("2024-12-31"),
    forecast_period_start = as.Date("2025-01-01"),
    forecast_period_end = as.Date("2025-06-30"),
    probability_higher = 0,
    probability_same = 0.004,
    probability_lower = 0.995,
    lower_same_cutoff = 90,
    upper_same_cutoff = 110,
    comparison_total = 100,
    point_forecast_total = 103
  )

  html <- as.character(build_forecast_comparison_html(comparison, "month"))

  expect_match(html, "less than 1%")
  expect_match(html, "more than 99%")
  expect_no_match(html, ">0%<", perl = TRUE)
  expect_no_match(html, ">100%<", perl = TRUE)
})
