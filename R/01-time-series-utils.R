# Shared time-series configuration and index helpers.

# Central configuration for each supported time frequency.
# To change how the app behaves for daily, weekly, monthly, or annual data,
# update the matching block here. This is the main place to manage:
# - singular/plural labels used in the UI
# - the adjective used in messages (for example "weekly")
# - minimum history thresholds
# - sparse-count thresholds
# - maximum recommended forecast horizons
# - how many recent observations are shown on the chart
period_config <- function(period_type) {
  switch(
    period_type,
    day = list(
      singular = "day",
      plural = "days",
      adjective = "daily",
      min_history = 90,
      sparse_mean_threshold = 5,
      max_horizon = 90,
      recent_points = 14
    ),
    week = list(
      singular = "week",
      plural = "weeks",
      adjective = "weekly",
      min_history = 52,
      sparse_mean_threshold = 5,
      max_horizon = 52,
      recent_points = 12
    ),
    month = list(
      singular = "month",
      plural = "months",
      adjective = "monthly",
      min_history = 24,
      sparse_mean_threshold = 5,
      max_horizon = 12,
      recent_points = 12
    ),
    year = list(
      singular = "year",
      plural = "years",
      adjective = "annual",
      min_history = 8,
      sparse_mean_threshold = 5,
      max_horizon = 5,
      recent_points = 10
    ),
    stop("Unsupported period type.", call. = FALSE)
  )
}


# Default number of future periods to forecast for each time frequency.
default_horizon <- function(period_type) {
  switch(
    period_type,
    day = 28,
    week = 12,
    month = 12,
    year = 3,
    12
  )
}


# Rank time frequencies from finest to coarsest so the app can detect when it
# needs to aggregate uploaded data before forecasting.
period_rank <- function(period_type) {
  switch(
    period_type,
    day = 1L,
    week = 2L,
    month = 3L,
    year = 4L,
    NA_integer_
  )
}


# Only some source/target frequency combinations can be safely nested when
# aggregating to a coarser unit.
can_aggregate_nested <- function(source_period_type, target_period_type) {
  identical(source_period_type, target_period_type) ||
    identical(source_period_type, "day") &&
      target_period_type %in% c("week", "month", "year") ||
    identical(source_period_type, "week") &&
      identical(target_period_type, "year") ||
    identical(source_period_type, "month") &&
      identical(target_period_type, "year")
}


# Check whether a date vector follows a regular daily, weekly, monthly, or
# annual cadence.
is_regular_date_sequence <- function(dates, by) {
  dates <- sort(unique(as.Date(dates)))

  if (length(dates) < 2) {
    return(FALSE)
  }

  expected_dates <- seq.Date(min(dates), by = by, length.out = length(dates))

  length(dates) == length(expected_dates) &&
    all(as.Date(dates) == as.Date(expected_dates))
}


# Return TRUE when a numeric column can be safely interpreted as calendar years.
is_strict_numeric_year_column <- function(column) {
  if (!is.numeric(column)) {
    return(FALSE)
  }

  non_missing <- column[!is.na(column)]

  if (length(non_missing) == 0 || any(!is.finite(non_missing))) {
    return(FALSE)
  }

  whole_years <- floor(non_missing) == non_missing

  if (!all(whole_years)) {
    return(FALSE)
  }

  years <- as.integer(non_missing)
  valid_years <- years >= 1000 & years <= 9999

  if (!all(valid_years)) {
    return(FALSE)
  }

  is_regular_date_sequence(
    as.Date(sprintf("%04d-01-01", sort(unique(years)))),
    "year"
  )
}


# Convert tsibble index classes to plain Date objects for plotting and display.
index_to_date <- function(index) {
  if (is.numeric(index)) {
    years <- suppressWarnings(as.integer(index))
    valid_years <- !is.na(years) & years >= 1000 & years <= 9999

    if (all(valid_years)) {
      return(as.Date(sprintf("%04d-01-01", years)))
    }
  }

  if (inherits(index, "Date")) {
    return(as.Date(index))
  }

  if (inherits(index, "yearweek") || inherits(index, "yearmonth")) {
    return(as.Date(index))
  }

  as.Date(index)
}


# Return the final calendar date covered by a given period.
period_end_date <- function(period_start, period_type) {
  start_date <- index_to_date(period_start)

  switch(
    period_type,
    day = start_date,
    week = start_date + 6,
    month = lubridate::ceiling_date(start_date, "month") - 1,
    year = as.Date(sprintf("%s-12-31", format(start_date, "%Y"))),
    stop("Unsupported period type.", call. = FALSE)
  )
}


# Build a complete sequence of index values between the first and last periods.
build_complete_index <- function(start_date, end_date, period_type) {
  switch(
    period_type,
    day = seq.Date(start_date, end_date, by = "day"),
    week = tsibble::yearweek(seq.Date(start_date, end_date, by = "week")),
    month = tsibble::yearmonth(seq.Date(start_date, end_date, by = "month")),
    year = seq.Date(start_date, end_date, by = "year"),
    stop("Unsupported period type.", call. = FALSE)
  )
}


# Format a time-series index value for display in the UI.
format_index_value <- function(index, period_type) {
  switch(
    period_type,
    day = format_display_date(as.Date(index)),
    week = format_display_date(index_to_date(index)),
    month = format_display_date(index_to_date(index)),
    year = format_display_date(index_to_date(index)),
    as.character(index)
  )
}


# Describe the full historical range covered by a prepared time series.
describe_series_range <- function(ts_data, period_type) {
  start_text <- index_to_date(ts_data$index[[1]])
  end_text <- index_to_date(ts_data$index[[nrow(ts_data)]])
  format_display_date_range(
    start_date = start_text,
    end_date = period_end_date(end_text, period_type)
  )
}
