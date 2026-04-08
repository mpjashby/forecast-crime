# Forecast fitting, diagnostics, and comparison helpers.

# Assess whether the prepared time series is likely to support reliable
# forecasting before the model is fitted.
assess_series <- function(ts_data, period_type, horizon) {
  config <- period_config(period_type)
  n_periods <- nrow(ts_data)
  mean_count <- mean(ts_data$count, na.rm = TRUE)
  zero_share <- mean(ts_data$count == 0, na.rm = TRUE)
  insufficient_history <- n_periods < config$min_history
  sparse_counts <- mean_count < config$sparse_mean_threshold
  long_horizon <- horizon > config$max_horizon
  horizon_vs_history <- horizon > max(1, floor(n_periods / 2))

  warnings <- character()

  if (insufficient_history) {
    warnings <- c(
      warnings,
      sprintf(
        "Only %s %ss were provided; at least %s usually gives more dependable forecasts. Generally speaking, the more data you provide the more accurate the forecasts will be.",
        scales::comma(n_periods),
        config$singular,
        config$min_history
      )
    )
  }

  if (sparse_counts) {
    warnings <- c(
      warnings,
      sprintf(
        "Average crime counts are low (%s per %s), so forecasts may be unstable.",
        round(mean_count, 1),
        config$singular
      )
    )
  }

  if (zero_share > 0.5) {
    warnings <- c(
      warnings,
      "More than half of the historical periods have zero crimes, which makes pattern detection harder."
    )
  }

  if (long_horizon) {
    warnings <- c(
      warnings,
      sprintf(
        "The requested horizon extends beyond the usual recommended limit for %s forecasting.",
        config$adjective
      )
    )
  }

  if (horizon_vs_history) {
    warnings <- c(
      warnings,
      "The forecast horizon is long relative to the amount of history provided. It is generally better to produce forecasts for a shorter period and then produce updated forecasts based on updated data later on."
    )
  }

  list(
    n_periods = n_periods,
    mean_count = mean_count,
    zero_share = zero_share,
    insufficient_history = insufficient_history,
    sparse_counts = sparse_counts,
    long_horizon = long_horizon,
    horizon_vs_history = horizon_vs_history,
    warnings = warnings
  )
}


# Centralise which model family is used for a given dataset so the fitted
# models and the user-facing explanation cannot drift apart.
forecast_model_profile <- function(period_type, n_periods) {
  nonseasonal_names <- c(
    "a 'näive' model",
    "an ETS (error, trend, seasonality) model",
    "a time-series linear regression (TSLM) model"
  )
  seasonal_names <- c(
    "a seasonal 'näive' model",
    "an ETS (error, trend, seasonality) model",
    "a time-series linear regression (TSLM) model",
    "a time-series decomposition (STL) model"
  )

  if (identical(period_type, "year")) {
    return(list(
      key = "nonseasonal",
      model_names = nonseasonal_names,
      seasonal_variables = character(0)
    ))
  }

  if (identical(period_type, "day") && n_periods <= 14) {
    return(list(
      key = "nonseasonal",
      model_names = nonseasonal_names,
      seasonal_variables = character(0)
    ))
  }

  if (identical(period_type, "day") && n_periods < 365) {
    return(list(
      key = "daily_weekly",
      model_names = seasonal_names,
      seasonal_variables = "repeating day-of-week patterns"
    ))
  }

  if (identical(period_type, "day")) {
    return(list(
      key = "daily_full",
      model_names = seasonal_names,
      seasonal_variables = c(
        "repeating day-of-week patterns",
        "repeating patterns across the year"
      )
    ))
  }

  if (identical(period_type, "week") && n_periods <= 53) {
    return(list(
      key = "nonseasonal",
      model_names = nonseasonal_names,
      seasonal_variables = character(0)
    ))
  }

  if (identical(period_type, "month") && n_periods <= 24) {
    return(list(
      key = "nonseasonal",
      model_names = nonseasonal_names,
      seasonal_variables = character(0)
    ))
  }

  list(
    key = "seasonal_generic",
    model_names = seasonal_names,
    seasonal_variables = "repeating patterns across the year"
  )
}


# Fit an ensemble of forecasting models. The exact ensemble adapts to the data
# frequency and the amount of available history so the app can still forecast
# sensibly when seasonal models would be inappropriate.
fit_crime_models <- function(ts_data, period_type, holiday_country = NULL) {
  n_periods <- nrow(ts_data)
  include_public_holidays <- !is.null(holiday_country) &&
    !identical(holiday_country, "")
  model_profile <- forecast_model_profile(period_type, n_periods)

  nonseasonal_ensemble <- function(data) {
    tslm_model <- if (include_public_holidays) {
      fable::TSLM(count ~ trend() + public_holiday_count)
    } else {
      fable::TSLM(count ~ trend())
    }

    fabletools::model(
      data,
      ensemble = fabletools::combination_model(
        fable::NAIVE(count),
        fable::ETS(count ~ error("A") + trend("A") + season("N")),
        tslm_model
      )
    )
  }

  weekly_daily_ensemble <- function(data) {
    tslm_model <- if (include_public_holidays) {
      fable::TSLM(
        count ~ trend() + season(period = "1 week") + public_holiday_count
      )
    } else {
      fable::TSLM(count ~ trend() + season(period = "1 week"))
    }

    fabletools::model(
      data,
      ensemble = fabletools::combination_model(
        fable::SNAIVE(count ~ lag(7)),
        fable::ETS(count ~ trend() + season(period = "1 week")),
        tslm_model,
        fabletools::decomposition_model(
          feasts::STL(count ~ trend() + season(period = "1 week")),
          fable::ETS(season_adjust)
        )
      )
    )
  }

  if (identical(model_profile$key, "daily_full")) {
    tslm_model <- if (include_public_holidays) {
      fable::TSLM(
        count ~ trend() +
          season(period = "1 week") +
          fourier(period = "1 year", K = 2) +
          public_holiday_count
      )
    } else {
      fable::TSLM(
        count ~ trend() +
          season(period = "1 week") +
          fourier(period = "1 year", K = 2)
      )
    }

    return(
      fabletools::model(
        ts_data,
        ensemble = fabletools::combination_model(
          fable::SNAIVE(count ~ lag(7)),
          fable::ETS(count ~ trend() + season(period = "1 week")),
          tslm_model,
          fabletools::decomposition_model(
            feasts::STL(
              count ~
                trend() +
                season(period = "1 week") +
                season(period = "1 year")
            ),
            fable::ETS(season_adjust)
          )
        )
      )
    )
  }

  if (identical(model_profile$key, "nonseasonal")) {
    return(nonseasonal_ensemble(ts_data))
  }

  if (identical(model_profile$key, "daily_weekly")) {
    return(weekly_daily_ensemble(ts_data))
  }

  tslm_model <- if (include_public_holidays) {
    fable::TSLM(count ~ trend() + season() + public_holiday_count)
  } else {
    fable::TSLM(count ~ trend() + season())
  }

  fabletools::model(
    ts_data,
    ensemble = fabletools::combination_model(
      fable::SNAIVE(count ~ lag()),
      fable::ETS(count ~ trend() + season()),
      tslm_model,
      fabletools::decomposition_model(
        feasts::STL(count ~ trend() + season()),
        fable::ETS(season_adjust)
      )
    )
  )
}


# Generate point forecasts and 50/80/95% intervals from the fitted ensemble.
generate_forecast <- function(
  ts_data,
  period_type,
  horizon,
  holiday_country = NULL
) {
  ts_data <- add_public_holiday_regressor(ts_data, period_type, holiday_country)
  model_tbl <- fit_crime_models(ts_data, period_type, holiday_country)
  accuracy_tbl <- fabletools::accuracy(model_tbl)
  future_data <- build_forecast_new_data(
    ts_data = ts_data,
    period_type = period_type,
    horizon = horizon,
    country = holiday_country
  )

  forecast_tbl <- model_tbl |>
    fabletools::forecast(new_data = future_data, level = c(50, 80, 95)) |>
    fabletools::hilo(level = c(50, 80, 95)) |>
    fabletools::unpack_hilo(`50%`, names_sep = "_") |>
    fabletools::unpack_hilo(`80%`, names_sep = "_") |>
    fabletools::unpack_hilo(`95%`, names_sep = "_") |>
    tibble::as_tibble() |>
    dplyr::transmute(
      index,
      period_start = index_to_date(index),
      forecast = pmax(0, .mean),
      lower_50 = pmax(0, `50%_lower`),
      upper_50 = pmax(0, `50%_upper`),
      lower_80 = pmax(0, `80%_lower`),
      upper_80 = pmax(0, `80%_upper`),
      lower_95 = pmax(0, `95%_lower`),
      upper_95 = pmax(0, `95%_upper`)
    )

  forecast_value_columns <- c(
    "forecast",
    "lower_50",
    "upper_50",
    "lower_80",
    "upper_80",
    "lower_95",
    "upper_95"
  )

  if (
    nrow(forecast_tbl) == 0 ||
      any(vapply(forecast_tbl[forecast_value_columns], anyNA, logical(1))) ||
      any(vapply(
        forecast_tbl[forecast_value_columns],
        function(x) any(!is.finite(x)),
        logical(1)
      ))
  ) {
    stop(
      paste(
        "The fitted forecasting models could not produce valid forecast values for this dataset.",
        "This usually means there is not enough history for one or more model components.",
        "Please try forecasting a coarser time frequency or upload a longer series."
      ),
      call. = FALSE
    )
  }

  reliability <- assess_forecast_reliability(
    accuracy_tbl = accuracy_tbl,
    forecast_tbl = forecast_tbl,
    ts_data = ts_data,
    period_type = period_type,
    horizon = horizon
  )

  list(
    models = model_tbl,
    accuracy = accuracy_tbl,
    forecast = forecast_tbl,
    reliability = reliability
  )
}


# Combine input-data diagnostics with model-fit diagnostics into a single
# reliability assessment used by Step 2.
assess_forecast_reliability <- function(
  accuracy_tbl,
  forecast_tbl,
  ts_data,
  period_type,
  horizon
) {
  config <- period_config(period_type)
  assessment <- assess_series(ts_data, period_type, horizon)

  mape <- accuracy_tbl$MAPE[[1]] %||% NA_real_
  mase <- accuracy_tbl$MASE[[1]] %||% NA_real_
  mean_forecast <- mean(forecast_tbl$forecast, na.rm = TRUE)
  mean_interval_width <- mean(
    forecast_tbl$upper_95 - forecast_tbl$lower_95,
    na.rm = TRUE
  )
  width_ratio <- if (isTRUE(mean_forecast > 0)) {
    mean_interval_width / mean_forecast
  } else {
    Inf
  }

  warnings <- assessment$warnings

  if (!is.na(mape) && !is.finite(mape)) {
    warnings <- c(
      warnings,
      "Historical fit could not be summarised with a finite MAPE because the series contains very low counts or many zero-count periods."
    )
  } else if (!is.na(mape) && mape > 20) {
    warnings <- c(
      warnings,
      sprintf(
        "Historical fit is weak (MAPE about %s), so future forecasts may be unreliable.",
        scales::percent(mape / 100, accuracy = 0.1)
      )
    )
  }

  if (!is.na(mase) && mase > 1.5) {
    warnings <- c(
      warnings,
      "Model accuracy is worse than a simple benchmark on the training data."
    )
  }

  if (is.finite(width_ratio) && width_ratio > 2) {
    warnings <- c(
      warnings,
      "The 95% intervals are very wide relative to the forecast level, indicating high uncertainty."
    )
  }

  list(
    summary = assessment,
    mape = mape,
    mase = mase,
    width_ratio = width_ratio,
    reliable = length(warnings) == 0,
    warnings = unique(warnings),
    recommended_recent_points = config$recent_points
  )
}


# Simulate complete future paths so the app can estimate probabilities for
# totals across the whole forecast horizon, not just individual periods.
simulate_forecast_paths <- function(
  model_tbl,
  ts_data,
  period_type,
  horizon,
  holiday_country = NULL,
  n_simulations = 2000L
) {
  future_data <- build_forecast_new_data(
    ts_data = ts_data,
    period_type = period_type,
    horizon = horizon,
    country = holiday_country
  )

  fabletools::generate(
    model_tbl,
    new_data = future_data,
    times = as.integer(n_simulations)
  ) |>
    tibble::as_tibble() |>
    dplyr::transmute(
      .rep,
      index,
      simulated_count = pmax(0, .sim)
    )
}


# Compare the total crime count across the forecast horizon with the total from
# the most recent matching historical window of the same length.
build_forecast_comparison <- function(
  ts_data,
  forecast_tbl,
  model_tbl,
  period_type,
  horizon,
  holiday_country = NULL,
  same_threshold = 0.05,
  same_minimum_crimes = 5,
  n_simulations = 2000L
) {
  observed_tbl <- ts_data |>
    tibble::as_tibble() |>
    dplyr::arrange(index)

  if (nrow(observed_tbl) < horizon) {
    return(
      list(
        available = FALSE,
        reason = sprintf(
          paste(
            "This comparison needs at least %s historical %s so it can match",
            "the %s-period forecast window, but the uploaded data contain only %s."
          ),
          scales::comma(horizon),
          period_config(period_type)$plural,
          scales::comma(horizon),
          scales::comma(nrow(observed_tbl))
        )
      )
    )
  }

  comparison_window <- observed_tbl |>
    dplyr::slice_tail(n = horizon)

  comparison_total <- sum(comparison_window$count, na.rm = TRUE)
  same_absolute_band <- max(
    comparison_total * same_threshold,
    same_minimum_crimes
  )
  lower_same_cutoff <- max(0, comparison_total - same_absolute_band)
  upper_same_cutoff <- comparison_total + same_absolute_band

  simulated_totals <- simulate_forecast_paths(
    model_tbl = model_tbl,
    ts_data = ts_data,
    period_type = period_type,
    horizon = horizon,
    holiday_country = holiday_country,
    n_simulations = n_simulations
  ) |>
    dplyr::group_by(.rep) |>
    dplyr::summarise(
      forecast_total = sum(simulated_count, na.rm = TRUE),
      .groups = "drop"
    )

  point_forecast_total <- sum(forecast_tbl$forecast, na.rm = TRUE)

  list(
    available = TRUE,
    comparison_total = comparison_total,
    point_forecast_total = point_forecast_total,
    forecast_periods = horizon,
    same_threshold = same_threshold,
    same_minimum_crimes = same_minimum_crimes,
    same_absolute_band = same_absolute_band,
    comparison_period_start = index_to_date(comparison_window$index[[1]]),
    comparison_period_end = period_end_date(
      index_to_date(comparison_window$index[[nrow(comparison_window)]]),
      period_type
    ),
    forecast_period_start = forecast_tbl$period_start[[1]],
    forecast_period_end = period_end_date(
      forecast_tbl$period_start[[nrow(forecast_tbl)]],
      period_type
    ),
    lower_same_cutoff = lower_same_cutoff,
    upper_same_cutoff = upper_same_cutoff,
    probability_higher = mean(
      simulated_totals$forecast_total > upper_same_cutoff
    ),
    probability_lower = mean(
      simulated_totals$forecast_total < lower_same_cutoff
    ),
    probability_same = mean(
      simulated_totals$forecast_total >= lower_same_cutoff &
        simulated_totals$forecast_total <= upper_same_cutoff
    )
  )
}
