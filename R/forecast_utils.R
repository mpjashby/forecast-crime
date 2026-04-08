# Format all user-facing dates consistently across the app.
format_display_date <- function(x) {
  format(as.Date(x), "%d %b %Y")
}


# Format user-facing dates without leading zeros when they appear in prose.
format_display_date_short <- function(x) {
  trimws(format(as.Date(x), "%e %b %Y"))
}


# Format a date range compactly for prose, omitting the repeated year when both
# dates fall in the same year.
format_display_date_range <- function(start_date, end_date) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  if (identical(start_date, end_date)) {
    return(format_display_date_short(start_date))
  }

  if (format(start_date, "%Y") == format(end_date, "%Y")) {
    return(sprintf(
      "%s to %s",
      trimws(format(start_date, "%e %b")),
      format_display_date_short(end_date)
    ))
  }

  sprintf(
    "%s to %s",
    format_display_date_short(start_date),
    format_display_date_short(end_date)
  )
}


# Simple null-or-empty fallback helper used throughout the app.
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || identical(x, "")) {
    y
  } else {
    x
  }
}


# Render a Bootstrap alert box using the classes already bundled with Shiny.
bootstrap_alert <- function(type = c("info", "warning", "danger"), ...) {
  type <- match.arg(type)

  htmltools::div(
    class = paste("alert", paste0("alert-", type)),
    role = "alert",
    ...
  )
}


# Central configuration for each supported time frequency.
# To change how the app behaves for daily, weekly, monthly, or annual data,
# update the matching block here. This is the main place to manage:
# - singular/plural labels used in the UI
# - the adjective used in messages (for example "weekly")
# - minimum history thresholds
# - sparse-count thresholds
# - maximum recommended forecast horizons
# - how many recent observations are shown on the chart
# If you add a new time frequency later, you should also add matching logic in
# the parsing, frequency-detection, aggregation, and modelling helpers below.
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


# Convert raw dates to the start of their containing day/week/month/year.
period_start_from_date <- function(dates, period_type) {
  dates <- as.Date(dates)

  switch(
    period_type,
    day = dates,
    week = tsibble::yearweek(dates),
    month = tsibble::yearmonth(dates),
    year = parse_year_values(dates),
    stop("Unsupported period type.", call. = FALSE)
  )
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


# Return the set of source periods that should be present for a given target
# period if aggregation is to be considered complete.
expected_source_periods_for_target <- function(
  target_period,
  source_period_type,
  target_period_type
) {
  target_start <- index_to_date(target_period)
  target_end <- period_end_date(target_start, target_period_type)
  calendar_days <- seq.Date(target_start, target_end, by = "day")
  unique(period_start_from_date(calendar_days, source_period_type))
}


# Infer the frequency of a single uploaded time column when the cadence is clear.
detect_frequency_from_column <- function(column) {
  parsed_day <- tryCatch(parse_day_values(column), error = function(e) NULL)

  if (!is.null(parsed_day) && is_complete_parse(parsed_day, column)) {
    parsed_day <- sort(unique(as.Date(parsed_day)))

    if (is_regular_date_sequence(parsed_day, "day")) {
      return("day")
    }

    if (is_regular_date_sequence(parsed_day, "week")) {
      return("week")
    }

    if (is_regular_date_sequence(parsed_day, "month")) {
      return("month")
    }

    if (is_regular_date_sequence(parsed_day, "year")) {
      return("year")
    }
  }

  raw_values <- trimws(as.character(column))
  raw_values <- raw_values[!is.na(raw_values) & raw_values != ""]
  explicit_week_values <- inherits(column, "yearweek") ||
    all(stringr::str_detect(raw_values, "^([0-9]{4})[- ]?[Ww]([0-9]{1,2})$"))

  parsed_week <- tryCatch(parse_week_values(column), error = function(e) NULL)
  if (
    explicit_week_values &&
      !is.null(parsed_week) &&
      is_complete_parse(parsed_week, column)
  ) {
    week_dates <- sort(unique(index_to_date(parsed_week)))
    weekly_spacing <- length(week_dates) >= 2 &&
      all(diff(week_dates) >= 7) &&
      all(as.integer(diff(week_dates)) %% 7 == 0)

    if (is_regular_date_sequence(week_dates, "week") || weekly_spacing) {
      return("week")
    }
  }

  parsed_month <- tryCatch(parse_month_values(column), error = function(e) NULL)
  if (!is.null(parsed_month) && is_complete_parse(parsed_month, column)) {
    month_dates <- sort(unique(index_to_date(parsed_month)))
    month_values_preserved <- length(unique(month_dates)) ==
      length(unique(raw_values))

    if (
      month_values_preserved && is_regular_date_sequence(month_dates, "month")
    ) {
      return("month")
    }
  }

  if (is_strict_numeric_year_column(column)) {
    return("year")
  }

  if (is.numeric(column)) {
    return(NULL)
  }

  parsed_year <- tryCatch(parse_year_values(column), error = function(e) NULL)
  if (!is.null(parsed_year) && is_complete_parse(parsed_year, column)) {
    year_dates <- sort(unique(as.Date(parsed_year)))
    year_values_preserved <- length(unique(year_dates)) ==
      length(unique(raw_values))

    if (year_values_preserved && is_regular_date_sequence(year_dates, "year")) {
      return("year")
    }
  }

  NULL
}


# Infer the overall dataset frequency when exactly one clear frequency is found.
detect_frequency_from_data <- function(data) {
  detected <- purrr::map_chr(
    data,
    function(column) detect_frequency_from_column(column) %||% ""
  )

  detected <- unique(detected[detected != ""])

  if (length(detected) == 1) {
    detected
  } else {
    NULL
  }
}


# Supported public-holiday calendars that can be added to eligible models.
resolve_time_date_holidays <- function(names) {
  lapply(names, getExportedValue, ns = "timeDate")
}


public_holiday_country_catalog <- function() {
  list(
    uk = list(
      label = "United Kingdom",
      flag = "🇬🇧",
      holiday_fun = function(years) timeDate::holidayLONDON(years)
    ),
    us = list(
      label = "United States",
      flag = "🇺🇸",
      holiday_fun = function(years) {
        timeDate::holiday(
          years,
          Holiday = resolve_time_date_holidays(c(
            "USNewYearsDay",
            "USMLKingsBirthday",
            "USPresidentsDay",
            "USMemorialDay",
            "USJuneteenthNationalIndependenceDay",
            "USIndependenceDay",
            "USLaborDay",
            "USColumbusDay",
            "USVeteransDay",
            "USThanksgivingDay",
            "USChristmasDay"
          ))
        )
      }
    ),
    ca = list(
      label = "Canada",
      flag = "🇨🇦",
      holiday_fun = function(years) {
        timeDate::holiday(
          years,
          Holiday = resolve_time_date_holidays(c(
            "NewYearsDay",
            "GoodFriday",
            "CAVictoriaDay",
            "CACanadaDay",
            "CACivicProvincialHoliday",
            "CALabourDay",
            "CAThanksgivingDay",
            "CaRemembranceDay",
            "ChristmasDay",
            "BoxingDay"
          ))
        )
      }
    ),
    fr = list(
      label = "France",
      flag = "🇫🇷",
      holiday_fun = function(years) {
        timeDate::holiday(
          years,
          Holiday = resolve_time_date_holidays(c(
            "NewYearsDay",
            "EasterMonday",
            "LaborDay",
            "FRFetDeLaVictoire1945",
            "FRAscension",
            "FRBastilleDay",
            "FRAssumptionVirginMary",
            "AllSaints",
            "FRArmisticeDay",
            "ChristmasDay"
          ))
        )
      }
    ),
    de = list(
      label = "Germany",
      flag = "🇩🇪",
      holiday_fun = function(years) {
        timeDate::holiday(
          years,
          Holiday = resolve_time_date_holidays(c(
            "NewYearsDay",
            "GoodFriday",
            "EasterMonday",
            "LaborDay",
            "DEAscension",
            "DECorpusChristi",
            "DEGermanUnity",
            "ChristmasDay",
            "BoxingDay"
          ))
        )
      }
    ),
    it = list(
      label = "Italy",
      flag = "🇮🇹",
      holiday_fun = function(years) {
        timeDate::holiday(
          years,
          Holiday = resolve_time_date_holidays(c(
            "NewYearsDay",
            "ITEpiphany",
            "EasterSunday",
            "EasterMonday",
            "ITLiberationDay",
            "LaborDay",
            "ITAssumptionOfVirginMary",
            "ITAllSaints",
            "ITImmaculateConception",
            "ChristmasDay",
            "BoxingDay"
          ))
        )
      }
    ),
    jp = list(
      label = "Japan",
      flag = "🇯🇵",
      holiday_fun = function(years) {
        timeDate::holiday(
          years,
          Holiday = resolve_time_date_holidays(c(
            "JPNewYearsDay",
            "JPComingOfAgeDay",
            "JPNatFoundationDay",
            "JPVernalEquinox",
            "JPGreeneryDay",
            "JPConstitutionDay",
            "JPChildrensDay",
            "JPMarineDay",
            "JPMountainDay",
            "JPRespectForTheAgedDay",
            "JPAutumnalEquinox",
            "JPHealthandSportsDay",
            "JPNationalCultureDay",
            "JPThanksgivingDay",
            "JPEmperorsBirthday"
          ))
        )
      }
    ),
    ch = list(
      label = "Switzerland",
      flag = "🇨🇭",
      holiday_fun = function(years) {
        timeDate::holiday(
          years,
          Holiday = resolve_time_date_holidays(c(
            "NewYearsDay",
            "CHBerchtoldsDay",
            "GoodFriday",
            "EasterMonday",
            "LaborDay",
            "CHAscension",
            "CHSechselaeuten",
            "CHConfederationDay",
            "CHKnabenschiessen",
            "ChristmasDay",
            "BoxingDay"
          ))
        )
      }
    )
  )
}


# Labels for the Step 1 holiday-country dropdown.
public_holiday_country_choices <- function(include_placeholder = TRUE) {
  catalog <- public_holiday_country_catalog()
  choices <- vapply(
    catalog,
    function(country) sprintf("%s %s", country$flag, country$label),
    character(1)
  )
  values <- names(catalog)

  if (include_placeholder) {
    c("Choose a country" = "", stats::setNames(values, choices))
  } else {
    stats::setNames(values, choices)
  }
}


# Return all public-holiday dates for the selected supported country.
public_holiday_dates <- function(years, country) {
  catalog <- public_holiday_country_catalog()

  if (!country %in% names(catalog)) {
    stop("Unsupported public-holiday country.", call. = FALSE)
  }

  dates <- catalog[[country]]$holiday_fun(sort(unique(as.integer(years))))
  sort(unique(as.Date(dates)))
}


# Count how many public holidays fall within each modelled period.
build_public_holiday_regressor <- function(index, period_type, country) {
  start_dates <- index_to_date(index)
  end_dates <- period_end_date(start_dates, period_type)
  years <- seq.int(
    min(lubridate::year(start_dates), lubridate::year(end_dates)),
    max(lubridate::year(start_dates), lubridate::year(end_dates))
  )
  holiday_dates <- public_holiday_dates(years, country)

  vapply(
    seq_along(start_dates),
    function(i) {
      sum(holiday_dates >= start_dates[[i]] & holiday_dates <= end_dates[[i]])
    },
    integer(1)
  )
}


# Add a public-holiday regressor to the prepared tsibble when requested.
add_public_holiday_regressor <- function(ts_data, period_type, country = NULL) {
  if (is.null(country) || identical(country, "")) {
    return(ts_data)
  }

  ts_data |>
    dplyr::mutate(
      public_holiday_count = build_public_holiday_regressor(
        index = index,
        period_type = period_type,
        country = country
      )
    )
}


# Create future predictor values needed to forecast models with holiday terms.
build_forecast_new_data <- function(
  ts_data,
  period_type,
  horizon,
  country = NULL
) {
  future_data <- tsibble::new_data(ts_data, n = horizon)

  if (is.null(country) || identical(country, "")) {
    return(future_data)
  }

  future_data |>
    dplyr::mutate(
      public_holiday_count = build_public_holiday_regressor(
        index = index,
        period_type = period_type,
        country = country
      )
    )
}


# Format short vectors into plain-English lists.
oxford_comma <- function(x) {
  str_flatten_comma(na.omit(x), last = ", and ")
}


# Return the singular or plural time-period label for the horizon input.
format_period_suffix <- function(period_type, n_periods) {
  if (
    is.null(period_type) ||
      identical(period_type, "") ||
      length(period_type) == 0
  ) {
    return("")
  }

  config <- period_config(period_type)

  if (isTRUE(n_periods == 1)) {
    config$singular
  } else {
    config$plural
  }
}


# Escape text so uploaded content cannot inject HTML into the UI.
escape_uploaded_text <- function(x) {
  escaped <- htmltools::htmlEscape(x, attribute = FALSE)
  Encoding(escaped) <- "UTF-8"
  escaped
}


# Sanitize uploaded column names and text values before the rest of the app
# touches them.
sanitize_uploaded_data <- function(data) {
  names(data) <- vapply(names(data), escape_uploaded_text, character(1))

  data |>
    dplyr::mutate(
      dplyr::across(
        where(~ is.character(.x) || is.factor(.x)),
        ~ escape_uploaded_text(as.character(.x))
      )
    )
}


# Build safe selectInput choices that display escaped labels but keep the
# original column names as values.
safe_choice_vector <- function(values, placeholder = NULL) {
  values <- as.character(values)
  labels <- vapply(values, escape_uploaded_text, character(1))
  choices <- stats::setNames(values, labels)

  if (!is.null(placeholder)) {
    c(stats::setNames("", placeholder), choices)
  } else {
    choices
  }
}


# Read a CSV file of uploaded crime counts.
read_crime_data <- function(path) {
  if (
    !is.character(path) ||
      length(path) != 1 ||
      is.na(path) ||
      !file.exists(path)
  ) {
    stop("The uploaded file could not be read safely.", call. = FALSE)
  }

  file_info <- file.info(path)
  if (is.na(file_info$size) || file_info$size <= 0) {
    stop("The uploaded CSV file is empty.", call. = FALSE)
  }

  raw_header <- readBin(path, what = "raw", n = min(file_info$size, 4096))
  raw_values <- as.integer(raw_header)
  disallowed_control_bytes <- raw_values < 32L &
    !raw_values %in% c(9L, 10L, 13L)
  has_binary_bytes <- any(disallowed_control_bytes)

  if (has_binary_bytes) {
    stop(
      "The uploaded file does not look like plain-text CSV data.",
      call. = FALSE
    )
  }

  data <- readr::read_csv(
    path,
    show_col_types = FALSE,
    progress = FALSE,
    name_repair = "minimal"
  )

  sanitize_uploaded_data(data)
}


# A POSIXt column can be treated like a date column only when every timestamp
# falls exactly at midnight.
datetime_is_midnight_only <- function(x) {
  if (!inherits(x, "POSIXt")) {
    return(FALSE)
  }

  non_missing <- !is.na(x)
  if (!any(non_missing)) {
    return(FALSE)
  }

  all(
    lubridate::hour(x[non_missing]) == 0 &
      lubridate::minute(x[non_missing]) == 0 &
      floor(lubridate::second(x[non_missing])) == 0
  )
}


# Parse a time column as daily dates.
parse_day_values <- function(x) {
  if (inherits(x, "Date")) {
    return(as.Date(x))
  }

  if (inherits(x, "POSIXt")) {
    if (!datetime_is_midnight_only(x)) {
      return(as.Date(rep(NA_character_, length(x))))
    }

    return(as.Date(x))
  }

  values <- trimws(as.character(x))
  values[values == ""] <- NA_character_

  parsed <- suppressWarnings(
    lubridate::parse_date_time(
      values,
      orders = c(
        "Y-m-d",
        "Y/m/d",
        "Ymd",
        "Y-m-d H:M:S",
        "Y-m-d H:M",
        "Y/m/d H:M:S",
        "Y/m/d H:M",
        "Y-m-d H:M:S z",
        "Y-m-d H:M z",
        "Y/m/d H:M:S z",
        "Y/m/d H:M z",
        "Ymd HMS",
        "Ymd HM",
        "ymd HMS",
        "ymd HM",
        "ymdTz",
        "ymdTz!",
        "d-b-Y",
        "d-b-Y H:M:S",
        "d-b-Y H:M",
        "d b Y",
        "d b Y H:M:S",
        "d b Y H:M",
        "d B Y",
        "d B Y H:M:S",
        "d B Y H:M",
        "Y b d",
        "Y b d H:M:S",
        "Y b d H:M",
        "Y B d",
        "Y B d H:M:S",
        "Y B d H:M",
        "d/m/Y",
        "d/m/Y H:M:S",
        "d/m/Y H:M",
        "d/m/Y H:M:S z",
        "d/m/Y H:M z",
        "m/d/Y",
        "m/d/Y H:M:S",
        "m/d/Y H:M",
        "m/d/Y H:M:S z",
        "m/d/Y H:M z"
      ),
      tz = "UTC",
      quiet = TRUE
    )
  )

  as.Date(parsed, tz = "UTC")
}


# Convert ISO week notation to the first day of the requested week.
iso_week_start <- function(year, week) {
  jan_fourth <- as.Date(sprintf("%04d-01-04", year))
  week_one_start <- jan_fourth -
    (lubridate::wday(jan_fourth, week_start = 1) - 1)
  week_one_start + lubridate::weeks(week - 1)
}


# Parse a time column as weekly periods, allowing either dates or YYYY-Wnn text.
parse_week_values <- function(x) {
  if (inherits(x, "Date")) {
    return(tsibble::yearweek(x))
  }

  if (inherits(x, "POSIXt")) {
    if (!datetime_is_midnight_only(x)) {
      return(tsibble::yearweek(as.Date(rep(NA_character_, length(x)))))
    }

    return(tsibble::yearweek(as.Date(x)))
  }

  if (inherits(x, "yearweek")) {
    return(x)
  }

  values <- trimws(as.character(x))
  values[values == ""] <- NA_character_

  parsed_day <- parse_day_values(values)
  out <- tsibble::yearweek(parsed_day)

  missing_idx <- which(is.na(out) & !is.na(values))
  if (length(missing_idx) == 0) {
    return(out)
  }

  week_pattern <- "^([0-9]{4})[- ]?[Ww]([0-9]{1,2})$"
  matches <- stringr::str_match(values[missing_idx], week_pattern)
  matched <- !is.na(matches[, 1])

  if (any(matched)) {
    starts <- purrr::map2(
      as.integer(matches[matched, 2]),
      as.integer(matches[matched, 3]),
      iso_week_start
    ) |>
      unlist() |>
      as.Date(origin = "1970-01-01")

    out[missing_idx[matched]] <- tsibble::yearweek(starts)
  }

  out
}


# Parse a time column as monthly periods.
parse_month_values <- function(x) {
  if (inherits(x, "Date")) {
    return(tsibble::yearmonth(x))
  }

  if (inherits(x, "POSIXt")) {
    if (!datetime_is_midnight_only(x)) {
      return(tsibble::yearmonth(as.Date(rep(NA_character_, length(x)))))
    }

    return(tsibble::yearmonth(as.Date(x)))
  }

  if (inherits(x, "yearmonth")) {
    return(x)
  }

  values <- trimws(as.character(x))
  values[values == ""] <- NA_character_

  parsed <- suppressWarnings(
    lubridate::parse_date_time(
      values,
      orders = c(
        "Y-m",
        "Ym",
        "Y/m",
        "b Y",
        "B Y",
        "Y b",
        "Y B",
        "Ymd",
        "Y-m-d",
        "Y/m/d",
        "d/m/Y",
        "m/d/Y"
      ),
      quiet = TRUE
    )
  )

  tsibble::yearmonth(as.Date(parsed))
}


# Parse a time column as annual periods.
parse_year_values <- function(x) {
  if (inherits(x, "Date")) {
    return(as.Date(format(as.Date(x), "%Y-01-01")))
  }

  if (inherits(x, "POSIXt")) {
    if (!datetime_is_midnight_only(x)) {
      return(as.Date(rep(NA_character_, length(x))))
    }

    return(as.Date(format(as.Date(x), "%Y-01-01")))
  }

  values <- trimws(as.character(x))
  values[values == ""] <- NA_character_

  year_only <- suppressWarnings(as.integer(values))
  out <- as.Date(rep(NA_character_, length(values)))
  valid_year <- !is.na(year_only) & year_only >= 1000 & year_only <= 9999

  if (any(valid_year)) {
    out[valid_year] <- as.Date(sprintf("%04d-01-01", year_only[valid_year]))
  }

  missing_idx <- which(is.na(out) & !is.na(values))
  if (length(missing_idx) > 0) {
    parsed <- parse_day_values(values[missing_idx])
    out[missing_idx] <- as.Date(format(parsed, "%Y-01-01"))
  }

  out
}


# Dispatch to the appropriate parser for the selected time frequency.
parse_period_column <- function(x, period_type) {
  switch(
    period_type,
    day = parse_day_values(x),
    week = parse_week_values(x),
    month = parse_month_values(x),
    year = parse_year_values(x),
    stop("Unsupported period type.", call. = FALSE)
  )
}


# Check whether all non-missing values in a column were parsed successfully.
is_complete_parse <- function(parsed, original) {
  non_missing <- !is.na(original) & trimws(as.character(original)) != ""

  if (!any(non_missing)) {
    return(FALSE)
  }

  all(!is.na(parsed[non_missing]))
}


# Find the columns that can be interpreted as time columns for a given frequency.
detect_time_columns <- function(data, period_type) {
  names(data)[vapply(
    data,
    function(column) {
      parsed <- tryCatch(
        parse_period_column(column, period_type),
        error = function(e) NULL
      )
      if (is.null(parsed)) {
        return(FALSE)
      }

      is_complete_parse(parsed, column)
    },
    logical(1)
  )]
}


# Find numeric columns that could contain crime counts.
detect_count_columns <- function(data) {
  names(data)[vapply(data, is.numeric, logical(1))]
}


# Backwards-compatible wrapper that returns only the prepared tsibble.
prepare_crime_ts <- function(data, time_col, count_col, period_type) {
  prepare_crime_input(data, time_col, count_col, period_type)$ts_data
}


# Parse, validate, and if necessary aggregate the uploaded data into the time
# series that will actually be used for forecasting. This helper also records
# metadata about aggregation and partial-period handling for Step 2 messages.
prepare_crime_input <- function(
  data,
  time_col,
  count_col,
  period_type,
  source_period_type = NULL
) {
  if (!time_col %in% names(data)) {
    stop(
      "Selected time-period column was not found in the uploaded data.",
      call. = FALSE
    )
  }

  if (!count_col %in% names(data)) {
    stop(
      "Selected count column was not found in the uploaded data.",
      call. = FALSE
    )
  }

  if (!is.numeric(data[[count_col]])) {
    stop("The selected crime-count column must be numeric.", call. = FALSE)
  }

  if (
    inherits(data[[time_col]], "POSIXt") &&
      !datetime_is_midnight_only(data[[time_col]])
  ) {
    stop(
      paste(
        "The selected time-period column contains date-time values with times other than midnight.",
        "This app can only handle daily, weekly, monthly, or annual data.",
        "Please choose a date column instead or remove the time-of-day information from the uploaded data."
      ),
      call. = FALSE
    )
  }

  index <- parse_period_column(data[[time_col]], period_type)
  counts <- data[[count_col]]

  if (any(is.na(index))) {
    stop(
      paste(
        "Some values in the selected time-period column could not be",
        "understood as valid dates or time periods of the selected frequency.",
        "Please check that you selected the correct time-period column, that",
        "the time frequency matches the data, and that every row in that",
        "column contains a valid value in the same format."
      ),
      call. = FALSE
    )
  }

  if (any(is.na(counts))) {
    stop(
      "The selected crime-count column contains missing values. Forecasts cannot be generated from data with missing values.",
      call. = FALSE
    )
  }

  if (any(!is.finite(counts))) {
    stop(
      "The selected crime-count column contains non-finite values. Forecasts cannot be generated from data with non-finite values.",
      call. = FALSE
    )
  }

  if (any(counts < 0)) {
    stop(
      "The crime-count column in the data contains negative values. Forecasts cannot be generated from data with negative values.",
      call. = FALSE
    )
  }

  detected_source_period <- source_period_type %||%
    detect_frequency_from_column(data[[time_col]])

  if (is.null(detected_source_period)) {
    stop(
      paste(
        "The selected time-period column does not imply a regular daily, weekly,",
        "monthly, or annual frequency. This usually means there are missing periods",
        "in the data or the time values are irregular. Please correct the time",
        "column so that every period is present before forecasting."
      ),
      call. = FALSE
    )
  }

  parse_period_type <- detected_source_period

  if (!can_aggregate_nested(parse_period_type, period_type)) {
    stop(
      paste(
        "The selected time frequency cannot be created safely from the uploaded data.",
        "This app can aggregate daily data to weekly, monthly, or annual periods,",
        "and it can aggregate weekly or monthly data to annual periods.",
        "It cannot safely aggregate weekly data to months because some weeks span more than one month.",
        "Please choose the same frequency as the uploaded data or a coarser nested frequency."
      ),
      call. = FALSE
    )
  }

  source_index <- parse_period_column(data[[time_col]], parse_period_type)

  observed_source_tbl <- tibble::tibble(
    index = source_index,
    count = as.numeric(counts)
  ) |>
    dplyr::group_by(index) |>
    dplyr::summarise(count = sum(count), .groups = "drop") |>
    dplyr::arrange(index)

  source_complete_index <- build_complete_index(
    min(index_to_date(observed_source_tbl$index)),
    max(index_to_date(observed_source_tbl$index)),
    parse_period_type
  )

  missing_source_periods <- setdiff(
    as.character(source_complete_index),
    as.character(observed_source_tbl$index)
  )

  if (length(missing_source_periods) > 0) {
    stop(
      paste(
        "The uploaded data have missing",
        period_config(parse_period_type)$plural,
        "within the series, so forecasts cannot be produced safely.",
        "Please upload data with every period present and no gaps in the middle of the series."
      ),
      call. = FALSE
    )
  }

  source_tbl <- observed_source_tbl |>
    tsibble::as_tsibble(index = index)

  target_tbl <- source_tbl |>
    tibble::as_tibble() |>
    dplyr::mutate(
      target_index = period_start_from_date(index_to_date(index), period_type)
    ) |>
    dplyr::group_by(target_index) |>
    dplyr::summarise(count = sum(count), .groups = "drop") |>
    dplyr::arrange(target_index)

  is_aggregated <- !is.null(detected_source_period) &&
    !is.na(period_rank(detected_source_period)) &&
    period_rank(detected_source_period) < period_rank(period_type)

  partial_initial_period_removed <- FALSE
  partial_final_period_removed <- FALSE
  removed_initial_period_label <- NULL
  removed_final_period_label <- NULL

  if (is_aggregated) {
    observed_target_map <- observed_source_tbl |>
      tibble::as_tibble() |>
      dplyr::mutate(
        target_index = period_start_from_date(index_to_date(index), period_type)
      )

    target_periods <- target_tbl$target_index

    incomplete_target_flags <- vapply(
      target_periods,
      function(target_period) {
        expected_periods <- expected_source_periods_for_target(
          target_period = target_period,
          source_period_type = parse_period_type,
          target_period_type = period_type
        )
        observed_periods <- observed_target_map |>
          dplyr::filter(target_index == target_period) |>
          dplyr::pull(index) |>
          unique()

        !setequal(
          as.character(expected_periods),
          as.character(observed_periods)
        )
      },
      logical(1)
    )

    if (any(incomplete_target_flags)) {
      incomplete_target_positions <- which(incomplete_target_flags)
      allowed_positions <- c(1, length(target_periods))
      unexpected_positions <- setdiff(incomplete_target_positions, allowed_positions)

      if (length(unexpected_positions) == 0) {
        first_target_position <- 1
        final_target_position <- length(target_periods)

        if (first_target_position %in% incomplete_target_positions) {
          partial_initial_period_removed <- TRUE
          removed_initial_period_label <- format_index_value(
            target_periods[[first_target_position]],
            period_type
          )
          target_tbl <- dplyr::slice(target_tbl, -first_target_position)
        }

        if (final_target_position %in% incomplete_target_positions) {
          partial_final_period_removed <- TRUE
          removed_final_period_label <- format_index_value(
            target_periods[[final_target_position]],
            period_type
          )
          target_tbl <- dplyr::slice_head(target_tbl, n = nrow(target_tbl) - 1)
        }
      } else {
        stop(
          paste(
            "The uploaded data do not contain all of the smaller time periods needed",
            "to build complete",
            period_config(period_type)$plural,
            "for forecasting.",
            "This usually means there are missing periods within the data or that the",
            "data start or end part-way through a larger period. Please upload data",
            "with all source periods present or choose a different time frequency."
          ),
          call. = FALSE
        )
      }
    }

    if (nrow(target_tbl) == 0) {
      stop(
        paste(
          "The uploaded data do not contain any complete",
          period_config(period_type)$plural,
          "after removing partial periods at the start or end. Please upload a longer",
          "series or choose a finer time frequency."
        ),
        call. = FALSE
      )
    }
  }

  complete_index <- build_complete_index(
    min(index_to_date(target_tbl$target_index)),
    max(index_to_date(target_tbl$target_index)),
    period_type
  )

  if (identical(period_type, "year")) {
    complete_index <- lubridate::year(index_to_date(complete_index))
    target_tbl <- target_tbl |>
      dplyr::mutate(target_index = lubridate::year(index_to_date(target_index)))
  }

  ts_data <- tibble::tibble(index = complete_index) |>
    dplyr::left_join(
      dplyr::rename(target_tbl, index = target_index),
      by = "index"
    ) |>
    dplyr::mutate(count = dplyr::coalesce(count, 0)) |>
    tsibble::as_tsibble(index = index)

  if (!identical(period_type, "year")) {
    ts_data <- ts_data |>
      tsibble::fill_gaps(count = 0)
  }

  list(
    ts_data = ts_data,
    metadata = list(
      source_period_type = detected_source_period,
      target_period_type = period_type,
      is_aggregated = is_aggregated,
      partial_initial_period_removed = partial_initial_period_removed,
      partial_final_period_removed = partial_final_period_removed,
      removed_initial_period_label = removed_initial_period_label,
      removed_final_period_label = removed_final_period_label
    )
  )
}


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


# Fit an ensemble of forecasting models. The exact ensemble adapts to the data
# frequency and the amount of available history so the app can still forecast
# sensibly when seasonal models would be inappropriate.
fit_crime_models <- function(ts_data, period_type, holiday_country = NULL) {
  n_periods <- nrow(ts_data)
  include_public_holidays <- !is.null(holiday_country) &&
    !identical(holiday_country, "")

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

  if (identical(period_type, "day")) {
    # The weekly seasonal daily ensemble needs more than exactly two weekly
    # cycles. With only 14 daily observations, the STL component warns and the
    # combined forecast can silently collapse to all-NA output.
    if (n_periods <= 14) {
      return(nonseasonal_ensemble(ts_data))
    }

    if (n_periods < 365) {
      return(weekly_daily_ensemble(ts_data))
    }

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

  if (identical(period_type, "year")) {
    return(nonseasonal_ensemble(ts_data))
  }

  # Weekly seasonal models become brittle right at the first annual-cycle
  # boundary, so keep the non-seasonal ensemble until there is a little more
  # than a year of weekly history available.
  if (identical(period_type, "week") && n_periods <= 53) {
    return(nonseasonal_ensemble(ts_data))
  }

  # Monthly seasonal models can also return null/NA forecasts at exact yearly
  # boundaries, so only switch once there is comfortably more than two years of
  # monthly history.
  if (identical(period_type, "month") && n_periods <= 24) {
    return(nonseasonal_ensemble(ts_data))
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

  model_names <- if (
    identical(period_type, "year") ||
      (identical(period_type, "week") && n_periods <= 53) ||
      (identical(period_type, "month") && n_periods <= 24) ||
      (identical(period_type, "day") && n_periods <= 14)
  ) {
    c(
      "a 'näive' model",
      "an ETS (error, trend, seasonality) model",
      "a time-series linear regression (TSLM) model"
    )
  } else {
    c(
      "a seasonal 'näive' model",
      "an ETS (error, trend, seasonality) model",
      "a time-series linear regression (TSLM) model",
      "a time-series decomposition (STL) model"
    )
  }

  seasonal_variables <- if (identical(period_type, "day")) {
    if (n_periods <= 14) {
      character(0)
    } else if (n_periods < 365) {
      "repeating day-of-week patterns"
    } else {
      c("repeating day-of-week patterns", "repeating patterns across the year")
    }
  } else if (
    identical(period_type, "year") ||
      (identical(period_type, "week") && n_periods <= 53) ||
      (identical(period_type, "month") && n_periods <= 24)
  ) {
    character(0)
  } else if (identical(period_type, "week")) {
    "repeating patterns across the year"
  } else if (identical(period_type, "month")) {
    "repeating patterns across the year"
  } else {
    character(0)
  }

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


# Describe the full historical range covered by a prepared time series.
describe_series_range <- function(ts_data, period_type) {
  start_text <- index_to_date(ts_data$index[[1]])
  end_text <- index_to_date(ts_data$index[[nrow(ts_data)]])
  format_display_date_range(
    start_date = start_text,
    end_date = period_end_date(end_text, period_type)
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

  info_notes <- character()
  warning_notes <- character()

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
    warning_notes <- c(
      warning_notes,
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
    warning_notes <- c(
      warning_notes,
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

  warning_html <- if (length(warning_notes) > 0) {
    bootstrap_alert(
      "warning",
      htmltools::HTML(paste0(warning_notes, collapse = ""))
    )
  } else {
    NULL
  }

  if (length(reliability$warnings) == 0) {
    return(
      htmltools::tagList(
        htmltools::HTML(paste0(
          intro,
          fit_line,
          horizon_line
        )),
        info_html,
        warning_html,
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
    htmltools::HTML(
      paste0(
        intro,
        fit_line,
        horizon_line
      )
    ),
    info_html,
    warning_html,
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
