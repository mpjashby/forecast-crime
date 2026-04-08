# Upload parsing, column detection, and prepared-series construction helpers.

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


# Infer the frequency of a single uploaded time column when the cadence is clear.
detect_frequency_from_column <- function(
  column,
  parsed_day = NULL,
  parsed_week = NULL,
  parsed_month = NULL,
  parsed_year = NULL
) {
  if (is.null(parsed_day)) {
    parsed_day <- tryCatch(parse_day_values(column), error = function(e) NULL)
  }

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

  if (is.null(parsed_week)) {
    parsed_week <- tryCatch(parse_week_values(column), error = function(e) NULL)
  }

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

  if (is.null(parsed_month)) {
    parsed_month <- tryCatch(parse_month_values(column), error = function(e) NULL)
  }

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

  if (is.null(parsed_year)) {
    parsed_year <- tryCatch(parse_year_values(column), error = function(e) NULL)
  }

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
detect_frequency_from_data <- function(data, profile = NULL) {
  detected <- if (!is.null(profile)) {
    profile$frequency_by_column
  } else {
    purrr::map_chr(
      data,
      function(column) detect_frequency_from_column(column) %||% ""
    )
  }

  detected <- unique(detected[detected != ""])

  if (length(detected) == 1) {
    detected
  } else {
    NULL
  }
}


# Return TRUE when a single header value looks like it belongs in a data column.
header_value_matches_column <- function(value, column) {
  value <- trimws(as.character(value %||% ""))

  if (!nzchar(value)) {
    return(FALSE)
  }

  non_missing <- column[!is.na(column)]
  if (length(non_missing) == 0) {
    return(FALSE)
  }

  if (is.numeric(column)) {
    parsed_numeric <- suppressWarnings(as.numeric(value))
    return(!is.na(parsed_numeric) && is.finite(parsed_numeric))
  }

  if (inherits(column, "Date") || inherits(column, "POSIXt")) {
    return(!is.na(parse_day_values(value)))
  }

  if (inherits(column, "yearweek")) {
    return(!is.na(parse_week_values(value)))
  }

  if (inherits(column, "yearmonth")) {
    return(!is.na(parse_month_values(value)))
  }

  FALSE
}


# Detect a missing header row using only the once-parsed dataset whose first
# data row may have been mistaken for column names.
looks_like_missing_header_from_parsed_data <- function(data_with_header) {
  if (ncol(data_with_header) == 0 || nrow(data_with_header) == 0) {
    return(FALSE)
  }

  column_matches <- vapply(
    seq_len(ncol(data_with_header)),
    function(i) {
      header_value_matches_column(
        names(data_with_header)[[i]],
        data_with_header[[i]]
      )
    },
    logical(1)
  )

  any(column_matches) && all(column_matches)
}


# Generate neutral column names when a CSV appears to be missing its header row.
generate_missing_header_names <- function(data) {
  vapply(
    seq_along(data),
    function(i) {
      column <- data[[i]]

      if (
        inherits(column, "Date") ||
          inherits(column, "POSIXt") ||
          inherits(column, "yearweek") ||
          inherits(column, "yearmonth")
      ) {
        return(sprintf("date_column_%s", i))
      }

      if (is.numeric(column)) {
        return(sprintf("numeric_column_%s", i))
      }

      parsed_dates <- tryCatch(parse_day_values(column), error = function(error) NULL)
      if (!is.null(parsed_dates) && is_complete_parse(parsed_dates, column)) {
        return(sprintf("date_column_%s", i))
      }

      sprintf("text_column_%s", i)
    },
    character(1)
  )
}


# Convert a string header value back into the same column type as parsed data so
# a missing-header CSV can be reconstructed without rereading the file.
coerce_header_value_to_column_type <- function(value, column) {
  value <- as.character(value %||% "")

  if (inherits(column, "Date")) {
    return(parse_day_values(value))
  }

  if (inherits(column, "POSIXt")) {
    parsed <- suppressWarnings(
      lubridate::parse_date_time(
        value,
        orders = c(
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
          "d-b-Y H:M:S",
          "d-b-Y H:M",
          "d b Y H:M:S",
          "d b Y H:M",
          "d B Y H:M:S",
          "d B Y H:M",
          "Y b d H:M:S",
          "Y b d H:M",
          "Y B d H:M:S",
          "Y B d H:M",
          "d/m/Y H:M:S",
          "d/m/Y H:M",
          "d/m/Y H:M:S z",
          "d/m/Y H:M z",
          "m/d/Y H:M:S",
          "m/d/Y H:M",
          "m/d/Y H:M:S z",
          "m/d/Y H:M z"
        ),
        tz = "UTC",
        quiet = TRUE
      )
    )
    return(parsed)
  }

  if (inherits(column, "yearweek")) {
    return(parse_week_values(value))
  }

  if (inherits(column, "yearmonth")) {
    return(parse_month_values(value))
  }

  if (is.numeric(column)) {
    return(suppressWarnings(as.numeric(value)))
  }

  if (is.logical(column)) {
    lower_value <- tolower(trimws(value))
    return(lower_value %in% c("true", "t", "1", "yes"))
  }

  value
}


# Rebuild the dropped first row when a headerless CSV was parsed as if it had
# column names.
reconstruct_headerless_data <- function(data_with_header) {
  reconstructed_first_row <- tibble::as_tibble(
    lapply(
      seq_along(data_with_header),
      function(i) {
        coerce_header_value_to_column_type(
          names(data_with_header)[[i]],
          data_with_header[[i]]
        )
      }
    ),
    .name_repair = "minimal"
  )

  names(reconstructed_first_row) <- names(data_with_header)

  dplyr::bind_rows(reconstructed_first_row, data_with_header)
}


# Parse each uploaded column once so the app can reuse candidate time parses
# and detected source frequencies across UI updates and forecast runs.
profile_data_columns <- function(data) {
  period_types <- c("day", "week", "month", "year")

  profiles <- purrr::imap(
    data,
    function(column, name) {
      parsed_by_period <- purrr::map(
        stats::setNames(period_types, period_types),
        function(period_type) {
          tryCatch(
            parse_period_column(column, period_type),
            error = function(error) NULL
          )
        }
      )

      time_candidates <- vapply(
        period_types,
        function(period_type) {
          parsed <- parsed_by_period[[period_type]]
          !is.null(parsed) && is_complete_parse(parsed, column)
        },
        logical(1)
      )

      detected_frequency <- detect_frequency_from_column(
        column,
        parsed_day = parsed_by_period$day,
        parsed_week = parsed_by_period$week,
        parsed_month = parsed_by_period$month,
        parsed_year = parsed_by_period$year
      )

      list(
        name = name,
        is_numeric = is.numeric(column),
        parsed_by_period = parsed_by_period,
        time_candidates = time_candidates,
        detected_frequency = detected_frequency
      )
    }
  )

  names(profiles) <- names(data)

  list(
    columns = profiles,
    count_columns = names(profiles)[vapply(profiles, `[[`, logical(1), "is_numeric")],
    time_columns = stats::setNames(
      lapply(
        period_types,
        function(period_type) {
          names(profiles)[vapply(
            profiles,
            function(profile) isTRUE(profile$time_candidates[[period_type]]),
            logical(1)
          )]
        }
      ),
      period_types
    ),
    frequency_by_column = stats::setNames(
      vapply(
        profiles,
        function(profile) profile$detected_frequency %||% "",
        character(1)
      ),
      names(profiles)
    )
  )
}


# Read a CSV file of uploaded crime counts.
read_crime_data <- function(path, max_size_bytes = 5 * 1024^2) {
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

  if (!is.null(max_size_bytes) && isTRUE(file_info$size > max_size_bytes)) {
    stop(
      sprintf(
        paste(
          "The uploaded CSV file is too large (%s).",
          "Please upload a file smaller than %s."
        ),
        format_file_size(file_info$size),
        format_file_size(max_size_bytes)
      ),
      call. = FALSE
    )
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

  header_text <- tryCatch(
    rawToChar(raw_header),
    error = function(error) ""
  )
  header_text <- iconv(header_text, from = "", to = "UTF-8", sub = "")
  leading_text <- trimws(header_text)
  leading_text <- sub("^\ufeff", "", leading_text, perl = TRUE)
  leading_text_lower <- tolower(leading_text)

  if (
    startsWith(leading_text_lower, "<?xml") ||
      startsWith(leading_text_lower, "<!doctype") ||
      startsWith(leading_text_lower, "<html") ||
      grepl("^<[[:alpha:]_][^>]*>", leading_text) ||
      startsWith(leading_text, "{") ||
      startsWith(leading_text, "[")
  ) {
    stop(
      paste(
        "The uploaded file does not look like CSV data.",
        "Please upload a plain-text CSV file with one header row and one row per time period."
      ),
      call. = FALSE
    )
  }

  data <- readr::read_csv(
    path,
    show_col_types = FALSE,
    progress = FALSE,
    name_repair = "minimal"
  )

  upload_metadata <- list(
    generated_column_names = FALSE,
    generated_names = character(0)
  )

  if (looks_like_missing_header_from_parsed_data(data)) {
    data <- reconstruct_headerless_data(data)
    names(data) <- generate_missing_header_names(data)
    upload_metadata$generated_column_names <- TRUE
    upload_metadata$generated_names <- names(data)
  }

  data <- sanitize_uploaded_data(data)
  attr(data, "upload_metadata") <- upload_metadata
  data
}


# Find the columns that can be interpreted as time columns for a given frequency.
detect_time_columns <- function(data, period_type, profile = NULL) {
  if (!is.null(profile)) {
    return(profile$time_columns[[period_type]] %||% character(0))
  }

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
detect_count_columns <- function(data, profile = NULL) {
  if (!is.null(profile)) {
    return(profile$count_columns)
  }

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
  source_period_type = NULL,
  parsed_index = NULL,
  parsed_source_index = NULL
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

  if (identical(time_col, count_col)) {
    stop(
      "The time-period column and crime-count column must be different.",
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

  index <- parsed_index %||% parse_period_column(data[[time_col]], period_type)
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

  source_index <- parsed_source_index %||%
    if (identical(parse_period_type, period_type) && !is.null(parsed_index)) {
      parsed_index
    } else {
      parse_period_column(data[[time_col]], parse_period_type)
    }

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
      unexpected_positions <- setdiff(
        incomplete_target_positions,
        allowed_positions
      )

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
