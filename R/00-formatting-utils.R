# Shared formatting and small utility helpers used across the app.

# Format all user-facing dates consistently across the app.
format_display_date <- function(x) {
  format(as.Date(x), "%d %b %Y")
}


# Format user-facing dates without leading zeros when they appear in prose.
format_display_date_short <- function(x) {
  trimws(format(as.Date(x), "%e %b %Y"))
}


# Format chart-axis dates according to the forecast period being displayed.
format_plot_axis_date <- function(x, period_type) {
  dates <- as.Date(x)

  switch(
    period_type,
    day = trimws(format(dates, "%e %b")),
    week = trimws(format(dates, "%e %b")),
    month = format(dates, "%b %Y"),
    year = format(dates, "%Y"),
    format_display_date_short(dates)
  )
}


# Build approximately evenly spaced date breaks for the forecast plot.
plot_date_breaks <- function(dates, n_labels = 5) {
  dates <- sort(unique(as.Date(dates)))

  if (length(dates) == 0) {
    return(as.Date(character(0)))
  }

  if (length(dates) <= n_labels) {
    return(dates)
  }

  unique(as.Date(seq(min(dates), max(dates), length.out = n_labels)))
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


# Format byte counts into compact user-facing file-size labels.
format_file_size <- function(bytes) {
  if (is.na(bytes) || !is.finite(bytes)) {
    return("unknown size")
  }

  if (bytes < 1024) {
    return(sprintf("%s bytes", scales::comma(round(bytes))))
  }

  if (bytes < 1024^2) {
    return(sprintf("%.1f KiB", bytes / 1024))
  }

  sprintf("%.1f MiB", bytes / 1024^2)
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
