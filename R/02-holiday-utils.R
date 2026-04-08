# Public-holiday lookup and regressor helpers used by the optional holiday
# forecasting feature.

# Supported public-holiday calendars that can be added to eligible models.
resolve_time_date_holidays <- function(names) {
  lapply(names, getExportedValue, ns = "timeDate")
}


holiday_cache <- new.env(parent = emptyenv())


public_holiday_country_catalog <- function() {
  if (!exists("country_catalog", envir = holiday_cache, inherits = FALSE)) {
    holiday_cache$country_catalog <- list(
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

  holiday_cache$country_catalog
}


# Labels for the Step 1 holiday-country dropdown.
public_holiday_country_choices <- function(include_placeholder = TRUE) {
  catalog <- public_holiday_country_catalog()
  choices <- vapply(
    catalog,
    function(country) sprintf("%s %s", country$flag, country$label),
    character(1)
  )

  if (include_placeholder) {
    c("Choose a country" = "", stats::setNames(names(catalog), choices))
  } else {
    stats::setNames(names(catalog), choices)
  }
}


# Return all public-holiday dates for the selected supported country.
public_holiday_dates <- function(years, country) {
  catalog <- public_holiday_country_catalog()

  if (!country %in% names(catalog)) {
    stop("Unsupported public-holiday country.", call. = FALSE)
  }

  years <- sort(unique(as.integer(years)))
  cache_key <- paste(country, paste(years, collapse = ","), sep = "::")

  if (!exists(cache_key, envir = holiday_cache, inherits = FALSE)) {
    dates <- catalog[[country]]$holiday_fun(years)
    holiday_cache[[cache_key]] <- sort(unique(as.Date(dates)))
  }

  holiday_cache[[cache_key]]
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

  if (length(holiday_dates) == 0) {
    return(integer(length(start_dates)))
  }

  start_positions <- findInterval(start_dates - 1, holiday_dates)
  end_positions <- findInterval(end_dates, holiday_dates)
  as.integer(end_positions - start_positions)
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
