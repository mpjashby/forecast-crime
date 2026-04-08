# This file downloads some real sample crime data that can be used to test the
# app. It is not used by the app itself, but can be run separately to get some
# data to test with.

pacman::p_load(crimedata, here, tidyverse, tsibble)

# Get Louisville crime data from 2010 to 2019
louisville_crime <- get_crime_data(
  years = 2010:2019,
  cities = "Louisville",
  type = "core"
)

# Make daily counts of residential burglary/breaking & entering
louisville_crime |>
  filter(offense_type == "residential burglary/breaking & entering") |>
  count(offense_date = as_date(date_single), name = "burglary") |>
  drop_na(offense_date, burglary) |>
  as_tsibble(index = offense_date) |>
  fill_gaps(burglary = 0) |>
  as_tibble() |>
  write_csv(here("sample-data", "louisville-daily-burglary.csv"))

# Make weekly counts of criminal damage
louisville_crime |>
  filter(
    offense_type %in%
      c("destruction/damage/vandalism of property (except arson)", "arson")
  ) |>
  count(
    week = floor_date(as_date(date_single), "week"),
    name = "criminal_damage"
  ) |>
  drop_na(week, criminal_damage) |>
  # Remove the first and last weeks, which are incomplete
  slice(2:(n() - 1)) |>
  write_csv(here("sample-data", "louisville-weekly-criminal-damage.csv")) |>
  mutate(week_as_week = yearweek(week)) |>
  write_csv(here(
    "sample-data",
    "louisville-weekly-criminal-damage-week-as-week.csv"
  ))

# Make monthly counts of aggravated assault
louisville_crime |>
  filter(offense_type == "aggravated assault") |>
  count(
    month = floor_date(as_date(date_single), "month"),
    name = "aggravated_assault"
  ) |>
  drop_na(month, aggravated_assault) |>
  write_csv(here("sample-data", "louisville-monthly-aggravated-assault.csv")) |>
  mutate(month_as_yearmonth = yearmonth(month)) |>
  write_csv(here(
    "sample-data",
    "louisville-monthly-aggravated-assault-month-as-yearmonth.csv"
  ))

# Make annual counts of homicide
louisville_crime |>
  filter(offense_type == "murder and nonnegligent manslaughter") |>
  count(year = year(as_date(date_single)), name = "homicides") |>
  drop_na(year, homicides) |>
  write_csv(here("sample-data", "louisville-annual-homicides.csv"))
