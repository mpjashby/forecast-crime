# Forecast Crime

This project provides an interactive web app for forecasting the future frequency of crime based on previous crime counts, using the methods outlined in [Ashby (2023)](https://doi.org/10.21428/cb6ab371.8c79f146). The app is available at https://lesscrime.info/forecast-crime/

The current Shiny app accepts CSV uploads containing one row per period and one count column, supports daily, weekly, and monthly forecasting with the `fable` suite, and exports forecasts with 95% intervals.

