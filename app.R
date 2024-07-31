suppressPackageStartupMessages({
	library(bslib)
	library(dplyr)
	library(fable)
	library(feasts)
	library(ggplot2)
	library(gt)
	library(htmltools)
	library(lubridate)
	library(purrr)
	library(readr)
	library(scales)
	library(stringr)
	library(tsibble)
	library(shiny)
})



get_date_col <- function(data) {
	
	date_col_name_i <- data |> 
		sapply(class) |> 
		sapply(first) |> 
		str_which("(Date|POSIXct)")
	
	data |> names() |> pluck(date_col_name_i, 1)
	
}



# Define UI for application that draws a histogram -----------------------------
ui <- page_fixed(
	
	## Application title ----
	card(
		h1("Forecast future crime frequency"),
		markdown(
			"This app forecasts how many crimes are likely to happen in an area in the 
			near future, based on how many crimes occurred there in the recent past. 
			The forecasts are created using an ensemble forecasting model that 
			[Ashby (2023)](https://doi.org/10.21428/cb6ab371.8c79f146) found to be 
			most accurate for forecasting crime.
			
			**Due to limitations in the underlying technology, this app currently 
			works only in Google Chrome.**
			
			Upload your crime data to produce forecasts. Forecasts are produced 
			directly in your web browser – no data is transferred or stored outside 
			your own computer."
		)
	),
	
	## Input and output columns ----
	layout_columns(
		card(
			p(strong("Step 1: upload crime data")),
			markdown(
				"Upload a single CSV file of data in which each row represents a single 
				crime. The file should contain **one** column that shows the date on 
				which each crime occurred."
			),
			fileInput(
				inputId = "datafile",
				label = NULL,
				# label = "Upload a CSV file of crime data",
				accept = ".csv"
			),
			p(strong("Step 2: choose type of forecasts")),
			radioButtons(
				inputId = "periodtype",
				label = NULL,
				choices = c(
					"daily forecasts" = "day",
					"weekly forecasts" = "week", 
					"monthly forecasts" = "month"
				)
				# selected = character(0)
			),
			p(strong("Step 3: choose number of forecasts")),
			p(
				"Choose the number of days/weeks/months into the future to forecast. 
				Forecasts will begin from the next period after the last period in the 
				data you provide."
			),
			numericInput(
				inputId = "periods",
				label = NULL,
				# label = "Number of periods into the future to forecast",
				value = 7,
				min = 0,
				step = 1
			),
			p(strong("Step 4: generate forecasts")),
			actionButton("submit", "Generate forecasts", icon = icon("chart-simple"))
		),
		card(
			accordion(
				accordion_panel(
					"Check forecast accuracy",
					shinycssloaders::withSpinner(htmlOutput("data_check_text")),
					icon = icon("circle-question")
				),
				accordion_panel(
					"View forecasts",
					textOutput("forecast_period"),
					shinycssloaders::withSpinner(plotOutput("plot")),
					icon = icon("chart-simple")
				),
				accordion_panel(
					"Download forecasts",
					htmlOutput("download_text"),
					gt_output("forecast_table"),
					downloadButton("downloadforecast", label = "Download forecasts"),
					icon = icon("download")
				),
				multiple = FALSE
			)
		),
		col_widths = c(5, 7),
		fillable = TRUE
	)
	
)





# Define server logic required to draw a histogram -----------------------------
server <- function(input, output) {
	
	
	
	## Count events ----	
	counts <- reactive({
		
		# Check file extension
		if (tools::file_ext(input$datafile$name) == "csv") {
			file_data <- read_csv(input$datafile$datapath, show_col_types = FALSE)
		} else {
			validate("Uploaded file is not a CSV file. Please upload a file with a `.csv` file extension")
		}

		column_types <- sapply(sapply(file_data, class), first)

		if (sum(column_types %in% c("Date", "POSIXct")) > 1) {
			validate("The data file contains more than one column of dates. Please upload a file containing one column of dates.")
		} else if (sum(column_types %in% c("Date", "POSIXct")) == 0) {
			validate("The data file does not contain a column of dates. Please upload a file containing one column of dates.")
		}

		date_col <- get_date_col(file_data)
		file_data <- rename(file_data, date_use = {{date_col}})

		if (input$periodtype == "week") {
			file_data <- mutate(file_data, date_use = yearweek(date_use))
		} else if (input$periodtype == "month") {
			file_data <- mutate(file_data, date_use = yearmonth(date_use))
		} else {
			file_data <- mutate(file_data, date_use = as_date(date_use))
		}
		
		file_data |> 
			count(date = date_use, name = "events") |> 
			as_tsibble(index = date) |> 
			fill_gaps(events = 0)
		
	}) |> 
		bindEvent(input$submit)
	
	
	
	## Convert period name another format ----
	period_name <- reactive(case_match(
		input$periodtype, 
		"day" ~ "daily", 
		"week" ~ "weekly", 
		"month" ~ "monthly"
	)) |> 
		bindEvent(input$submit)
	
	
	
	## Model frequency ----
	models <- reactive({
		
		model(
			counts(),
			forecast = combination_model(
				ETS(events ~ trend() + season()),
				TSLM(events ~ trend() + season()),
				decomposition_model(STL(events ~ trend() + season()), ETS(season_adjust))
			)
			# forecast = combination_model(
			# 	SNAIVE(events ~ lag()),
			# ETS(events ~ trend() + season()),
			# TSLM(events ~ trend() + season()),
			# decomposition_model(STL(events ~ trend() + season()), ETS(season_adjust))
			# )
		)
		
	}) |> 
		bindEvent(input$submit)
	
	
	
	## Forecast events ----
	forecasts <- reactive({
		
		models() |>
			forecast(h = input$periods) |>
			as_tibble() |>
			select(date, forecast = .mean)
		
	}) |> 
		bindEvent(input$submit)
	
	
	
	## Format text to check forecast accuracy ----
	output$data_check_text <- renderText({
		
		event_counts <- counts()
		first_date <- first(event_counts$date)
		last_date <- last(event_counts$date)
		
		
		### Format text of start/finish dates string ----
		
		if (input$periodtype == "week") {
			date_start <- format(first_date, "%Y, Week %U")
			date_end <- format(last_date, "%Y, Week %U")
		} else if (input$periodtype == "month") {
			date_start <- format(first_date, "%B %Y")
			date_end <- format(last_date, "%B %Y")
		} else {
			date_start <- format(first_date, "%e %B %Y")
			date_end <- format(last_date, "%e %B %Y")
		}
		
		mean_events <- event_counts |> 
			summarise(mean_events = mean(events, na.rm = TRUE)) |> 
			pluck("mean_events", 1) |> 
			round()
		
		text1 <- str_glue(
			"<p>Original data: {scales::comma(nrow(event_counts))} periods of ",
			"{period_name()} data from {date_start} to {date_end} with an average ",
			"of {mean_events} events per {input$periodtype}.</p>"
		)
		
		
		### Check input data ----
		
		data_rows <- nrow(event_counts)
		
		mean_events <- event_counts |> 
			summarise(mean_events = mean(events, na.rm = TRUE)) |> 
			pluck("mean_events", 1) |> 
			round()
		
		if (input$periodtype == "day") {
			next_period <- str_glue(
				"The forecasts may be more accurate if you produced weekly forecasts ",
				"instead."
			)
		} else if (input$periodtype == "week") {
			next_period <- str_glue(
				"The forecasts may be more accurate if you produced monthly forecasts ",
				"instead."
			)
		} else {
			next_period <- ""
		}
		
		if (data_rows < 20) {
			text2 <- str_glue(
				'<p><i class="fa-solid fa-hand" style="color: #CC0000;"></i> 
				<strong style="color: #CC0000;">Only {input$periodtype}s of data have 
				been provided</strong>. Forecasts are likely to be more accurate if 
				historical data are available for 20 or more {input$periodtype}s.</p>'
			)
		} else if (mean_events < 5) {
			text2 <- str_glue(
				'<p><i class="fa-solid fa-hand" style="color: #CC0000;"></i> 
				<strong style="color: #CC0000;">Forecasts of rare events are likely to 
				be unreliable</strong>. The data you have uploaded include an average of 
				{mean_events} events per {input$periodtype}. All else being equal, 
				forecasting becomes more difficult when there are very few events in 
				each period. {next_period}</p>'
			)
		} else {
			text2 <- str_glue(
				'<p><i class="fa-solid fa-thumbs-up"></i> In general, the more data that 
				is provided the more accurate the resulting forecasts will be, with at 
				least 20 {input$periodtype}s of data required for reasonably accurate 
				forecasting.</p>'
			)
		}
		
		
		### Check forecasts ----
		
		forecast_rows <- nrow(forecasts())
		mape <- models() |> accuracy() |> pluck("MAPE", 1)
		
		if (input$periodtype == "day" & forecast_rows > 365) {
			text3 <- str_glue(
				'<p><i class="fa-solid fa-hand" style="color: #CC0000;"></i> 
				<strong style="color: #CC0000;">Forecasting {comma(forecast_rows)} days 
				into the future will be less accurate</strong>. The further into the 
				future you attempt to forecast, the less accurate the forecasts will be 
				because the processes underlying any patterns of crime may change in the 
				meantime. Producing daily forecasts more than 365 days into the future 
				is not advised.</p>'
			)
		} else if (input$periodtype == "week" & forecast_rows > 52) {
			text3 <- str_glue(
				'<p><i class="fa-solid fa-hand" style="color: #CC0000;"></i> 
				<strong style="color: #CC0000;">Forecasting {comma(forecast_rows)} weeks 
				into the future will be less accurate</strong>. The further into the 
				future you attempt to forecast, the less accurate the forecasts will be 
				because the processes underlying any patterns of crime may change in the 
				meantime. Producing weekly forecasts more than 52 weeks into the future 
				is not advised.</p>'
			)
		} else if (input$periodtype == "month" & forecast_rows > 24) {
			text3 <- str_glue(
				'<p><i class="fa-solid fa-hand" style="color: #CC0000;"></i> 
				<strong style="color: #CC0000;">Forecasting {comma(forecast_rows)} 
				months into the future will be less accurate</strong>. The further into 
				the future you attempt to forecast, the less accurate the forecasts will 
				be because the processes underlying any patterns of crime may change in 
				the meantime. Producing monthly forecasts more than 24 months into the 
				future is not advised.</p>'
			)
		} else if (data_rows < forecast_rows) {
			text3 <- str_glue(
				'<p><i class="fa-solid fa-hand" style="color: #CC0000;"></i> 
				<strong style="color: #CC0000;">Forecasting {comma(forecast_rows)} 
				{input$periodtype}s into the future based on only {comma(data_rows)} 
				{input$periodtype}s of data is likely to produce inaccurate 
				forecasts</strong>. Provide data for a longer period or generate 
				forecasts for a shorter period.</p>'
			)
		} else if (mape > 10) {
			text3 <- str_glue(
				'<p><i class="fa-solid fa-hand" style="color: #CC0000;"></i> 
				<strong style="color: #CC0000;">Generated forecasts are likely to be
				inaccurate</strong>. The app has identified patterns in the data 
				provided, but those patterns are unlikely to be consistent enough for 
				reliable forecasting, with differences between forecasts and actual 
				crime counts of about {percent(mape, scale = 1)}. It may be possible to 
				produce more-accurate forecasts manually.</p>'
			)
		} else {
			text3 <- str_glue(
				'<p><i class="fa-solid fa-thumbs-up"></i> Forecasts for 
				{comma(forecast_rows)} {input$periodtype}s into the future are likely to 
				be reasonably accurate based on the data provided.</p>'
			)
		}
		
    ### Combine text ----		
		str_glue("{text1}{text2}{text3}")
		
	}) |> 
		bindEvent(input$submit)
	
	
	
	## Format text of forecast-period string ----
	output$forecast_period <- renderText({
		
		req(input$periodtype)
		
		if (input$periodtype == "week") {
			date_start <- format(first(forecasts()$date), "%Y, Week %U")
			date_end <- format(last(forecasts()$date), "%Y, Week %U")
		} else if (input$periodtype == "month") {
			date_start <- format(first(forecasts()$date), "%B %Y")
			date_end <- format(last(forecasts()$date), "%B %Y")
		} else {
			date_start <- format(first(forecasts()$date), "%e %B %Y")
			date_end <- format(last(forecasts()$date), "%e %B %Y")
		}
		
		str_glue(
			"Forecasts: {scales::comma(nrow(forecasts()))} {period_name()} ",
			"forecasts from {date_start} to {date_end}."
		)
		
	}) |> 
		bindEvent(input$submit)
	
	
	
	## Produce output plot ----
	output$plot <- renderPlot({
		
		event_counts <- counts() |> 
			mutate(date = as_date(date))
		
		event_forecasts <- forecasts() |> 
			mutate(date = as_date(date))
		
		forecast_plus <- counts() |>
			tail(1) |>
			select(date, forecast = events) |> 
			mutate(date = as_date(date)) |>
			bind_rows(event_forecasts)
		
		prev_events_to_show <- min(7, nrow(forecast_plus) - 1, nrow(event_counts))
		
		ggplot() +
			geom_line(
				aes(x = date, y = events, linetype = "A"), 
				data = tail(event_counts, n = prev_events_to_show)
			) +
			geom_line(
				aes(x = date, y = forecast, linetype = "B"), 
				data = forecast_plus
			) +
			geom_point(
				aes(x = date, y = events), 
				tail(event_counts, prev_events_to_show)
			) +
			geom_point(aes(x = date, y = forecast), data = event_forecasts) +
			scale_x_date(expand = expansion(mult = 0.02)) +
			scale_y_continuous(expand = expansion(mult = 0.25)) +
			scale_linetype_manual(
				values = c("A" = "solid", "B" = "12"),
				labels = c("A" = "past events", "B" = "forecasts")
			) +
			labs(
				caption = str_glue(
					"Note: {prev_events_to_show} most recent past event counts are ",
					"shown for comparison with forecasts"
				),
				x = NULL, 
				y = "number of crimes",
				linetype = NULL
			) +
			theme_minimal() +
			theme(
				legend.position = "bottom",
				plot.caption = element_text(hjust = 0),
				plot.caption.position = "plot"
			)
		
	}) |> 
		bindEvent(input$submit)
	
	
	
	## Produce text for download box ----
	output$download_text <- renderText({
		
		download_text <- str_glue(
			"<p>The first 10 forecasts are shown below. To download all forecasts ",
			"as a CSV file, click 'Download forecasts' below.</p>"
		)
		
		if (input$periodtype %in% c("week", "month")) {
			str_glue(
				"{download_text}<p>Note: for {period_name()} forecasts, the dates ",
				"shown in the table and downloaded CSV file are the first day of the ",
				"{input$periodtype}</p>"
			)
		} else {
			download_text
		}
		
	}) |> 
		bindEvent(input$submit)
	
	
	
	## Produce table for download box ----
	output$forecast_table <- render_gt({
		
		forecast_table <- forecasts() |> 
			slice(1:10) |> 
			mutate(
				date = format(date, "%Y-%m-%d"),
				forecast = as.integer(round(forecast))
			) |> 
			gt() |> 
			cols_align("left", columns = date)
		
	}) |> 
		bindEvent(input$submit)
	
	
	
}

# Run the application ----------------------------------------------------------
shinyApp(ui = ui, server = server)
