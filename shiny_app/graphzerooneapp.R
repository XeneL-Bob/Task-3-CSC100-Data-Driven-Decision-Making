library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyr)
library(DT)

# Define mappings for user-friendly labels and column names
column_mappings <- list(
  datetime = c("datetime", "date_time"),
  
  # Atmos_Aggregated.csv
  airTemperature = "Air Temperature",
  airTemperatureDiff = "Air Temperature Difference",
  atmosphericPressure = "Atmospheric Pressure",
  atmosphericPressureDiff = "Atmospheric Pressure Difference",
  gustSpeed = "Gust Speed",
  gustSpeedDiff = "Gust Speed Difference",
  precipitation = "Precipitation",
  relativeHumidity = "Relative Humidity",
  relativeHumidityDiff = "Relative Humidity Difference",
  solarRadiation = "Solar Radiation",
  windSpeed = "Wind Speed",
  windSpeedDiff = "Wind Speed Difference",
  windDirection = "Wind Direction",
  windDirectionDiff = "Wind Direction Difference",
  
  # DingTek_Aggregated.csv
  battery = "Battery Level",
  peopleCount = "People Count",
  nonNegPeopleCount = "Non-negative People Count",
  
  # Farmo_Aggregated.csv
  count1 = "Count 1",
  nonNegDifCount1 = "Non-negative Difference Count 1",
  count2 = "Count 2",
  nonNegDifCount2 = "Non-negative Difference Count 2",
  activity = "Activity Level",
  nonNegDigActivity = "Non-negative Activity Level",
  
  # Milesight_Aggregated.csv
  peopleIn = "People In",
  peopleOut = "People Out",
  
  # NCount_Aggregated.csv
  new = "New Events",
  newDiff = "New Difference",
  current = "Current Events",
  currentDiff = "Current Difference",
  total = "Total Events",
  totalDiff = "Total Difference",
  
  # R712_Aggregated.csv
  batteryDiff = "Battery Difference",
  humidity = "Humidity",
  humidityDiff = "Humidity Difference",
  temperature = "Temperature",
  temperatureDiff = "Temperature Difference",
  
  # R718b140_Aggregated.csv
  batteryDiff = "Battery Difference",
  temperature = "Temperature",
  temperatureDiff = "Temperature Difference",
  
  # SMT100a_Aggregated.csv
  soilMoisture = "Soil Moisture",
  soilMoistureDiff = "Soil Moisture Difference",
  temperatureDiff = "Temperature Difference"
)

# Helper function to clean and standardize column names
standardize_columns <- function(df) {
  datetime_cols <- intersect(names(df), unlist(column_mappings$datetime))
  if (length(datetime_cols) > 0) {
    names(df)[names(df) %in% datetime_cols] <- "datetime"
  }
  
  for (col in names(df)) {
    if (col %in% names(column_mappings)) {
      friendly_name <- column_mappings[[col]]
      names(df)[names(df) == col] <- friendly_name
    }
  }
  
  df <- df %>% select(-starts_with("dev_id"), -starts_with("unnecessary_column"))
  
  return(df)
}

# Define the UI for the app with tabs
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  
  tags$head(
    tags$style(HTML("
      .sidebar { background-color: #f7f7f7; padding: 20px; border-radius: 10px; }
      .main-panel { background-color: #ffffff; padding: 20px; border-radius: 10px; box-shadow: 0px 0px 15px rgba(0, 0, 0, 0.1); }
      .data-title { font-size: 1.5em; font-weight: bold; color: #333; }
      .slider-selected-date { font-size: 18px; font-weight: bold; color: #333; margin-top: 10px; }
    "))
  ),
  
  titlePanel(tags$div(class = "data-title", "Graph 0 One")),
  
  tabsetPanel(
    tabPanel("Data Visualisation",
      sidebarLayout(
        sidebarPanel(
          class = "sidebar",
          fileInput("file", "Upload CSV File", accept = c(".csv")),
          uiOutput("variableSelect"),
          actionButton("goButton", "Go", class = "btn btn-primary btn-lg"),
          actionButton("resetButton", "Reset", class = "btn btn-secondary btn-lg"),
          br(), br(),
          uiOutput("sliderMenu"),
          # Display selected date range under the Data Visualization slider
          div(class = "slider-selected-date", textOutput("selectedDateRangeText"))
        ),
        mainPanel(
          class = "main-panel",
          h3("Uploaded Data"),
          DTOutput("dataTable"),
          br(), br(),
          h3("Interactive Plot"),
          plotlyOutput("plot")
        )
      )
    ),
    
    tabPanel("Overview",
      sidebarLayout(
        sidebarPanel(
          class = "sidebar",
          uiOutput("overviewVariableSelect"),
          uiOutput("summaryDateRange"),
          # Display selected date range under the Overview slider
          div(class = "slider-selected-date", textOutput("overviewSelectedDateRangeText"))
        ),
        mainPanel(
          class = "main-panel",
          h3("Summary Statistics"),
          tableOutput("summaryStatsTable"),
          br(), br(),
          h3("Visualisations of Selected Variables"),
          plotlyOutput("overviewPlot1"),
          br(),
          plotlyOutput("overviewPlot2"),
          br(),
          plotlyOutput("overviewPlot3")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  observeEvent(input$resetButton, {
    updateCheckboxGroupInput(session, "variables", selected = character(0))
    updateCheckboxGroupInput(session, "overviewVariables", selected = character(0))
    updateSliderInput(session, "dateRange", value = NULL)
    updateSliderInput(session, "summaryDateRange", value = NULL)
  })
  
  dataset <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    df <- standardize_columns(df)
    df
  })
  
  output$variableSelect <- renderUI({
    req(dataset())
    cols <- setdiff(names(dataset()), "datetime")
    checkboxGroupInput("variables", "Select Variables to Plot:", choices = cols)
  })
  
  output$dataTable <- renderDT({
    req(dataset())
    datatable(dataset(), options = list(pageLength = 5))
  })
  
  output$sliderMenu <- renderUI({
    req(dataset())
    
    datetime_vals <- dataset()$datetime
    min_date <- min(datetime_vals, na.rm = TRUE)
    max_date <- max(datetime_vals, na.rm = TRUE)
    
    tagList(
      sliderInput("dateRange", "Select Time Range:", 
                  min = as.POSIXct(min_date), max = as.POSIXct(max_date), 
                  value = c(as.POSIXct(min_date), as.POSIXct(max_date)),
                  timeFormat = "%Y-%m-%d %H:%M:%S")
    )
  })
  
  output$selectedDateRangeText <- renderText({
    req(input$dateRange)
    start_date <- format(input$dateRange[1], "%Y-%m-%d %H:%M:%S")
    end_date <- format(input$dateRange[2], "%Y-%m-%d %H:%M:%S")
    paste("Currently selected range: ", start_date, " to ", end_date)
  })
  
  output$overviewVariableSelect <- renderUI({
    req(dataset())
    cols <- setdiff(names(dataset()), "datetime")
    checkboxGroupInput("overviewVariables", "Select Variables for Comparison:", choices = cols)
  })
  
  output$summaryDateRange <- renderUI({
    req(dataset())
    datetime_vals <- dataset()$datetime
    min_date <- min(datetime_vals, na.rm = TRUE)
    max_date <- max(datetime_vals, na.rm = TRUE)
    
    tagList(
      sliderInput("summaryDateRange", "Select Date Range for Summary:", 
                  min = as.POSIXct(min_date), max = as.POSIXct(max_date), 
                  value = c(as.POSIXct(min_date), as.POSIXct(max_date)),
                  timeFormat = "%Y-%m-%d %H:%M:%S")
    )
  })
  
  output$overviewSelectedDateRangeText <- renderText({
    req(input$summaryDateRange)
    start_date <- format(input$summaryDateRange[1], "%Y-%m-%d %H:%M:%S")
    end_date <- format(input$summaryDateRange[2], "%Y-%m-%d %H:%M:%S")
    paste("Currently selected range: ", start_date, " to ", end_date)
  })
  
  output$plot <- renderPlotly({
    req(input$goButton > 0)
    req(input$variables)
    req(input$dateRange)
    
    selected_vars <- input$variables
    data <- dataset() %>% 
      filter(as.POSIXct(datetime) >= input$dateRange[1] & as.POSIXct(datetime) <= input$dateRange[2]) %>% 
      select(all_of(c("datetime", selected_vars)))
    
    data_long <- pivot_longer(data, cols = -datetime, names_to = "Variable", values_to = "Value")
    
    y_label <- if (length(selected_vars) == 1) selected_vars[1] else "Selected Variables"
    title <- paste("Relation of", paste(selected_vars, collapse = ", "), "to Date and Time")
    
    p <- ggplot(data_long, aes(x = as.POSIXct(datetime), y = Value, color = Variable)) +
      geom_line(size = 1) +
      theme_minimal() +
      labs(title = title, x = "Date and Time", y = y_label)
    
    ggplotly(p)
  })
  
  output$summaryStatsTable <- renderTable({
    req(input$overviewVariables, input$summaryDateRange)
    
    data <- dataset() %>% 
      filter(as.POSIXct(datetime) >= input$summaryDateRange[1] & as.POSIXct(datetime) <= input$summaryDateRange[2])
    
    summary_table <- lapply(input$overviewVariables, function(var) {
      stats <- data %>%
        select(var) %>%
        summarise(
          Mean = mean(.data[[var]], na.rm = TRUE),
          Std_Dev = sd(.data[[var]], na.rm = TRUE),
          Min = min(.data[[var]], na.rm = TRUE),
          Max = max(.data[[var]], na.rm = TRUE)
        )
      stats <- as.data.frame(stats)
      stats$Variable <- var
      stats
    })
    summary_table <- do.call(rbind, summary_table)
    summary_table <- summary_table[, c("Variable", "Mean", "Std_Dev", "Min", "Max")]
    summary_table
  }, rownames = FALSE)
  
  output$overviewPlot1 <- renderPlotly({
    req(input$overviewVariables, input$summaryDateRange)
    
    selected_vars <- paste(input$overviewVariables, collapse = ", ")
    y_label <- if (length(input$overviewVariables) == 1) input$overviewVariables[1] else "Selected Variables"
    title <- paste("Comparison of", selected_vars, "Over Time")
    
    data <- dataset() %>%
      filter(as.POSIXct(datetime) >= input$summaryDateRange[1] & 
             as.POSIXct(datetime) <= input$summaryDateRange[2]) %>%
      select(datetime, all_of(input$overviewVariables)) %>%
      pivot_longer(-datetime, names_to = "Variable", values_to = "Value")
    
    p <- ggplot(data, aes(x = as.POSIXct(datetime), y = Value, color = Variable)) +
      geom_point(size = 1) +
      theme_minimal() +
      labs(title = title, x = "Date and Time", y = y_label)
    
    ggplotly(p)
  })
  
  output$overviewPlot2 <- renderPlotly({
    req(input$overviewVariables, input$summaryDateRange)
    
    selected_vars <- paste(input$overviewVariables, collapse = ", ")
    y_label <- "Value"
    title <- paste("Boxplot Comparison of", selected_vars)
    
    data <- dataset() %>%
      filter(as.POSIXct(datetime) >= input$summaryDateRange[1] & 
             as.POSIXct(datetime) <= input$summaryDateRange[2]) %>%
      select(all_of(input$overviewVariables)) %>%
      pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")
    
    p <- ggplot(data, aes(x = Variable, y = Value, fill = Variable)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = title, x = "Variable", y = y_label)
    
    ggplotly(p)
  })
  
  output$overviewPlot3 <- renderPlotly({
    req(input$overviewVariables, input$summaryDateRange)
    
    selected_vars <- paste(input$overviewVariables, collapse = ", ")
    x_label <- "Value"
    y_label <- "Frequency"
    title <- paste("Histogram of", selected_vars)
    
    data <- dataset() %>%
      filter(as.POSIXct(datetime) >= input$summaryDateRange[1] & 
             as.POSIXct(datetime) <= input$summaryDateRange[2]) %>%
      select(all_of(input$overviewVariables)) %>%
      pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")
    
    p <- ggplot(data, aes(x = Value, fill = Variable)) +
      geom_histogram(bins = 30, color = "black") +
      theme_minimal() +
      labs(title = title, x = x_label, y = y_label)
    
    ggplotly(p)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
