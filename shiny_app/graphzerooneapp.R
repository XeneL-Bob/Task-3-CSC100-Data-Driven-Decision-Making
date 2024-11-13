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
  battery = "Battery Level",
  peopleCount = "People Count",
  nonNegPeopleCount = "Non-negative People Count",
  count1 = "Count 1",
  nonNegDifCount1 = "Non-negative Difference Count 1",
  count2 = "Count 2",
  nonNegDifCount2 = "Non-negative Difference Count 2",
  activity = "Activity Level",
  nonNegDigActivity = "Non-negative Activity Level",
  peopleIn = "People In",
  peopleOut = "People Out",
  new = "New Events",
  newDiff = "New Difference",
  current = "Current Events",
  currentDiff = "Current Difference",
  total = "Total Events",
  totalDiff = "Total Difference",
  batteryDiff = "Battery Difference",
  humidity = "Humidity",
  humidityDiff = "Humidity Difference",
  temperature = "Temperature",
  temperatureDiff = "Temperature Difference",
  soilMoisture = "Soil Moisture",
  soilMoistureDiff = "Soil Moisture Difference"
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
      .summary-stats { font-size: 1.2em; font-weight: bold; margin-bottom: 10px; }
      .select-input { margin-bottom: 15px; }
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
          dateInput("startDate", "Start Date:", value = NULL),
          dateInput("endDate", "End Date:", value = NULL),
          actionButton("goButton", "Go", class = "btn btn-primary btn-lg"),
          actionButton("resetButton", "Reset", class = "btn btn-secondary btn-lg"),
          br(), br(),
          uiOutput("sliderMenu")
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
          uiOutput("summaryDateRange")
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
    updateDateInput(session, "startDate", value = NULL)
    updateDateInput(session, "endDate", value = NULL)
  })
  
  dataset <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    df <- standardize_columns(df)
    df$datetime <- as.POSIXct(df$datetime)  # Ensure datetime is in correct format
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
    req(input$goButton > 0)
    req(dataset())
    
    datetime_vals <- dataset()$datetime
    min_date <- min(datetime_vals, na.rm = TRUE)
    max_date <- max(datetime_vals, na.rm = TRUE)
    
    sliderInput("dateRange", "Select Time Range:", 
                min = as.POSIXct(min_date), max = as.POSIXct(max_date), 
                value = c(as.POSIXct(min_date), as.POSIXct(max_date)),
                timeFormat = "%Y-%m-%d %H:%M:%S")
  })
  
  observeEvent(dataset(), {
    req(dataset())
    datetime_vals <- dataset()$datetime
    min_date <- min(datetime_vals, na.rm = TRUE)
    max_date <- max(datetime_vals, na.rm = TRUE)
    
    # Update both the Date Inputs and Slider Input when the dataset changes
    updateDateInput(session, "startDate", value = format(min_date, "%Y-%m-%d"))
    updateDateInput(session, "endDate", value = format(max_date, "%Y-%m-%d"))
    
    updateSliderInput(session, "summaryDateRange", 
                      min = as.POSIXct(min_date), max = as.POSIXct(max_date), 
                      value = c(as.POSIXct(min_date), as.POSIXct(max_date)))
  })

  output$plot <- renderPlotly({
    req(input$goButton > 0)
    req(input$variables)
    
    # Get the datetime range and ensure start and end date inputs are valid
    start_date <- as.POSIXct(input$startDate)
    end_date <- as.POSIXct(input$endDate)
    
    if (is.na(start_date) || is.na(end_date) || start_date >= end_date) {
      return(NULL)  # If the dates are invalid, do not plot
    }

    selected_vars <- input$variables
    data <- dataset() %>%
      filter(datetime >= start_date & datetime <= end_date) %>%
      select(datetime, all_of(selected_vars))
    
    if (nrow(data) == 0) {
      return(NULL)  # No data to plot
    }
    
    data_long <- pivot_longer(data, cols = -datetime, names_to = "Variable", values_to = "Value")
    
    y_label <- if (length(selected_vars) == 1) selected_vars[1] else "Selected Variables"
    title <- paste("Relation of", paste(selected_vars, collapse = ", "), "to Date and Time")
    
    p <- ggplot(data_long, aes(x = datetime, y = Value, color = Variable)) +
      geom_line(size = 1) +
      theme_minimal() +
      labs(title = title, x = "Date and Time", y = y_label)
    
    ggplotly(p)
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
    
    sliderInput("summaryDateRange", "Select Date Range for Summary:", 
                min = as.POSIXct(min_date), max = as.POSIXct(max_date), 
                value = c(as.POSIXct(min_date), as.POSIXct(max_date)),
                timeFormat = "%Y-%m-%d %H:%M:%S")
  })
  
  # Ensure the summary date range slider updates the date input fields correctly
  observeEvent(input$summaryDateRange, {
    req(input$summaryDateRange)

    # Debugging: Print the new values
    print(paste("Slider Start Date:", format(as.POSIXct(input$summaryDateRange[1]), "%Y-%m-%d")))
    print(paste("Slider End Date:", format(as.POSIXCt(input$summaryDateRange[2]), "%Y-%m-%d")))

    # Update start and end date fields based on the slider's values
    updateDateInput(session, "startDate", value = format(as.POSIXct(input$summaryDateRange[1]), "%Y-%m-%d"))
    updateDateInput(session, "endDate", value = format(as.POSIXct(input$summaryDateRange[2]), "%Y-%m-%d"))
  })
  
  output$summaryStatsTable <- renderTable({
    req(input$overviewVariables, input$summaryDateRange)
    
    data <- dataset() %>% 
      filter(datetime >= as.POSIXct(input$summaryDateRange[1]) & 
             datetime <= as.POSIXct(input$summaryDateRange[2]))
    
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
      filter(datetime >= as.POSIXct(input$summaryDateRange[1]) & 
             datetime <= as.POSIXct(input$summaryDateRange[2])) %>%
      select(datetime, all_of(input$overviewVariables)) %>%
      pivot_longer(-datetime, names_to = "Variable", values_to = "Value")
    
    p <- ggplot(data, aes(x = datetime, y = Value, color = Variable)) +
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
      filter(datetime >= as.POSIXct(input$summaryDateRange[1]) & 
             datetime <= as.POSIXct(input$summaryDateRange[2])) %>%
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
      filter(datetime >= as.POSIXct(input$summaryDateRange[1]) & 
             datetime <= as.POSIXct(input$summaryDateRange[2])) %>%
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