library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyr)
library(DT)

# Define the UI for the app with tabs
ui <- fluidPage(
  titlePanel("Offline Data Visualization with Fixed Time Axis"),
  
  # Tabs for Data Visualization and Overview
  tabsetPanel(
    tabPanel("Data Visualization",
      sidebarLayout(
        sidebarPanel(
          fileInput("file", "Upload CSV File", accept = c(".csv")),
          uiOutput("variableSelect"),     # Checkbox group to select variables for plotting
          actionButton("goButton", "Go"), # Button to trigger the plot
          uiOutput("sliderMenu")          # Slider menu to filter time range
        ),
        mainPanel(
          DTOutput("dataTable"),          # Display the data table
          plotlyOutput("plot")            # Display the interactive plot
        )
      )
    ),
    
    tabPanel("Overview",
      sidebarLayout(
        sidebarPanel(
          uiOutput("overviewVariableSelect"), # Variable selection for the overview tab
          uiOutput("summaryDateRange")        # Date range selection for summary statistics
        ),
        mainPanel(
          verbatimTextOutput("summaryStats"), # Text output for summary statistics
          plotlyOutput("overviewPlot1"),      # Additional plot (e.g., scatter plot)
          plotlyOutput("overviewPlot2")       # Additional plot (e.g., pie chart or histogram)
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive expression to load and read the uploaded CSV file
  dataset <- reactive({
    req(input$file)                 # Ensure a file is uploaded
    read.csv(input$file$datapath)    # Read the CSV file
  })
  
  # Check if "datetime" column exists and select other variables for plotting
  output$variableSelect <- renderUI({
    req(dataset())                   # Ensure dataset is available
    cols <- names(dataset())
    
    if (!"datetime" %in% cols) {
      showNotification("Error: 'datetime' column not found in the dataset.", type = "error")
      return(NULL)
    }
    
    # Exclude datetime column from selectable variables
    variable_choices <- setdiff(cols, "datetime")
    checkboxGroupInput("variables", "Select Variables to Plot:", choices = variable_choices)
  })
  
  # Display the data table
  output$dataTable <- renderDT({
    req(dataset())
    datatable(dataset(), options = list(pageLength = 5))
  })
  
  # Show slider only after "Go" button is clicked
  output$sliderMenu <- renderUI({
    req(input$goButton > 0)          # Only display slider after "Go" is clicked
    req(dataset())
    
    # Get datetime range
    datetime_vals <- dataset()$datetime
    min_date <- min(datetime_vals)
    max_date <- max(datetime_vals)
    
    sliderInput("dateRange", "Select Time Range:", 
                min = as.POSIXct(min_date), max = as.POSIXct(max_date), 
                value = c(as.POSIXct(min_date), as.POSIXct(max_date)),
                timeFormat = "%Y-%m-%d %H:%M:%S")
  })
  
  # Render the interactive plot only after "Go" button is clicked
  output$plot <- renderPlotly({
    req(input$goButton > 0)           # Only plot after "Go" is clicked
    req(input$variables)              # Ensure variables are selected
    req(input$dateRange)              # Ensure slider range is set
    
    # Prepare data for plotting within the selected date range
    data <- dataset() %>%
      filter(as.POSIXct(datetime) >= input$dateRange[1] & as.POSIXct(datetime) <= input$dateRange[2]) %>%
      select(all_of(c("datetime", input$variables)))
    
    # Convert data to long format for easy plotting
    data_long <- pivot_longer(data, cols = -datetime, names_to = "Variable", values_to = "Value")
    
    # Generate the plot
    p <- ggplot(data_long, aes(x = as.POSIXct(datetime), y = Value, color = Variable)) +
      geom_line() +
      theme_minimal() +
      labs(title = "Interactive Visualization of Selected Variables Over Time",
           x = "Datetime", y = "Value")
    
    # Make plot interactive
    ggplotly(p)
  })
  
  # Overview tab: Variable selection and date range
  output$overviewVariableSelect <- renderUI({
    req(dataset())
    variable_choices <- setdiff(names(dataset()), "datetime")
    selectInput("overviewVariable", "Select Variable for Statistics:", choices = variable_choices)
  })
  
  output$summaryDateRange <- renderUI({
    req(dataset())
    datetime_vals <- dataset()$datetime
    min_date <- min(datetime_vals)
    max_date <- max(datetime_vals)
    
    sliderInput("summaryDateRange", "Select Date Range for Summary:", 
                min = as.POSIXct(min_date), max = as.POSIXct(max_date), 
                value = c(as.POSIXct(min_date), as.POSIXct(max_date)),
                timeFormat = "%Y-%m-%d %H:%M:%S")
  })
  
  # Calculate summary statistics and display as text
  output$summaryStats <- renderPrint({
    req(input$overviewVariable, input$summaryDateRange)
    
    data <- dataset() %>%
      filter(as.POSIXct(datetime) >= input$summaryDateRange[1] & as.POSIXct(datetime) <= input$summaryDateRange[2]) %>%
      pull(input$overviewVariable)
    
    list(
      Mean = mean(data, na.rm = TRUE),
      Standard_Deviation = sd(data, na.rm = TRUE),
      Minimum = min(data, na.rm = TRUE),
      Maximum = max(data, na.rm = TRUE)
    )
  })
  
  # Generate additional charts for the Overview tab
  output$overviewPlot1 <- renderPlotly({
    req(input$overviewVariable, input$summaryDateRange)
    data <- dataset() %>%
      filter(as.POSIXct(datetime) >= input$summaryDateRange[1] & as.POSIXct(datetime) <= input$summaryDateRange[2]) %>%
      select(datetime, input$overviewVariable)
    
    # Scatter plot example
    p <- ggplot(data, aes(x = as.POSIXct(datetime), y = .data[[input$overviewVariable]])) +
      geom_point() +
      theme_minimal() +
      labs(title = paste("Scatter Plot of", input$overviewVariable, "Over Time"),
           x = "Datetime", y = input$overviewVariable)
    
    ggplotly(p)
  })
  
  output$overviewPlot2 <- renderPlotly({
    req(input$overviewVariable, input$summaryDateRange)
    data <- dataset() %>%
      filter(as.POSIXct(datetime) >= input$summaryDateRange[1] & as.POSIXct(datetime) <= input$summaryDateRange[2]) %>%
      select(input$overviewVariable)
    
    # Histogram example
    p <- ggplot(data, aes(x = .data[[input$overviewVariable]])) +
      geom_histogram(fill = "blue", color = "black", bins = 30) +
      theme_minimal() +
      labs(title = paste("Histogram of", input$overviewVariable),
           x = input$overviewVariable, y = "Count")
    
    ggplotly(p)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
