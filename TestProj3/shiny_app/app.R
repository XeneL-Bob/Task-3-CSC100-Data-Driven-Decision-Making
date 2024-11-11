library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyr)
library(DT)  # For displaying the data table interactively

# Define the UI for the app
ui <- fluidPage(
  titlePanel("Offline Data Visualization with Fixed Time Axis"),
  
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
}

# Run the application
shinyApp(ui = ui, server = server)
