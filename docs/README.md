################################################################################################# Graph 0 One ################################################################################################
##############################################################################################################################################################################################################

# Graph 0 One
**Graph 0 One** is an interactive data visualisation application built using R and Shiny. This tool allows users to upload CSV files and visualize selected variables over specified date ranges with interactive plots and summary statistics.

##############################################################################################################################################################################################################
##############################################################################################################################################################################################################

## Requirements

To run **Graph 0 One**, you need:

- **R Version**: 4.4.1
- **RStudio** (optional, but recommended for easier management and execution of the Shiny application)

### Required R Packages

The application requires the following R packages:

- `shiny` - For building the interactive web application
- `shinythemes` - To apply themes to the Shiny UI
- `ggplot2` - For data visualization
- `dplyr` - For data manipulation
- `plotly` - For interactive plots
- `tidyr` - For reshaping data
- `DT` - For interactive tables
- `rsconnect` (only if deploying to shinyapps.io)


#### Installing Packages

To install all the required packages, open R or RStudio and run:

```
install.packages(c("shiny", "shinythemes", "ggplot2", "dplyr", "plotly", "tidyr", "DT"))
```

If you plan to deploy the application to shinyapps.io, install `rsconnect` as well:

```
install.packages("rsconnect")
```

##############################################################################################################################################################################################################
##############################################################################################################################################################################################################


## Running the Application

### Steps to Run Locally

1. **Download the Source Code**

   - Clone or download the repository containing **Graph 0 One** source files.

2. **Open the Project Folder**

   - Open the project directory containing the main application file, typically named `app.R`.

3. **Open the App in RStudio or R Console**

   - You can run the app directly in RStudio by opening the `app.R` file or the main application file and clicking the "Run App" button.
   - Alternatively, if you are using an R console, navigate to the project folder and run:

   ```
   setwd("path/to/your/project/folder")  # Replace with your actual path
   source("app.R")  # Or "graphzerooneapp.R" if that's your main file name
   ```

4. **Launch the App**

   - Once the application script is executed, it will open a local web browser window with the **Graph 0 One** interface.

5. **Uploading Data**

   - Upload a CSV file using the "Upload CSV File" button.
   - Select the variables you want to visualize and set the date range using either the slider or input boxes.
   - Click "Go" to view the interactive plot and summary statistics.

##############################################################################################################################################################################################################
##############################################################################################################################################################################################################


### Using the Source Folders

If you only have access to the source folders, ensure that the main application script (e.g., `app.R`) and all partial files (such as additional data if used) are in the same directory or correctly referenced.

### Troubleshooting

- **Error: Package Not Found**
   - If you encounter an error about a missing package, install it by running `install.packages("package_name")`, replacing `"package_name"` with the specific package name.

- **Compatibility Issue with R Version**
   - The application is designed for R 4.4.1. Running it on other versions may lead to compatibility issues. You can download and install R 4.4.1 from the [CRAN website](https://cran.r-project.org/bin/windows/base/).

##############################################################################################################################################################################################################
##############################################################################################################################################################################################################

## Deployment to Shinyapps.io (Optional)

If you want to deploy the app to Shinyapps.io for online access, you’ll need an account with [Shinyapps.io](https://www.shinyapps.io/) and the `rsconnect` package installed.

1. **Authenticate with Shinyapps.io**

   Run the following in R to set up your Shinyapps.io account:

   ```
   library(rsconnect)
   rsconnect::setAccountInfo(name='your_username',
                             token='your_token',
                             secret='your_secret')
   ```

2. **Deploy the App**

   Deploy the application by specifying the folder path:

   ```
   rsconnect::deployApp("path/to/your/project/folder")
   ```

   This will upload the app and required packages to Shinyapps.io, making it accessible online.

##############################################################################################################################################################################################################
##############################################################################################################################################################################################################

## Additional Notes

- **Data Privacy**: Note that **Graph 0 One** does not store data permanently. All uploaded data is processed within the session and deleted upon app closure.

############## **This project is open source, the application is free to use and distribute as you please =)** ##############