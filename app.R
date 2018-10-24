#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load Required packages
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(fpp2)
library(dplyr)
library(ggplot2)
library(stringr)
library(stargazer)


# Create User Interface ######################################################
ui <- dashboardPage(
  # Create Header for dashboard
  dashboardHeader(title = "Forecasting App"),
  
  # Create Sidebar ###########################################################
  dashboardSidebar( 
    
    # Allow for users to input a file
    fileInput("file1",
              "Choose CSV File",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")),
    
    selectInput("demo", "or select demo data:", 
                choices = c("","Glacier Visits"=2, "Australian Restaurant Sales" = 3)),
    
    # Create a horizontal line element
    tags$hr(),
    
  
    # Select Field to forecast
    selectInput("select", "Select input",
              label = c("Please load a CSV")),
    
    # User Chooses Frequency
    selectizeInput("freq", 
                   "Data Frequency:", 
                   choices = c("Yearly" = 1,
                               "Quartlerly" = 4,
                               "Monthly" = 12,
                               "Daily" = 365), 
                   selected = 12),
    
    # User Chooses Start Date
    dateInput("date","Start Date:", 
              value="2009-01-01", 
              startview = "decade"),
    tags$hr(),
    tags$p(
      
      tags$style('p, li {margin: 15px; font-size:12px; line-height:1.5}
                 a {margin-bottom:10px; font-weight:bold; line-height:2}'),
      tags$p(
        "For more details about the mechanics and mathmatics behind these forecasting 
        techniques, check out the resources below:",
        tags$br(), tags$br(),
      
      tags$a(
        "Forecasting Principles and Practice", 
        href="https://otexts.org/fpp2/", 
        target="_blank"),
      
      tags$br(), 
      
      tags$a("Introduction to ARIMA Forecasting",
             href='https://www.datascience.com/blog/introduction-to-forecasting-with-arima-in-r-learn-data-science-tutorials',
             target="_blank"),
      
      tags$br(), 
      
      tags$a("Intro to Time Series Decomposition",
             href='https://align-alytics.com/seasonal-decomposition-of-time-series-by-loessan-experiment/',
             target="_blank"),
      
      tags$br(), tags$br(),
      
      tags$strong("Disclaimer:"),tags$br(), "This forecasting application is 
      intended for demostration and education. No holdback sample was used. 
      Statistical models cannot account for dramatic changes in market 
      conditions, internal dynamics, and/or acts of the divine." 
    , tags$br()))
  ),
  
  
  # Create Dashboard Body Content ###############################################################
  dashboardBody(
    tags$head(
      
      # Link to style sheet
      tags$link(rel = "stylesheet", 
                type = "text/css", 
                href = "styles.css"),
      
      # Input CSS that won't work through styles?
      tags$style(type='text/css', 
                 ".nav-tabs { font-weight:bold } 
                 td {padding: 5px; font-weight:bold}")),
       h1("ARIMA Time Series Forecast", align = 'center'),

      # Create tabs for main panel ##########################################################
      tabsetPanel(
        
        # Basic plot and summary stats
        tabPanel("Plot",plotOutput("contents") %>% withSpinner(),
                 htmlOutput('summary') %>% withSpinner(), align = "center"),
        
        # JQuery Data Table
        tabPanel("Data",dataTableOutput('table') %>% withSpinner(), align = 'center',padding='10px'),
        
        # ARIMA Forecast
        tabPanel("Forecast", plotOutput('arima_forecast')),
        
        # STL Decomposition
        tabPanel("Decomposition", plotOutput('decomp') %>% withSpinner())
        
        )
    )
)


server <- function(input, output, session) { 
  
  
  glacier <- read.csv('ecns513-data04-glacier_month.csv')
  
  aus <- data.frame(sales = fpp2::auscafe *1000000000, year = trunc(time(fpp2::auscafe)), 
                    month = month.abb[cycle(fpp2::auscafe)])
  
  # Get User Input Data #############################
  data <- reactive({
  
      if(input$demo == "2" & !isTruthy(input$file1)){
        df <- glacier      
        # Update the "Fields" selector based on user data
        updateSelectInput(session, "select",
                          # Remove date fields from forecasting options
                          choices = names(df)[!str_detect(names(df),"(?i)year|month|day|date")],                      
                          label = c('Choose value to Forecast:'))
        
        return(df)
      } else if (input$demo == "3" & !isTruthy(input$file1)) {
        df <- aus
        # Update the "Fields" selector based on user data
        updateSelectInput(session, "select",
                          # Remove date fields from forecasting options
                          choices = names(df)[!str_detect(names(df),"(?i)year|month|day|date")],                      
                          label = c('Choose value to Forecast:'))
        return(df)
      }
          
      # check the user has entered a file
      req(input$file1)
  
      # Get the data
      tryCatch({
        # Read user-provided csv
          df <- read.csv(input$file1$datapath) },
        
        # return a safeError if a parsing error occurs
          error = function(e) {
          stop(safeError(e))
        })  
      
      # Update the "Fields" selector based on user data
      updateSelectInput(session, "select",
                      # Remove date fields from forecasting options
                      choices = names(df)[!str_detect(names(df),"(?i)year|month|day|date")],                      
                      label = c('Choose value to Forecast:'))
      
          return(df)
        
    })
  


  # Create interactive data table #############################################
  output$table <- renderDataTable({
    data()
  })

  # Create SUmmary Statistics
  output$summary <- renderText({
    names = data() %>% names() %>% str_replace_all("^|$","\t \t")
    data() %>% stargazer(type = "html", digits = 0, digits.extra = 2, median =T)
  })
    
  
  
  # Basic Time Series Plot ##########################################
  output$contents <- renderPlot({
    
    # Get data frame
    df <- data()
    
    # Get frequency from UI
    freq <- input$freq %>% as.integer()
    
    # Get start year from UI
    start.year <- input$date %>% str_extract("^....") %>% as.integer()
    
    # Output a Time Series plot
    df[input$select %>% as.character()] %>% 
      ts(frequency = freq, start = start.year) %>% ts.plot()
    
  })
  
  # ARIMA forecast  ###############################################
  output$arima_forecast <- renderPlot({
    
    # Make a modal for when the forecast is being calculated
    showModal(modalDialog("Calculating your forecast... This may take a moment", easyClose = T))
    
    # Get data
    df <- data() 
    
    # Get user inputs
    freq <- input$freq %>% as.integer()
    start.year <- input$date %>% str_extract("^....") %>% as.integer()
    
    # Create ARIMA plot
    plot <- df[input$select] %>%
      ts(frequency = freq, start = start.year) %>% 
      auto.arima() %>% 
      forecast() %>% 
      autoplot()
    
    # Remove Modal (I think you got that...)
    removeModal()
    
    # Output the plot
    plot
  })
  
  # STL Seasonal Decomposition ####################################
  output$decomp <- renderPlot({
    
    # Get data
    df <- data() 
    df <- df[[input$select]]
    
    # Get user inputs
    freq <- input$freq %>% as.integer()
    start.year <- input$date %>% str_extract("^....") %>% as.integer()
    
    # Make and return STL decomposition plot
    ts(df[1:length(df)], frequency = freq, start = start.year) %>%
      stl(t.window=7, s.window="periodic") %>%
      autoplot()
  })

  # Download ARIMA Plot
  output$export = downloadHandler(
    filename = function() {"plots.pdf"},
    content = function(file) {
      ggsave("file", device = "pdf", width=11, height=8.5)
      
    }
  )
  
}



# Run the application 
shinyApp(ui = ui, server = server)

