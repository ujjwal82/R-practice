#install.packages('plotly')
#install.packages('yaml')

library(shiny)
library(mongolite)
library(dplyr)
library(plotly)
library(shinyjs)

## ------------------------  Function definitions ----------------------##
data <- function(query, fields) {
  
  m <- mongo("DeviceData", 
             url = "mongodb://savenindia:saven1ndia@54.196.58.142:27017/DeviceData")
  
  result <- m$find(query= query, fields=fields)
  rm(m)
  gc()
  
  return(result)
}

getDate <- function(date) {
  date <- paste(strsplit(as.character(date), "-")[[1]], collapse = "")
}

## ---------------------------------------------------------------------##

###
# Shiny UI
###
ui <- fluidPage(
  useShinyjs(),
  sidebarLayout(
    div( id ="Sidebar", sidebarPanel(
      
      ###
      # Header text
      ###
      h1("Select Site Name and Machine", align ="center"),
      
      ###
      # Add dropdown list for selection of site
      ###
      selectInput("Site", 
                  label = h3("Choose a Site"),
                  choices = c("JayaShree Unit 1" = 55000,
                              "JayaShree DG"     = 55001,  ## These numbers need to be verified
                              "JayaShree Unit 4" = 55002), ## These numbers need to be verified
                  selected = "JayaShree Unit 4"),
      
      ###
      # Add dropdown list for selection of machine
      ###
      selectInput("Machine", 
                  label = h3("Choose a Machine"),
                  choices = c("Air flow meter of Compressor" = 1,
                              "Phosphating Energy meter" = 2,
                              "Compressor Energy meter" = 3),
                  selected = "Air flow meter of Compressor"),
      
      ###
      # Add date range selection control
      # default set the start date as 100 days prior to current
      #         and end date as current date
      ###
      dateRangeInput(inputId = 'dateRange',
                     label = 'Date range input: yyyy-mm-dd',
                     start = Sys.Date() - 60, end = Sys.Date()
      ),
      
      ###
      # Add output text for debug purpose, this should be removed before final draft
      ###
      textOutput("debug")
    )),
    
    mainPanel(
      
      actionButton("showSidebar", ">>"),
      actionButton("hideSidebar", "<<"),               
      ###
      # Add output plot control to show the graphs.
      ###
      plotOutput("plot"),
      
      ###
      # Add output table control to show the grid of data.
      ###
      tableOutput("data")
    ) 
  )
)

###
# Shiny server.
###
server <- function(input, output, session) {
  
  ###
  # Event handdling
  ###
  observeEvent(input$showSidebar, {
    shinyjs::show(id = "Sidebar")
    shinyjs::hide(id = "showSidebar")
    shinyjs::show(id = "hideSidebar")
  })
  observeEvent(input$hideSidebar, {
    shinyjs::hide(id = "Sidebar")
    shinyjs::hide(id = "hideSidebar")
    shinyjs::show(id = "showSidebar")
  })

  ###
  # Display the user selections on screen.
  ###
  output$debug <- renderText(
    paste("date range selected : ", 
          paste(getDate(input$dateRange[1]), '000000', sep = '' ),
          " to ",
          paste(getDate(input$dateRange[2]), '000000', sep = '' ),
          paste(" Machine : ", input$Machine))
  )
  
  ###
  # Let's work on the user selction, data filteration and Visualizations.
  ###
  userSelectedData <- reactive({
    
    ###
    # Prepare start and end ID range from site, machine and date selections. .
    ###
    startID <- paste(input$Site, input$Machine, getDate(input$dateRange[1]), '000000', sep = '' )
    endID <- paste(input$Site, input$Machine, getDate(input$dateRange[2]), '000000', sep = '' )
    
    outputResult <- data(query = paste('{"ID" : {"$gt" : "', startID,
                                       '", "$lt" : "', endID, '"}, "B":"',
                                       input$Machine, '"}', sep = '')
                         , fields = '{"ID": true, "A": true, "B": true, "DTR": true, "DKWH": true, "KWH": true, "_id": false}'
    )
    
    print(paste('startID : ', startID))
    print(paste('endID : ', endID))
    outputResult$Hour <- substr(x = outputResult$A, 1, 10)
    outputResult$Minute <- substr(x = outputResult$A, 11, 12)
    
    outputResult <- outputResult %>%
                    group_by(Hour) %>%
      summarise(cound = n(), DTR = mean(as.numeric(DTR)))
      
    outputResult
    
  })
  
  output$data <- renderTable({
    energyData <- userSelectedData()
    
  })
  
  output$plot <- renderPlot({
    energyData <- userSelectedData()

    ggplot(energyData, aes(x=Hour,y=DTR)) +
      geom_point(colour = 'blue') +
      theme(axis.text.x = element_text(angle = 80, hjust = 1))
    },height = 400, width = 900)
  
  
}

shinyApp(ui = ui, server = server)