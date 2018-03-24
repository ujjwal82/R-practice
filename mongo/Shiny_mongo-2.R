library(shinydashboard)


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

## ----------------------------------------------------------------------##

MenuMessage <- dropdownMenu(type = "messages",
             messageItem(
               from = "Sales Dept",
               message = "Sales are steady this month."
             ),
             messageItem(
               from = "New User",
               message = "How do I register?",
               icon = icon("question"),
               time = "13:45"
             ),
             messageItem(
               from = "Support",
               message = "The new server is ready.",
               icon = icon("life-ring"),
               time = "2014-12-01"
             )
)

menuNotification <- dropdownMenu(type = "notifications",
                                 notificationItem(
                                   text = "5 new users today",
                                   icon("users")
                                 ),
                                 notificationItem(
                                   text = "12 items delivered",
                                   icon("truck"),
                                   status = "success"
                                 ),
                                 notificationItem(
                                   text = "Server load at 86%",
                                   icon = icon("exclamation-triangle"),
                                   status = "warning"
                                 )
)

menuTask <- dropdownMenu(type = "tasks", badgeStatus = "success",
                         taskItem(value = 90, color = "green",
                                  "Documentation"
                         ),
                         taskItem(value = 17, color = "aqua",
                                  "Project X"
                         ),
                         taskItem(value = 75, color = "yellow",
                                  "Server deployment"
                         ),
                         taskItem(value = 80, color = "red",
                                  "Overall project"
                         )
)

header <-   dashboardHeader(disable = FALSE, title = "My dashboard", MenuMessage, menuNotification, menuTask)

## Sidebar content
sidebar <- dashboardSidebar( width = 280,
  sidebarMenu(id = "sidebar",
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Dashboard1", tabName = "dashboard1", icon = icon("dashboard")),
    # menuItem("Widgets", icon = icon("th")),
    menuItem("Inputs", tabName = "widgets", icon = icon("filter"),
             ###
             # Add dropdown list for selection of site
             ###
             selectInput("Site", label = h4("Choose a Site"),
                         choices = c("JayaShree Unit 1" = 55000,
                                     "JayaShree DG"     = 55001,  ## These numbers need to be verified
                                     "JayaShree Unit 4" = 55002) ## These numbers need to be verified
                         , selectize=TRUE, selected = "JayaShree Unit 4"
                         , width = '98%'),
             ###
             # Add dropdown list for selection of machine
             ###
             selectInput("Machine", 
                         label = h4("Choose a Machine"),
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
                            label = 'Date range:',
                            start = Sys.Date() - 60, end = Sys.Date(), width = 200
             ),
             actionButton( label = "GO", icon = icon("refresh"), inputId = "filterData")
    )
    
  )
)


## Body content
body <- dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "dashboard"
            , h2("Dashboard tab content")
            
    ),
    
    # First tab content
    tabItem(tabName = "dashboard1"
            , h2("Widgets tab content")
            , fluidRow(
              box( 
                title = "Inputs", status = "warning", solidHeader = TRUE,
                collapsible = TRUE,
                "Box content here", br(), "More box content"
              )
            )
            , fluidRow( 
              ###
              # Add output plot control to show the graphs.
              ###
              box( width = 400,
                   title = "Histogram", status = "primary", solidHeader = TRUE,
                   collapsible = FALSE,
                   plotOutput("plot", height = 400, width = 900)
              )
            )
            
    ),
    
    # Second tab content
    tabItem(tabName = "widgets",
            h2("Widgets tab content")
    )
  )
)

ui <- dashboardPage(header, sidebar, body)


server <- function(input, output, session) {

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
    
    # print(paste('startID : ', startID))
    # print(paste('endID : ', endID))
    outputResult$Hour <- substr(x = outputResult$A, 1, 10)
    outputResult$Minute <- substr(x = outputResult$A, 11, 12)
    
    outputResult <- outputResult %>%
      group_by(Hour) %>%
      summarise(cound = n(), DTR = mean(as.numeric(DTR)))
    
    outputResult
  })

  output$plot <- renderPlot({
    energyData <- userSelectedData()
    
    ggplot(energyData, aes(x=Hour,y=DTR)) +
      geom_point(colour = 'blue') +
      theme(axis.text.x = element_text(angle = 80, hjust = 1))
  })
  
  observeEvent(input$filterData, {
    newtab <- switch(input$sidebar, "widgets" = "dashboard", "dashboard" = "widgets")
    updateTabItems(session = session, "sidebar", "dashboard1")
  })
    
}

shinyApp(ui, server)
