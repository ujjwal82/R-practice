library(shiny)
library(shinyTime)
source("datasource.R")

ui <- fluidPage(
  
  # Application title
  titlePanel("SavEn Data Analytics"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    
    sidebarPanel(
      
      helpText("Create graphical representation of Data based on the selected options"),
      
      selectInput("site", 
                  label = "Choose a Site",
                  choices = c("JU4"),
                  selected = "JU4"),
      
      selectInput("device", 
                  label = "Choose a Device",
                  choices = c("Air flow meter of Compressor",
                              "Phosphating Energy meter",
                              "Compressor Energy meter"),
                  selected = "Air flow meter of Compressor"),
      
      dateInput("startDate",
                label = "From : ",
                format = "yyyy-mm-dd", value = Sys.Date()-100),
      dateInput("endDate",
                label = "To : ",
                format = "yyyy-mm-dd", value = Sys.Date(),
                max = Sys.Date()),
      
      textOutput("result")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      title="Data",
      id="mainPanel",
      plotOutput("dataTable"),
      textOutput("dataResult")
      #plotOutput("distPlot")
    )
    
  )
)

server <- shinyServer(function(input, output){

  output$result <- renderText({
    paste("Filter date range, from: ", input$startDate, " to : ", input$endDate)
  })

  query_output <- reactive({
    site <- switch(input$site, "JayaShree Unit 1" = "1","JayaShree DG" = "2","JayaShree Unit 4" = "3")
    
    deviceId <- switch(input$Machine,
                       "Air flow meter of Compressor" = "1",
                       "Phosphating Energy meter" = "2",
                       "Compressor Energy meter" = "3")
    
    print(paste("deviceId : ", deviceId))
    
    #query1 <- '{"ID" : {"$gt" : "'
    #query2 <- '", "$lt" : "'
    #query3 <- '"}}'
    
    #selectedDate = input$dateRange
    #print(selectedDate)
    #formatedDate <- selectedDate$year
    
    
    date <- getDate(input$date)
    #print(paste("dateId : ", date))
    
    date1<-getDate(input$inputId[1])
    date2<-getDate(input$inputId[2])
    
    print(paste("date1Id : ", date1))
    print(paste("date2Id : ", date2))
    
    fromDate <- paste(date1, '000000', sep='')
    endDate <- paste(date2, '000000', sep='')
    
    print(paste("fromDate : ", fromDate))
    print(paste("endDate : ", endDate))
    
    
    #output$inputId  <- renderText({
    #          print(paste("input$inputId is", 
    #        paste(as.character(input$dateRange), collapse = " to "))
    # )
    #})
  })
  
  output$dataResult <- renderText('Hello')
  
  output$plot <- renderPlot({
    d = query_output()   
    da=datasour()
    
    #print(paste("result : ", da))
    
    chartTitle <- switch(input$Machine,
                         "Air flow meter of Compressor" = "Air flow meter of Compressor...",
                         "Phosphating Energy meter" = "Phosphating Energy meter...",
                         "Compressor Energy meter" = "Compressor Energy meter..." 
    )
    
    
    if(input$Machine == "Air flow meter of Compressor") {
      plot(x = c(0,1,2,3,4,5),y= c(2,12,2,12,2,12),type="o",pch=19,
           xlab="Time",ylab="DTR",cex.lab=1.5,col="blue",lwd  = 3,
           main=paste("GDP-weighted averages shown for", chartTitle))
      
    }else if(input$Machine == "Phosphating Energy meter" ){
      plot(x = c(0,1,2,3,4,5),y= c(2,10,4,12,2,12),type="o",pch=22,
           xlab="Time",ylab="DKWH",cex.lab=1.5,col="red",lwd  = 3,
           main=paste("GDP-weighted averages shown for", chartTitle))
      
    } else {
      plot(x =c(0,1,2,3,4,5),y= c(2,11,4,8.5,2,10),type="o",pch=23,
           xlab="Time",ylab="KWH",cex.lab=1.5,col="Black",lwd  = 3,
           main=paste("GDP-weighted averages shown for", chartTitle))
    }
    
  })
})
shinyApp(ui = ui, server = server)