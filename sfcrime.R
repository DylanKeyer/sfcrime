#sfcrime pre-processing
sfcrime <- function(Shiny = FALSE){
  setwd('C:/Users/dylan_000/OneDrive/KaggleData/sfcrime')

  train <- read.csv('train.csv', stringsAsFactors = FALSE)
  test <- read.csv('test.csv', stringsAsFactors = FALSE)

  str(train)
  str(test)

  train$Dates <- as.Date(train$Dates)
  str(train)

  test$Dates <- as.Date(test$Dates)

  library(ggplot2)
  library(dplyr)
  if(Shiny == TRUE){
    library(shiny)
    library(leaflet)
    library(ggmap)
  
    loc <- 'San Francisco, CA'
    location <- geocode(loc)
    
    ui <- bootstrapPage(
      tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
      leafletOutput("mymap", width = "100%", height = "100%"),
      absolutePanel(top=10,right=10,
                    selectInput('offense','Choose Offense:', choices=unique(train$Category))
      ))
    
    
    server <- (function(input, output, session) {
      filteredData <- reactive({
        train[train$Category == input$offense,]
      })
      output$mymap <- renderLeaflet({
        map <- leaflet() %>% addTiles()
        map <- map %>% setView(location$lon,location$lat, zoom = 13)      
      })
      observe({
        leafletProxy("mymap", data = filteredData()) %>%
          clearMarkers() %>%
          addMarkers(filteredData()$X,filteredData()$Y,clusterOptions=markerClusterOptions(),popup=
                       paste(paste(month(filteredData()$Dates,label=T,abbr=F),day(filteredData()$Dates),sep=' '),
                             year(filteredData()$Dates),sep=', '))
      })
    })
    shinyApp(ui, server)
  }
  
  
  
}