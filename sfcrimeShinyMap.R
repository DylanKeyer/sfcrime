#Interactive plot of San Francisco crimes using Shiny and Leaflet#

#load libraries
require(leaflet)
require(shiny)
require(ggmap)

#read in data
train <- read.csv('train.csv', stringsAsFactors = FALSE)
test <- read.csv('test.csv', stringsAsFactors = FALSE)

#convert dates to Data format
train$Dates <- as.Date(train$Dates)
test$Dates <- as.Date(test$Dates)


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