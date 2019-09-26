options(shiny.maxRequestSize=30*1024^2)
library(shiny)
library(leaflet)
library(dplyr)

# Define server that analyzes the patterns of crimes in DC
shinyServer(function(input, output) {
  
  # Create a map output variable (Always need a variable for something to be created)
  output$map <- renderLeaflet({
    
    # Connect to the sidebar of file input
    inFile <- input$file
    
    if(is.null(inFile))
      return(NULL)
    
    # Read input file
    mydata <- read.csv(inFile$datapath)
    attach(mydata)
    
    
    # Create colors with a categorical color function
    color <- colorFactor(rainbow(9), mydata$OFFENSE)
    
    # Create the leaflet function for data
    leaflet(mydata) %>%
      
      # Set the default view
      setView(lng = -77.0369, lat = 38.9072, zoom = 12) %>%
      
      # Provide tiles
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
      
      # Add circles
      addCircleMarkers(
        radius = 2,
        lng= mydata$LONGITUDE,
        lat= mydata$LATITUDE,
        stroke= FALSE,
        fillOpacity=0.1,
        color=color(OFFENSE)
      ) 
  })
})
