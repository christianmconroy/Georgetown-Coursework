options(shiny.maxRequestSize=30*1024^2)
library(shiny)
library(leaflet)
library(dplyr)

# Define server that analyzes the patterns of crimes in DC
shinyServer(function(input, output) {
  
  # Create a map output variable
  output$map <- renderLeaflet({
    
    # Connect to the sidebar of file input
    inFile <- input$file

    if(is.null(inFile))
      return(NULL)
    
    # Read input file
    mydata <- read.csv(inFile$datapath)
    attach(mydata)
    
    # Filter the data for different time slots and different districts (Note there is nothing unless at least one thing is connected)
    target1 <- c(input$Shift)
    target2 <- c(input$Districts)
    map_df <- filter(mydata, SHIFT %in% target1 & DISTRICT %in% target2)
    
    # Create colors with a categorical color function
    color <- colorFactor(rainbow(9), map_df$OFFENSE)
    
    # Create the leaflet function for data
    leaflet(map_df) %>%
      
      # Set the default view
      setView(lng = -77.0369, lat = 38.9072, zoom = 12) %>%
      
      # Provide tiles
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
      
      # Add circles
      addCircleMarkers(
        radius = 2,
        lng= map_df$LONGITUDE,
        lat= map_df$LATITUDE,
        stroke= FALSE,
        fillOpacity=0.1,
        color=color(OFFENSE)
      ) %>%
      
      # Add legends for different types of crime (Marker won't automatically create this)
      addLegend(
        "bottomleft",
        pal=color,
        values=OFFENSE,
        opacity=0.5,
        title="Type of Crime Committed"
      )
  })
})
