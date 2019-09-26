
options(shiny.maxRequestSize=30*1024^2)
library(shiny)
library(leaflet)
library(dplyr)

# Define server that analyzes the patterns of crimes in DC
shinyServer(function(input, output) {
  
  # Create an output variable for problem description
  output$text <- renderText({
    
    "This project uses the dataset 'Crime Incidents In 2017'. The dataset contains information for 2017 criminal patterns in DC, including CCN, Report Date, Shift, Method, Offense, Block, Ward, ANC, District, PSA, Neighborhood Cluster, Block Group, Census Tract, Voting Precinct, Latitude, Longitude, Bid, Start Date, End Date, and Object ID. Question: How Do the Patterns of Crimes in 2017 Vary at Different Time Slots and in Different Police Districts of Washington, DC? To answer this question, we analyze the types of crimes, the methods of crimes, the report frequency at different hours, and create a map for visualization. This question is a great interest to police officials in DC."
    
  })
  
  
  # Create a descriptive table for different offenses
  output$table1 <- renderPrint({
    
    # Connect to the sidebar of file input
    inFile <- input$file

    if(is.null(inFile))
      return("Please Upload A File For Analysis")
    
    # Read input file
    mydata <- read.csv(inFile$datapath)
    attach(mydata)
    
    # Filter the data for different time slots and different districts
    target1 <- c(input$Shift)
    target2 <- c(input$Districts)
    offense_df <- filter(mydata, SHIFT %in% target1 & DISTRICT %in% target2)
    
    # Create a table for offense (This is what we connect to with the verbatim output in the ui)
    table(offense_df$OFFENSE)
    
  })
  
  # Create a descriptive table for different criminal methods
  output$table2 <- renderPrint({
    
    # Connect to the sidebar of file input
    inFile <- input$file

    if(is.null(inFile))
      return("Please Upload A File For Analysis")
    
    # Read input file
    mydata <- read.csv(inFile$datapath)
    attach(mydata)
    
    # Filter the data for different time slots and different districts
    target1 <- c(input$Shift)
    target2 <- c(input$Districts)
    method_df <- filter(mydata, SHIFT %in% target1 & DISTRICT %in% target2)
    
    # Create a table for offense
    table(method_df$METHOD)
    
  })
  
  
  # Create a map output variable
  output$map <- renderLeaflet({
    
    # Connect to the sidebar of file input
    inFile <- input$file
    
    if(is.null(inFile))
      return(NULL)
    
    # Read input file
    mydata <- read.csv(inFile$datapath)
    attach(mydata)
    
    # Filter the data for different time slots and different districts
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
      
      # Add legends for different types of crime
      addLegend(
        "bottomleft",
        pal=color,
        values=OFFENSE,
        opacity=0.5,
        title="Type of Crime Committed"
      )
  })
})