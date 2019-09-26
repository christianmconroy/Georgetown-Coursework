options(shiny.maxRequestSize=30*1024^2)
# See this to see how to change limits on file uploads
# https://stackoverflow.com/questions/18037737/how-to-change-maximum-upload-size-exceeded-restriction-in-shiny-and-save-user
library(shiny)
library(leaflet)

# Define server that analyzes the patterns of crimes in DC
shinyServer(function(input, output) {
  
  # Create a map output variable (renderLeaflet key here)
  output$map <- renderLeaflet({
    
    # Create the leaflet function
    leaflet() %>%
      
      # Set the default view
      setView(lng = -77.0369, lat = 38.9072, zoom = 12) %>%
      
      # Provide tiles (The actual map visual - the CartoDB is one option)
      addProviderTiles("CartoDB.Positron", 
                       options = providerTileOptions(noWrap = TRUE))
      # To understand this see https://rstudio.github.io/leaflet/basemaps.html
      # and search for third-party tiles
  })
})
