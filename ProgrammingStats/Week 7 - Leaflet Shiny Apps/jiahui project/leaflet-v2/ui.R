library(shiny)
library(leaflet)
library(shinythemes)

# Define UI for application that analyzes the patterns of crimes in DC
shinyUI(fluidPage(
  
  # Change the theme to flatly
  theme = shinytheme("flatly"),
  
  # Application title
  titlePanel("Patterns of Crimes in Washington DC"),
  
  # Sidebar for uploading the file
  sidebarLayout(
    sidebarPanel(
      
      # Create a file input
      fileInput("file","Choose a CSV File Please",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))
      
    ),
    
    # Make the sidebar on the right of the webpage
    position = "right",
    fluid = TRUE,
    
    # Show the the map in the tab panel (Could add a bunch of other tabs to this of course)
    # The tabPabel "Map" part and the leafletOutput sets it up to have markers
    mainPanel(
      hr(),
      tabsetPanel(type="tabs",
                  tabPanel("Map", leafletOutput("map", height=630))
      )
    )
  )
))
