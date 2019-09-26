library(shiny)
library(leaflet)
library(shinythemes)

# Define UI for application that analyzes the patterns of crimes in DC
shinyUI(fluidPage(
  
  # Change the theme to flatly
  theme = shinytheme("flatly"),
  
  # Application title
  titlePanel("Patterns of Crimes in Washington DC"),
  
  # Three sidebars for uploading files, selecting time slots and districts
  sidebarLayout(
    sidebarPanel(
      
      # Create a file input
      fileInput("file","Choose A CSV File Please",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Create a multiple checkbox input for time slot
      checkboxGroupInput("Shift",
                         "Time Slot:",
                         c("DAY","EVENING","MIDNIGHT")
      ),
      # hr() is evidently used as "Enter" here
      hr(),
      helpText("Please Select The Time Slot You Want To Analyze For Criminal Patterns"),
      helpText("You Can Choose More Than One"),
      
      hr(),
      hr(),
      
      # Create a multiple checkbox input for police districts (As usual, DC Police Districts is the name and Districts is what we call out in the server)
      checkboxGroupInput("Districts",
                         "DC Police Districts:",
                         choices = list("District 1"= 1,"District 2"= 2,"District 3"= 3,"District 4"= 4,
                                        "District 5"= 5,"District 6"= 6,"District 7"= 7)
      ),
      
      hr(),
      helpText("Please Select The Police Districts You Want To Analyze For Criminal Patterns"),
      helpText("You Can Choose More Than One")
    ),
    
    # Make the sidebar on the right of the webpage
    position = "right",
    fluid = TRUE,
    
    
    
    # Show the map in the tab panel
    mainPanel(
      hr(),
      tabsetPanel(type="tabs",
                  tabPanel("Map", leafletOutput("map", height=630))
      )
    )
  )
))