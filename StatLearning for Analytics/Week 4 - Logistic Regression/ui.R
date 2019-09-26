shinyUI(fluidPage(
  titlePanel("Logistic Function"),
  
  sidebarLayout(
    sidebarPanel( 
      
      sliderInput("beta_0", 
                  label = "Beta 0",
                  min = -7, max = 5, value = -4),
      
      sliderInput("beta_1", 
                  label = "Beta 1 [Age]",
                  min = -7, max = 7, value = 2),
      
      sliderInput("beta_2", 
                  label = "Beta 2 [Female]",
                  min = -5, max = 5, value = 0),
      
      sliderInput("beta_3", 
                  label = "Beta 3 [Age x Female interaction]",
                  min = -5, max = 5, value = 0)
    ),
    
    mainPanel(
      plotOutput("plot"),
      br(),
      withMathJax(),
      helpText(" \\( p(y=1|...) = \\beta_0 + \\beta_1 Age + \\beta_2 Female + \\beta_3 Age*Female  \\).")
    )
    
  )
  
))
