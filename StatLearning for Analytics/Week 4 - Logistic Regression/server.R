shinyServer(function(input, output) {
library(ggplot2)
  
  output$plot<- renderPlot({

    x <- seq(0,8,length.out=50)
    beta_0 <- input$beta_0
    beta_1 <- input$beta_1
    beta_2 <- input$beta_2
    beta_3 <- input$beta_3
    
    y_hat <- exp(beta_0 + beta_1*x +beta_2 + beta_3*x*beta_2 )
    
    logit <- y_hat / (1 + y_hat)
    
    df <- data.frame("X" = x, "logit" = logit)
    
    ggplot(df, aes(x=X, y=logit)) + geom_line(colour='#3690C0', size=3) + labs(y = "p(y=1)", x="Age (Decades)") +ylim(0,1)

  })

})
