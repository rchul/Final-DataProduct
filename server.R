#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  testpred <- reactive({
    req(input$vary)
    one <- paste(input$vary, collapse = " + ") 
    two <- paste("mpg ~ ",one, collapse = "")
    lm(as.formula(two),data = mtcars)
    })
  
  output$distPlot <- renderPlotly({
    
        plotyy <- ggplot(mtcars, aes(x=fitted(testpred()),y=mpg,colour=cyl)) +
        labs(y = "Miles Per Gallon", x = "Fitted Values") +
        geom_point() +
        geom_smooth(method = "lm") +
        theme_minimal()
        ggplotly(plotyy)
        
    })
  
  output$coef <- renderText({ 
    
    coef(testpred()) 
    
    })
  
  output$rs <- renderText({ 
    
    summary(testpred())$r.squared 
    
    })
  
})




#
#pred <- switch(input$var,
#               "Weight" = mtcars$wt,
#               "DISP" = mtcars$disp,
#               "Horse Power" = mtcars$hp)