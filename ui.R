#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  plotlyOutput("distPlot"),
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
        column(4,h3("MTCars Regression"),
                checkboxGroupInput("vary","Choose Predictors:",
                        choiceNames = list("Weight","DISP","Horse Power"),
                        choiceValues = list("wt","disp","hp"),
                        selected = list("wt")
          )
        ),
        column(4,h3("R-Square"),
              textOutput("rs"))
  )
))
