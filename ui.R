library(shiny)

shinyUI(fluidPage(
  titlePanel("Regression Model"),
  sidebarLayout(
    sidebarPanel(
      selectInput("model.name", "Choose a model:", choices = c("model1", "model2","model3","model4","model5","model6","model7"))
    ),
    mainPanel(   
      plotOutput("regressionPlot")     
    )
  )
))
