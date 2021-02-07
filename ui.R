library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Histogram"),
  
  sidebarPanel(
    uiOutput("selectRCell"),
    uiOutput("selectCCell"),
    sliderInput("smoothingPar", "Smoothing parameter", 0, 1, 0.5, step = 0.05),
    checkboxInput("logval", "Log", 0),
    sliderInput("plotWidth", "Plot width (px)", 200, 2000, 850),
    sliderInput("plotHeight", "Plot height (px)", 200, 2000, 500)
  ),
  
  mainPanel(
    uiOutput("reacOut")
  )
  
))