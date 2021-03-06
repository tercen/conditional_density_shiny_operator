library(shiny)
library(tercen)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

############################################
#### This part should not be modified
getCtx <- function(session) {
  # retreive url query parameters provided by tercen
  query <- parseQueryString(session$clientData$url_search)
  token <- query[["token"]]
  taskId <- query[["taskId"]]
  
  # create a Tercen context object using the token
  ctx <- tercenCtx(taskId = taskId, authToken = token)
  return(ctx)
}
####
############################################

shinyServer(function(input, output, session) {
  
  dataInput <- reactive({
    getValues(session)
  })
  
  output$selectRCell <- renderUI({
    r.cells <- dataInput()$ri.names
    selectInput(inputId = "rcell", label = "Select row:", choices = r.cells)
  }) 
  
  output$selectCCell <- renderUI({
    c.cells <- dataInput()$ci.names
    selectInput(inputId = "ccell", label = "Select column:", choices = c.cells)
  }) 
  
  output$reacOut <- renderUI({
    plotOutput(
      "main.plot",
      height = input$plotHeight,
      width = input$plotWidth
    )
  }) 
  
  output$main.plot <- renderPlot({
    values <- dataInput()
    
    ri.id <- which(values$ri.names %in% input$rcell) - 1
    ci.id <- which(values$ci.names %in% input$ccell) - 1
    
    data <- values$data %>% subset(., .ri == ri.id & .ci == ci.id) %>% select(.y, colors)
    
    ## Conditional density plot 
    
    data$Y <- data$colors
    if(input$logval) data$X <- log1p(data$.y)
    if(!input$logval) data$X <- (data$.y)
    
    var_name <- values$ci.names[1]
    
    p1 <- ggplot(data, aes(x=X, fill=Y)) +
      geom_histogram(alpha=0.4, bins = 15, position="identity", aes(y = ..density..)) +
      geom_density(alpha = 0.25, adjust = input$smoothingPar)+
      scale_fill_manual(values=c("#fc8d62", "#66c2a5")) +
      theme_minimal() +
      labs(fill = "Quality flag", x = var_name, y = "Proportion",
           title = paste0("Distribution of ", var_name))
    
    p2 <- ggplot(data, aes(X, ..count.., fill = Y)) +
      geom_density(position = "fill") +
      scale_fill_manual(values=c("#fc8d62", "#66c2a5")) +
      theme_minimal() +
      labs(fill = "Quality flag", x = var_name, y = "Probability of passing",
           title = paste0("Conditional density plot - ", var_name))
    
    grid.arrange(p1, p2, nrow = 1)
    
    
  })
  
})

getValues <- function(session){
  ctx <- getCtx(session)
  values <- list()
  
  values$data <- ctx %>% select(.y, .ri, .ci) %>%
    group_by(.ci, .ri)
  
  values$ri.names <- c(ctx$rselect()[[1]])
  values$ci.names <- c(ctx$cselect()[[1]])
  values$data$colors <- ctx$select(ctx$colors)[[1]]
  
  return(values)
}