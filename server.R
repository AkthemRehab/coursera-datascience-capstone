library(shiny)

source("week4-MarkovChain.R")
load("./dormantroot/transitionMatrix.RData")

isValid <- function(input) {
  if (length(input) == 0) FALSE
  else if (length(input) == 1 && input[1] == "") FALSE
  else if (length(input[grep("\\d", input, perl = TRUE)])) FALSE
  else if (length(input[grep("\\W", input, perl = TRUE)])) FALSE
  else if (length(input) == 1 && input[1] != "") TRUE
  else FALSE
    
}

model <- new("markovchain", transitionMatrix = transitionMatrix)

predictionModelHandler <- function(model, input, numToPredict) {
  
}

predictionModel <- function(input) {
  if (isValid(input)) return(paste(input, ", ", input, ", ", input, sep = ""))
  else return("<Please use a valid input>")
}

shinyServer(
  function(input, output) {    
    reactiveInputHandler <- reactive({
        predictionModel(input$inputText)
    })
    
    reactiveInputHandler2 <- reactive({
      if (isValid(input$inputText)) return(paste("\"", input$inputText , "\"", sep = ""))
      else return("<Please use a valid input>")
    })
    
    output$inputText <- renderText(reactiveInputHandler2())
    
    output$predictedWords <- renderText(reactiveInputHandler())
  })