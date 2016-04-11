library(shiny)

source("week4-MarkovChain.R")
load("./dormantroot/transitionMatrix.RData")

isValid <- function(input) {
  if (length(input) == 0) FALSE
  else if (length(input) == 1 && input[1] == "") FALSE
  else if (length(input) == 1 && input[1] != "") TRUE
  else FALSE
    
}

model <- new("markovchain", transitionMatrix = transitionMatrix)

predictionModelHandler <- function(input, numToPredict) {
  redictedWords <- predictFollowingWord(model, preprocessInputText(input), numToPredict)
  redictedWords <- colnames(t(as.matrix(predictedWords$conditionalProbabilities)))
  return(paste(redictedWords, collapse = ", "))
}

predictionModel <- function(input, numToPredict) {
  if (isValid(input)) return(predictionModelHandler(input, numToPredict))
  else return("<Please use a valid input>")
}

shinyServer(
  function(input, output) {    
    reactiveInputHandler <- reactive({
        predictionModel(input$inputText, input$numToPredict)
    })
    
    reactiveInputHandler2 <- reactive({
      if (isValid(input$inputText)) return(paste("\"", input$inputText , "\"", sep = ""))
      else return("<Please use a valid input>")
    })
    
    output$inputText <- renderText(reactiveInputHandler2())
    
    output$predictedWords <- renderText(reactiveInputHandler())
  })