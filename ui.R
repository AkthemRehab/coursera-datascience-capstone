library(shiny)

shinyUI(
  fluidPage(theme = "bootstrap.css",
    titlePanel("Text Prediction Application"),
    
    sidebarPanel(
      textInput("inputText", h5("Text input")),
      numericInput("n", h5("Numbers of words to predict"), value = 3, min = 2, max = 5),
      # radioButtons("numToPredict", h5("Smoothing selection"),
      #             choices = list("Simple back-off" = 1, "Your algo " = 2),
      #              selected = 1),
      submitButton("SUBMIT"),
                    br()),
    mainPanel(
      tabsetPanel(type = "tabs", 
        tabPanel(h4("Overview"), 
          h4("Introduction"),
          p("This Text Prediction Application is a submission for the Coursera Data Science Specialization in collaboration with the Johns Hopkins University, Data Science Capstone module."),
          p("The inspiration of this application came from the SwiftKey Keyboard application, which is a type of text predicting tool that is capable of predicting the next word as users type onto the smart phone keyboard."),
          h4("Text Prediction Model"),
          p("The model used for this text prediction is the Markov Chain modelling. Which each word is modeled as state with some probability of moving into another starte."),
          h4("Application Instruction"),
          p("1. Type some text into the text box under the \"Text input\" heading"),
          p("2. \"Text input\" values are restricted to only alphabetical words."),
          p("3. Select the number of words to be predicted. default is 3, minimum is 2 and maximum is 5."),
          p("4. Click submit button once inputs are statisfied."),
          p("5. Navigate to Output Panel to view results by clickingon the \"Output\" tab.")),
        
        tabPanel(h4("Output"),
          h4("You have entered,"),
          span(h4(textOutput("inputText")), style="font-weight: bold; color: #4582ec"),
          br(),
          h4("The predicted words are as follow,"),
          span(h4(textOutput("predictedWords")), style="font-weight: bold; color: #4582ec")
          ))
  ))
)