library(tm)
library(markovchain)

load("./transitionMatrix.RData"); # format(object.size(transitionMatrix), units = "MB") # [1] "280.5 Mb"
load("./dormantroot/transitionMatrix.RData"); format(object.size(transitionMatrix), units = "MB") # [1] "13.5 Mb"
markovChainModel <- new("markovchain", transitionMatrix = transitionMatrix)
# save(markovChainModel, file = "markovChainModel")

predictFollowingWord <- function(model, input, numberOfOutcome = 10) {
  inputString <- input
  inputStringParts <- strsplit(inputString, " ")[[1]]
  inputStringLength <- length(inputStringParts)
  dictionary <- states(model)
  
  getRandomIndex <- function (len) (len * runif(1)) + 1
  getRandomWord <- function (len, dictionary) dictionary[getRandomIndex(len)]
  
  currentState <- NULL
  nextState <- NULL
  cache <- list()
  cache$stateHistory <- c()
  
  currentState <- inputStringParts[1]
  print(paste("first word:", currentState))
  if (!currentState %in% dictionary) 
    currentState <- getRandomWord(inputStringLength, dictionary)
  
  print(paste("check dictionary:", currentState))
  cache$stateHistory  <- c(cache$stateHistory, currentState)
  
  remainingInputStringParts <- inputStringParts[2:inputStringLength]
  
  for (remainingInputString in remainingInputStringParts) {
    nextState <- remainingInputString
    print(paste("next word:", nextState))
    if (!nextState %in% dictionary) {
      nextPossibilities <- conditionalDistribution(markovChainModel, currentState)
      nextStates <- dictionary[which.max(nextPossibilities)]
      if (length(nextStates) > 0) 
        nextState <- nextStates[getRandomIndex(length(nextStates))]
      else
        warning("Unable to find next state in model")
    }

    currentState <- nextState
    
    cache$stateHistory  <- c(cache$stateHistory, currentState)
  }
  
  cache$conditionalProbabilities <- 
    sort(conditionalDistribution(markovChainModel, currentState),
         decreasing = TRUE)[1:numberOfOutcome]
  
  cache
}

preprocessInputText <- function(inputText) {
  corpus <- Corpus(VectorSource(inputText))
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  # corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, stemDocument) # E.g. running and run may have different linguistic context
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, PlainTextDocument)
  return(as.character(corpus[[1]]))
}

predictFollowingWord(markovChainModel, preprocessInputText("jokingly wished the two could"))
