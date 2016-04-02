set.seed(123456789)
# load unigram and bigrams
load("oneGram.RData"); load("biGram.RData");
sanitizeDataFrame <- function(df) {
  names(df) <- c("Term", "Count")
  df$Term <- as.character(df$Term)
  df <- df[order(df$Count, decreasing = TRUE), ]
  df
}
oneGramDf <- sanitizeDataFrame(data.frame(table(oneGram)))
biGramDf <- sanitizeDataFrame(data.frame(table(biGram)))
# only alphabet words remain
oneGramDf1 <- oneGramDf[grepl("^[a-z]+$", oneGramDf$Term, perl = TRUE), ]
# remove stop words
library(tm)
oneGramDf2 <- oneGramDf1[!oneGramDf1$Term %in% stopwords(), ]
summary(oneGramDf2)
# Term with 1 Count
nrow(oneGramDf2[oneGramDf2$Count == 1, ]) # [1] 32366
# Remove Term with 1 Count
oneGramDf2 <- oneGramDf2[oneGramDf2$Count != 1, ]
# remove words that has less occurence by threshold
# sum(oneGramDf2$Count) # [1] 536568
oneGramDf3 <- oneGramDf2
writeLines(text = oneGramDf3$Term, con = "oneGramDf3Term.txt")
# applying spell check filtering
spellChecked <- aspell("oneGramDf3Term.txt")
# head(spellChecked[[1]])
spellCheckedList <- tolower(unlist(spellChecked$Suggestions))
writeLines(text = spellCheckedList, con = "oneGramDf3TermpellChecked.txt")
# filter against spell check
oneGramDf4 <- oneGramDf3[oneGramDf3$Term %in% spellCheckedList, ]
# further cleaning with regexp
# stickyAlphaRegexp <- ""
# for (i in 1:length(letters)) {
#   regexp1 <- paste(letters[i], letters[i], sep = "")
#   if (i == length(letters)) stickyAlphaRegexp <- paste(stickyAlphaRegexp, regexp1, sep = "")
#   else stickyAlphaRegexp <- paste(stickyAlphaRegexp, regexp1, "|", sep = "")
# }
# oneGramDf5 <- oneGramDf4[grep(stickyAlphaRegexp, oneGramDf4$Term, perl = TRUE), ]
# consider only anything more than mean
summary(oneGramDf4)
oneGramDf5 <- oneGramDf4[oneGramDf4$Count >= 31, ]

oneGramDfFinal <- oneGramDf5[1:1000, ]
head(oneGramDfFinal, 50)

# lowest probability in onegram
oneGramDfFinal$Probability <- oneGramDfFinal$Count / sum(oneGramDfFinal$Count)
lowestProbability <- min(oneGramDfFinal$Probability)
# investigation about matrix access
allPossibleNextWords <- dimnames(transitionMatrix)[[1]]
# create n x n matrix according to length of unigram
transitionMatrix <- matrix(data = rep(0, as.numeric(lengthOfOneGram) * as.numeric(lengthOfOneGram)),
                           nrow = as.numeric(lengthOfOneGram), ncol = as.numeric(lengthOfOneGram),
                           dimnames = list(oneGramDfFinal$Term, oneGramDfFinal$Term))
# for each unigram search for combinations from bigram with ^ meta pattern match
for (term in oneGramDfFinal[, "Term"]) {
  filteredBiGram <- biGramDf[grep(paste("^", term, " ", sep=""), biGramDf$Term, perl = TRUE), ]
  # get following words from Bi Gram
  nextWords <- c()
  for (phrase in filteredBiGram$Term) {
    splitedPhrase <- strsplit(phrase, " ")[[1]]
    nextWord <- splitedPhrase[2]
    nextWords <- c(nextWords, nextWord)
  }
  # cross validate with oneGramDfFinal (e.g. how many nextWord found in oneGramDfFinal ?)
  predictableOneGramDf <- oneGramDfFinal[oneGramDfFinal$Term %in% nextWords, ]
  # all nextWords are not found in one gram
  if (nrow(predictableOneGramDf) == 0) {
    equalProbability <- 1 / lengthOfOneGram
    for (name in allPossibleNextWords) {
      transitionMatrix[term, name] <- equalProbability
    }
  # some nextWords are foudn in one gram
  } else {
    # compute initial probabilities
    sumOfProbabilityForNotPredictables <- (lengthOfOneGram - nrow(predictableOneGramDf)) * lowestProbability
    sumOfRemainingProbabilityForPredictables <- 1 - sumOfProbabilityForNotPredictables
    # compute predictable probabilities
    predictableOneGramDf$Probability <- 
      (predictableOneGramDf$Count / sum(predictableOneGramDf$Count)) * sumOfRemainingProbabilityForPredictables
    # catch if suspecious
    summ <- sum(predictableOneGramDf$Probability)
    if (all.equal(summ, sumOfRemainingProbabilityForPredictables)) {
      warning(paste("[", term, "] Probability must always equal to 1. (", 
                 summ, " <> ", sumOfRemainingProbabilityForPredictables, ")", sep = ""))
    }
    # update transition matrix
    for (name in allPossibleNextWords) {
      if (name %in% predictableOneGramDf$Term) {
        transitionMatrix[term, name] <-
          predictableOneGramDf[predictableOneGramDf$Term == name, "Probability"]
      } else {
        transitionMatrix[term, name]  <- lowestProbability
      }
    }
    summ <- sum(transitionMatrix[term, ])
    if (all.equal(summ, 1)) {
      warning(paste("[", term, "] Row did not sum to 1. (", summ, ")", sep = ""))
    }
  }
}
warnings()

save(transitionMatrix, file = "transitionMatrix.RData")
write.table(transitionMatrix, "transitionMatrix.txt")

library("markovchain")
markovChainModel <- new("markovchain", transitionMatrix = transitionMatrix)

predictFollowingWord <- function(model, , numberOfOutCome = 10) {
  inputString <- input
  inputStringParts <- strsplit(inputString, " ")[[1]]
  inputStringLength <- length(inputStringParts)
  dictionary <- states(markovChainModel)
  
  getRandomIndex <- function (len) (len * runif(1)) + 1
  getRandomWord <- 
    function (len, dictionary = dictionary) dictionary[getRandomIndex(len)]
  
  currentState <- NULL
  nextState <- NULL
  cache <- list()
  cache$stateHistory <- c()
  
  currentState <- inputStringParts[1]
  if (!currentState %in% dictionary)
    currentState <- getRandomWord(inputStringLength)
  
  remainingInputStringParts <- inputStringParts[2:inputStringLength]
  
  for (remainingInputString in remainingInputStringParts) {
    nextState <- remainingInputString
    
    if (!nextState %in% ?conditionalDistribution)
      nextPossibilities <- conditionalDistribution(markovChainModel, currentState)
    
    nextStates <- dictionary[which.max(nextPossibilities)]
    if (length(nextStates) > 0) 
      nextState <- nextStates[getRandomIndex(length(nextStates))]
    else
      warning("Unable to find next state in model")
    
    currentState <- nextState
    
    cache$stateHistory  <- c(cache$stateHistory, currentState)
  }
  
  cache$conditionalProbabilities <- 
    sort(conditionalDistribution(markovChainModel, currentState),
         decreasing = TRUE)[1:numberOfOutcome]
}

week4 <- function() {
  questions <- c(
    "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd",
    "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his")
}
