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
writeLines(oneGramDf3$Term, con = "oneGramDf3Term.txt")
# applying spell check filtering
spellChecked <- aspell("oneGramDf3Term.txt")
head(spellChecked[[1]])
writeLines(spellChecked, con = "oneGramDf3TermpellChecked.txt")
spellCheckedList <- tolower(unlist(spellChecked$Suggestions))
# filter against spell check
oneGramDf4 <- oneGramDf3[oneGramDf3$Term %in% spellCheckedList, ]
# lowest probability in onegram
oneGramDf4$probability <- oneGramDf4$Count / sum(oneGramDf4$Count)
lowestProbability <- min(oneGramDf4$probability)
# create n x n matrix according to length of unigram
rm(transitionMatrix)
lengthOfOneGram <- nrow(oneGramDf4)
transitionMatrix <- matrix(data = rep(0, as.numeric(lengthOfOneGram) * as.numeric(lengthOfOneGram)),
                           nrow = as.numeric(lengthOfOneGram), ncol = as.numeric(lengthOfOneGram),
                           dimnames = list(oneGramDf4$Term, oneGramDf4$Term))
# investigation about artificial values
# lengthOfOneGram * lowestProbability
# investigation about matrix access
dimnames(transitionMatrix)

# for each unigram search for combinations from bigram with ^ meta pattern match
for (term in oneGramDf4[1:1,][, "Term"]) {
  filteredBiGram <- biGramDf[grep(paste("^", term, " ", sep=""), biGramDf$Term, perl = TRUE), ]
  # get following words
  nextWords <- c()
  for (phrase in filteredBiGram$Term) {
    splitedPhrase <- strsplit(phrase, " ")[[1]]
    nextWord <- splitedPhrase[2]
    nextWords <- c(nextWords, nextWord)
  }
  # compute initial probabilities
  probabilityForNotPredictables <- (lengthOfOneGram - length(nextWords)) * lowestProbability
  remainingProbabilityForPredictables <- 1 - probabilityForNotPredictables
  
  # compute predictable probabilities
  predictableOneGramDf <- oneGramDf4[oneGramDf4$Term %in% nextWords, ]
  predictableOneGramDf$probability <- (predictableOneGramDf$Count / sum(predictableOneGramDf$Count)) * remainingProbabilityForPredictables
  
  # update transition matrix
  # term - y of matrix
  for (name in dimnames(transitionMatrix)) {
    found <- name %in% predictableOneGramDf$Term
    if (length(found) > 1) {
      print(name)
      print(length(found))
    }
#     if (found == TRUE) {
#       transitionMatrix[term, name] <- predictableOneGramDf[predictableOneGramDf$Term == name, "probability"]
#     } else {
#       transitionMatrix[term, name]  <- lowestProbability
#     }
  }
#   print(sum(transitionMatrix[term, ]))
}


