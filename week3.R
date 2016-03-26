options( java.parameters = "-Xmx8g" )

library(ggplot2); library(RWeka); library(slam); library(reshape2)
library(tm); library(wordcloud)

set.seed(55669)

source("./sampleData.R")
makeSampleFiles()

enUsOutputDirectory <- paste(outputDirectory, locales, sep = filePathSep)

makeCorpus <- function(d) {
  dirSource <- DirSource(directory = d, encoding = "UTF-8")
  ovid <- VCorpus(dirSource, readerControl = list(language = "eng"))
  on.exit(close(dirSource))
  ovid
}

ovid <- makeCorpus(enUsOutputDirectory)

transformCorpus <- function(corpus) {
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  # corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, stemDocument) # E.g. running and run may have different linguistic context
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, PlainTextDocument)
  corpus
}

ovid <- transformCorpus(ovid)

tagDocumentWithId <- function(corpus) {
  for(i in c(1 : length(corpus))) {
    DublinCore(corpus[[i]], "id") <- i
  }
  corpus
}

ovid <- tagDocumentWithId(ovid)

save(ovid, file="corpus.RData")
gc()

#########################################################
## N Gram Analysis
########################################################

gramTokenizer <- function(corpus, n) {
  NGramTokenizer(corpus, Weka_control(min = n, max = n))
}

Sys.time()
oneGram <- gramTokenizer(ovid, 1); gc()
biGram <- gramTokenizer(ovid, 2); gc()
triGram <- gramTokenizer(ovid, 3); gc()
fourGram <- gramTokenizer(ovid, 4); gc()
fiveGram <- gramTokenizer(ovid, 5); gc()
Sys.time()

save(oneGram, file = "oneGram.RData")
save(biGram, file = "biGram.RData")
save(triGram, file = "triGram.RData")
save(fourGram, file = "fourGram.RData")
save(fiveGram, file = "fiveGram.RData")
gc()

oneGramDf <- data.frame(table(oneGram))
biGramDf <- data.frame(table(biGram))
triGramDf <- data.frame(table(triGram))
fourGramDf <- data.frame(table(fourGram))
fiveGramDf <- data.frame(table(fiveGram))

sanitizeGramDf <- function(df) {
  newDf <- data.frame(Term = as.character(df[, 1]), Count = df[, 2])
  newDf$Term <- as.character(newDf$Term)
  newDf
}

oneGramDf <- sanitizeGramDf(oneGramDf)
biGramDf <- sanitizeGramDf(biGramDf)
triGramDf <- sanitizeGramDf(triGramDf)
fourGramDf <- sanitizeGramDf(fourGramDf)
fiveGramDf <- sanitizeGramDf(fiveGramDf)

sortGramDf <- function(df) {
  df[order(df$Count, decreasing = TRUE), ]
}

oneGramDf <- sortGramDf(oneGramDf)
biGramDf <- sortGramDf(biGramDf)
triGramDf <- sortGramDf(triGramDf)
fourGramDf <- sortGramDf(fourGramDf)
fiveGramDf <- sortGramDf(fiveGramDf)
gc()

reductionRows <- c(1: 30)
oneGramDfReduced <- oneGramDf[reductionRows, ]
biGramDfReduced <- biGramDf[reductionRows, ]
triGramDfReduced <- triGramDf[reductionRows, ]
fourGramDfReduced <- fourGramDf[reductionRows, ]
fiveGramDfReduced <- fiveGramDf[reductionRows, ]

plotNgram <- function(df, titleLabel, xLabel, yLabel) {
  plot1 <- ggplot(df, aes(x = reorder(Term, -Count), y = Count))
  plot1 <- plot1 + geom_bar(stat = "identity")
  plot1 <- plot1 + ggtitle(titleLabel)
  plot1 <- plot1 + labs(x = xLabel, y = yLabel)
  plot1 <- plot1 + theme(axis.text.x = element_text(angle = 45, size = 14, hjust = 1), 
                         plot.title = element_text(size = 20, face = "bold"))
  plot1
}

plotNgram(oneGramDfReduced, "Top 30 1-Gram", "1-Gram", "Count of 1-Gram")
plotNgram(biGramDfReduced, "Top 30 2-Grams", "2-Grams", "Count of 2-Grams")
plotNgram(triGramDfReduced, "Top 30 3-Grams", "3-Grams", "Count of 3-Grams")
plotNgram(fourGramDfReduced, "Top 30 4-Grams", "4-Grams", "Count of 4-Grams")
plotNgram(fiveGramDfReduced, "Top 30 5-Grams", "5-Grams", "Count of 5-Grams")

#########################################################
## Building predictive model
########################################################
# class(fourGramDf) # [1] "data.frame"
# names(fourGramDf) # [1] "Term"  "Count"
# head(fourGramDf)

## 
## Get last word out of a string
##
getLastWord <- function (txt, seperator = " ") {
  txtElems <- strsplit(txt, seperator)[[1]]
  txtElems[length(txtElems)]
}

## 
## Get last word out of a vector of strings
##
getLastWords <- function(txts) {
  numOfTxt <- length(txts)
  lastWords <- vector(length = numOfTxt)
  for(i in c(1:numOfTxt)) {
    lastWords[i] <- getLastWord(txts[i])
  }
  lastWords
}

# Test
getLastWord(filteredGrams[1])
getLastWords(filteredGrams)

##
## Get number of words from the end of string
##
getEndingWords <- function (txt, seperator = " ") {
  txtToLower <- tolower(txt)
  txtElems <- strsplit(txtToLower, seperator)[[1]]
  lengthOfTxt <- length(txtElems)
  lowerBound <- 0
  upperBound <- lengthOfTxt
  offset <- 1 ## sequence indexing starts from 1
  if (lengthOfTxt == 0) {
    lowerBound <- upperBound
  } else if (lengthOfTxt == 1) {
    lowerBound <- 1
  } else if (lengthOfTxt == 2) {
    lowerBound <- 1
  } else {
    lowerBound <- lengthOfTxt - 3 + offset
  }
  paste(txtElems[lowerBound:upperBound], collapse = " ")
}

# Test
sampleTxts <- c("",
                "Hello", 
                "Hello there",
                "thanks for the",
                "and thanks for the",
                "Hello there has been a long time and thanks for the")

for (txt in sampleTxts) {
  # print(sampleText)
  print(getEndingWords(txt))
}

##
## Get the N based on string length
##
getN <- function(txt, seperator = " ") {
  txtLength <- length(strsplit(txt, seperator)[[1]])
  if (txtLength == 0) {
    1
  } else if (txtLength == 1) {
    2
  } else if (txtLength == 2) {
    3
  } else {
    4
  }
}

##
## Return the "standard" N Gram Data Fram name
##
getNGramDfName <- function(n) {
  if (n == 1) {
    "oneGramDf"
  } else if (n == 2) {
    "biGramDf"
  } else if (n == 3) {
    "triGramDf"
  } else if (n == 4) {
    "fourGramDf"
  } else {
    stop("N Gram does not exist!")
  }
}

##
##
##
isValidNgramDfName <- function(nGramDfName) {
    exists(nGramDfName)
}

##
## Returns the appropriate N Gram DF depending on n argument
##
getNGramDf <- function(n) {
  nGramDfName <- getNGramDfName(n)
  if (isValidNgramDfName(nGramDfName)) {
    get(nGramDfName)
  } else {
    stop("N Gram does not exist!")
  }
}

# Test
getNGramDfName(1)
isValidNgramDfName(getNGramDfName(1))
class(getNGramDf(1))

##
## Match search text with entries in N Gram data.frame
##
filterNgrams <- function(nGramDf, searchTxt) {
  # Will perl = TRUE incure performance issue ???
  nGramDf[grep(paste("^", searchTxt, sep = ""), nGramDf$Term, perl = TRUE), ][1:3, c("Term")]
}

##
## Given a text string as input, predict the 3 following possible words
##
getNextWordsSuggestion <- function(inputTxt) {
  N <- getN(inputTxt)
  nGramDf <- getNGramDf(N)
  endingWords <- getEndingWords(inputTxt)
  filteredNgrams <- filterNgrams(nGramDf, endingWords)
  filteredNgrams
  # if (length(filteredNgrams) == 0) {
  #  filteredNgrams <- oneGramDf[1:3, c("Term")]
  # }
  # getLastWords(filteredNgrams)
}

# Test
getNextWordsSuggestion("The guy in front of me just bought a pound of bacon, a bouquet, and a case of")

#########################################################
## Week3 quiz
########################################################
week3 <- function() {
  inputData <- c(
                "The guy in front of me just bought a pound of bacon, a bouquet, and a case of",
                "You're the reason why I smile everyday. Can you follow me please? It would mean the",
                "Hey sunshine, can you follow me and make me the",
                "Very early observations on the Bills game: Offense still struggling but the",
                "Go on a romantic date at the",
                "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my",
                "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some",
                "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little",
                "Be grateful for the good times and keep the faith during the",
                "If this isn't the cutest thing you've ever seen, then you must be")
  for(i in 1:length(inputData)) {
    answer <- paste("Q", i, ": ", paste(getNextWordsSuggestion(inputData[i]), collapse = ","), sep = "")
    print(answer)
  }
}
week3()
