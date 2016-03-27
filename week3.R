options(java.parameters = "-Xmx8192m" )
library(ggplot2); library(slam); library(tm);
library(RWeka)

set.seed(55669)

source("./sampleData.R")
makeSampleFiles(0.01) # 3%

source("./constructCorpus.R")
enUsOutputDirectory <- paste(outputDirectory, locales, sep = filePathSep)
ovid <- makeCorpus(enUsOutputDirectory)
ovid <- transformCorpus(ovid)
ovid <- tagDocumentWithId(ovid)

save(ovid, file="corpus.RData")
gc()

#########################################################
## N Gram Analysis
########################################################

source("./parallelProcessing.R")
gramTokenizer <- function(corpus, n) {
  options(java.parameters = "-Xmx8192m" )
  RWeka::NGramTokenizer(corpus, RWeka::Weka_control(min = n, max = n))
}

cluster <- startParallelProcessing()
clusterEvalQ(cluster, function() { 
  options(java.parameters = "-Xmx8192m" )
  library(RWeka)
  })
Sys.time()
ngrams <- foreach(x = c(1:5),
                  .combine = list,
                  .multicombine = TRUE,
                  .export = "ovid") %dopar% 
  gramTokenizer(ovid, x)
stopParallelProcessing(cluster)
Sys.time()
gc()

oneGram <- ngrams[[1]]; biGram <- ngrams[[2]]; triGram <- ngrams[[3]];
fourGram <- ngrams[[4]]; fiveGram <- ngrams[[5]]
rm(ngrams)
gc()

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

source("nGramAnalysis.R")

reductionRows <- c(1: 30)
oneGramDfReduced <- oneGramDf[reductionRows, ]
biGramDfReduced <- biGramDf[reductionRows, ]
triGramDfReduced <- triGramDf[reductionRows, ]
fourGramDfReduced <- fourGramDf[reductionRows, ]
fiveGramDfReduced <- fiveGramDf[reductionRows, ]

plotNgram(oneGramDfReduced, "Top 30 1-Gram", "1-Gram", "Count of 1-Gram")
plotNgram(biGramDfReduced, "Top 30 2-Grams", "2-Grams", "Count of 2-Grams")
plotNgram(triGramDfReduced, "Top 30 3-Grams", "3-Grams", "Count of 3-Grams")
plotNgram(fourGramDfReduced, "Top 30 4-Grams", "4-Grams", "Count of 4-Grams")
plotNgram(fiveGramDfReduced, "Top 30 5-Grams", "5-Grams", "Count of 5-Grams")
#########################################################
## Building predictive model
########################################################

## Get last word out of a string
getLastWord <- function (txt, seperator = " ") {
  txtElem <- strsplit(txt, seperator)[[1]]
  txtElem[length(txtElem)]
}

## Get last word out of a vector of strings
getLastWords <- function(txts) {
  lastWords <- c()
  if (length(txts) != 0) {
    for(i in c(1:length(txts)))
      lastWords[i] <- getLastWord(txts[i])
  }
  lastWords
}

getLengthOfWords <- function(txt, seperator = " ") {
  length(strsplit(txt, seperator)[[1]])
}

sanitizeNlastWords <- function(txt, sanitize = TRUE) {
  if (sanitize){
    txt <- tolower(txt)
  }
  txt
}

getLastNwords <- function(txt, n, seperator = " ") {
  txtElems <- strsplit(txt, seperator)[[1]]
  if (length(txtElems) < n) {
    stop("Text length invalid.")
  } else {
    lowerBound <- (length(txtElems) - n + 1)
    txtElems <- txtElems[lowerBound:length(txtElems)]
  }
  lastWords <- paste(txtElems, collapse = " ")
  sanitizeNlastWords(lastWords)
}

##
## Match search text with entries in N Gram data.frame
##
filterNgrams <- function(nGramDf, searchTxt) {
  # Will perl = TRUE incure performance issue ??? Or is it relevant ???
  nGramDf[grep(paste("^", searchTxt, " ", sep = ""), nGramDf$Term, perl = TRUE), ][, c("Term")]
}

##
## Given a text string as input, predict the 3 following possible words
##
getNextWordsSuggestion <- function(inputTxt) {
  suggestedWords <- c()
  nGramDfNames <- c("fiveGramDf", "fourGramDf", "triGramDf", "biGramDf", "oneGramDf") # 4 3 2 1 0
  for (i in 1:length(nGramDfNames)) {
    lowerBound <- 5 - i
    if (getLengthOfWords(inputTxt) < lowerBound) {
      next
    } else {
      if (nGramDfNames[i] == nGramDfNames[5]) {
        suggestedWords <- c(suggestedWords, get(nGramDfNames[i])[1:3, "Term"])
      } else {
        lastNwords <- getLastNwords(inputTxt, lowerBound)
        suggestedWords<- c(suggestedWords, 
                        getLastWords(filterNgrams(get(nGramDfNames[i]), lastNwords)))
      }
    }
  }
  unique(suggestedWords[1:30])[1:3]
}

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
