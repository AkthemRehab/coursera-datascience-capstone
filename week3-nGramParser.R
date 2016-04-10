# ------------------------------------------------------------------------------
# Get Term with Count One
# ------------------------------------------------------------------------------
load("oneGram.RData")

preprocessNgramVector <- function(ngram) {
  ngram <- ngram[grep("^[a-z]+$", ngram, perl = TRUE)]
}

getTermWithCountOne <- function(oneGram) {
  as.character(subset(data.frame(table(oneGram)), Freq == 1)[, "oneGram"])
}

oneGramFreqOne <- getTermWithCountOne(preprocessNgramVector(oneGram))
save(oneGramFreqOne, file = "oneGramFreqOne.RData")

rm(oneGram); rm(oneGramFreqOne); gc()

# ------------------------------------------------------------------------------
# Cleaning individual ngrams
# 1. Get only characters
# ------------------------------------------------------------------------------
load("oneGram.RData")
oneGramCleaned <- preprocessNgramVector(oneGram)
save(oneGramCleaned, file = "oneGramCleaned.RData")
rm(oneGram); rm(oneGramCleaned); gc()

load("biGram.RData")
biGramCleaned <- biGram[grep("^[a-z]+ [a-z]+$", biGram, perl = TRUE)]
save(biGramCleaned, file = "biGramCleaned.RData")
rm(biGram); rm(biGramCleaned); gc()

load("triGram.RData")
triGramCleaned <- triGram[grep("^[a-z]+ [a-z]+ [a-z]+$", triGram, perl = TRUE)]
save(triGramCleaned, file = "triGramCleaned.RData")
rm(triGram); rm(triGramCleaned); gc();

gc()

# ------------------------------------------------------------------------------
# Replace term of count 1 with <UNK>
# ------------------------------------------------------------------------------
load("oneGramFreqOne.RData")
UNK <- "<UNK>"

Sys.time()

load("oneGramCleaned.RData")
oneGramParsed <- unlist(
  lapply(oneGramCleaned, function(term) {
    ifelse(term %in% oneGramFreqOne, UNK, term)
  }))
save(oneGramParsed, file = "oneGramParsed.RData")
rm(oneGramCleaned); rm(oneGramParsed); gc()

load("biGramCleaned.RData")
biGramParsed <- unlist(
  lapply(biGramCleaned, function(terms){
    termParts <- strsplit(terms, " ")[[1]]
    paste(
      ifelse(termParts[1] %in% oneGramFreqOne, UNK, termParts[1]),
      ifelse(termParts[2] %in% oneGramFreqOne, UNK, termParts[2]),
      sep = " ")
  }))
save(biGramParsed, file = "biGramParsed.RData")
rm(biGramCleaned); rm(biGramParsed); gc()

load("triGramCleaned.RData")
triGramParsed <- unlist(
  lapply(triGramCleaned, function(terms){
    termParts <- strsplit(terms, " ")[[1]]
    paste(
      ifelse(termParts[1] %in% oneGramFreqOne, UNK, termParts[1]),
      ifelse(termParts[2] %in% oneGramFreqOne, UNK, termParts[2]),
      ifelse(termParts[3] %in% oneGramFreqOne, UNK, termParts[3]),
      sep = " ")
  }))
save(triGramParsed, file = "triGramParsed.RData")
rm(triGramCleaned); rm(triGramParsed); gc()

Sys.time()
rm(UNK)

# ------------------------------------------------------------------------------
# Make Term Count Data Frame
# ------------------------------------------------------------------------------
sanitizeDataFrame <- function(df) {
  names(df) <- c("Term", "Count")
  df$Term <- as.character(df$Term)
  df
}

sortDataFrame <- function(df) {
  df[order(df$Count, decreasing = TRUE), ]
}

load("oneGramParsed.RData")
oneGramDataFrame <- data.frame(table(oneGramParsed))
oneGramDataFrame <- sortDataFrame(sanitizeDataFrame(oneGramDataFrame))
save(oneGramDataFrame, file = "oneGramDataFrame.RData")
rm(oneGramParsed); rm(oneGramDataFrame); gc();

load("biGramParsed.RData")
biGramDataFrame <- data.frame(table(biGramParsed))
biGramDataFrame <- sortDataFrame(sanitizeDataFrame(biGramDataFrame))
save(biGramDataFrame, file = "biGramDataFrame.RData")
rm(biGramParsed); rm(biGramDataFrame); gc();

load("triGramParsed.RData")
triGramDataFrame <- data.frame(table(triGramParsed))
triGramDataFrame <- sortDataFrame(sanitizeDataFrame(triGramDataFrame))
save(triGramDataFrame, file = "triGramDataFrame.RData")
rm(triGramParsed); rm(triGramDataFrame); gc();
