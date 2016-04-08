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
# ------------------------------------------------------------------------------
load("biGram.RData")
biGramCleaned <- biGram[grep("^[a-z]+ [a-z]+$", biGram, perl = TRUE)]
save(biGramCleaned, file = "biGramCleaned.RData")
rm(biGram)

load("triGram.RData")
triGramCleaned <- triGram[grep("^[a-z]+ [a-z]+ [a-z]+$", triGram, perl = TRUE)]
save(triGramCleaned, file = "triGramCleaned.RData")
rm(triGram)

gc()

# ------------------------------------------------------------------------------
# Replace term of count 1 with <UNK>
# ------------------------------------------------------------------------------
load("oneGramFreqOne.RData")
UNK <- "<UNK>"

Sys.time()

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