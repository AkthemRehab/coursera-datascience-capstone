set.seed(123456789)
# load unigram and bigrams
load("oneGram.RData"); load("biGram.RData");
sanitizeDataFrame <- function(df) {
  names(df) <- c("Terms", "Count")
  df$Terms <- as.character(df$Terms)
  df <- df[order(df$Count, decreasing = TRUE), ]
  df
}
oneGramDf <- sanitizeDataFrame(data.frame(table(oneGram)))
biGramDf <- sanitizeDataFrame(data.frame(table(biGram)))
# only alphabet words remain
oneGramDf1 <- oneGramDf[grepl("^[a-z]+$", oneGramDf$Terms, perl = TRUE), ]
# remove stop words
library(tm)
oneGramDf2 <- oneGramDf1[!oneGramDf1$Terms %in% stopwords(), ]
# remove words that has less occurence by threshold
oneGramDf3 <- oneGramDf2
oneGramDf3$Probability <- oneGramDf2$Count / nrow(oneGramDf2)
summary(oneGramDf3)
occurenceThreshold <- 1.694e-05
oneGramDf4 <- oneGramDf3[oneGramDf3$Probability >= occurenceThreshold, ]
# create n x n matrix according to length of unigram
lengthOfOneGram <- nrow(oneGramDf4)
transitionMatrix <- matrix(data = rep(0, as.numeric(lengthOfOneGram) * as.numeric(lengthOfOneGram)),
                           nrow = as.numeric(lengthOfOneGram), ncol = as.numeric(lengthOfOneGram)
                           )
# for each unigram search for combinations from bigram with ^ meta pattern match
for (term in oneGramDf4[c(22334: 22350), ][, "Terms"]) {
  print(biGramDf[grep(paste("^", term, " ", sep=""), biGramDf$Term, perl = TRUE), ])
}


