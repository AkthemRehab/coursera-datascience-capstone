# ------------------------------------------------------------------------------
# PART 0: Enrich Term Frequency data frames
# ------------------------------------------------------------------------------
enrichNGramDataFrame <- function(nGramDf, termCount) {
  
  rowKeys <- function(nGramDf) {
    unlist(lapply(nGramDf$Term, function(term) {
      paste(strsplit(term, " ")[[1]][c(1: termCount)], collapse = " ")
    }))
  }

  columnKeys <- function(nGramDf) {
    unlist(lapply(nGramDf$Term, function(term) {
      tail(strsplit(term, " ")[[1]], 1)
    }))
  }

  nGramDf$rowKeys <- rowKeys(nGramDf)
  nGramDf$columnKeys <- columnKeys(nGramDf)
  
  nGramDf
}

load("biGramDataFrame.RData")
biGramDataFrameEnriched <- enrichNGramDataFrame(biGramDataFrame, 1)
save(biGramDataFrameEnriched, file = "biGramDataFrameEnriched.RData")
rm(biGramDataFrame); rm(biGramDataFrameEnriched); gc()

load("triGramDataFrame.RData")
triGramDataFrameEnriched <- enrichNGramDataFrame(triGramDataFrame, 2)
save(triGramDataFrameEnriched, file = "triGramDataFrameEnriched.RData")
rm(triGramDataFrame); rm(triGramDataFrameEnriched); gc()

# ------------------------------------------------------------------------------
# PART 0.1: Further reduce enriched N-Grams
# ------------------------------------------------------------------------------
load("biGramDataFrameEnriched.RData")
summary(subset(biGramDataFrameEnriched, Count >= 8.46))

load("triGramDataFrameEnriched.RData")
summary(subset(triGramDataFrameEnriched, Count >= 3.75))

# ------------------------------------------------------------------------------
# PART 1: Make Transition Matrix with Count
# ------------------------------------------------------------------------------
makeTransitionMatrixCount <- function(enrichedNgramDataFrame) {
  matrix(data = ???, 
         nrow = ???, 
         ncol = ???,
         dimnames = list(???, ???))
}

load("biGramDataFrameEnriched.RData")
biGramTransitionMatrixCount <- 
  makeTransitionMatrixCount(subset(biGramDataFrameEnriched, Count >= 8.46))
save(biGramTransitionMatrixCount, file = "biGramTransitionMatrixCount.RData")
rm(biGramDataFrameEnriched); rm(biGramTransitionMatrixCount); gc()

load("triGramDataFrameEnriched.RData")
triGramTransitionMatrixCount <- 
  makeTransitionMatrixCount(subset(triGramDataFrameEnriched, Count >= 3.75))
save(triGramTransitionMatrixCount, file = "triGramTransitionMatrixCount.RData")
rm(triGramDataFrameEnriched); rm(triGramTransitionMatrixCount); gc()

# ------------------------------------------------------------------------------
# PART 2: Make Transition Matrix with Probability
# ------------------------------------------------------------------------------
load("biGramTransitionMatrixCount.RData")

