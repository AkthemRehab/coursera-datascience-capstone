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
load("oneGramDataFrame.RData")
summary(subset(oneGramDataFrame, Count >= 13.00))

load("biGramDataFrameEnriched.RData")
summary(subset(biGramDataFrameEnriched, Count >= 8.46))

load("triGramDataFrameEnriched.RData")
summary(subset(triGramDataFrameEnriched, Count >= 3.75))

# ------------------------------------------------------------------------------
# PART 1: Make Transition Matrix with Count
# ------------------------------------------------------------------------------
makeTransitionMatrixCount <- function(dimensionNames, referenceDataFrame) {
  len <- length(dimensionNames)
  transitionMatrix <- 
    matrix(data = rep(0, len),
         nrow = len, ncol = len,
         dimnames = list(dimensionNames, dimensionNames))
  
  for (i in 1: nrow(referenceDataFrame)) {
    row <- referenceDataFrame[i, ]
    if (row$rowKeys %in% dimensionNames && row$columnKeys %in% dimensionNames)
      transitionMatrix[row$rowKeys, row$columnKeys] <- row$Count
  }
  transitionMatrix
}

load("oneGramDataFrame.RData")
load("biGramDataFrameEnriched.RData")
transitionMatrix <- makeTransitionMatrixCount(
  subset(oneGramDataFrame, Count >= 13.00)$Term, biGramDataFrameEnriched)
save(transitionMatrix, file = "transitionMatrix.RData")
rm(oneGramDataFrame); rm(biGramDataFrameEnriched); rm(transitionMatrix);

# ------------------------------------------------------------------------------
# PART 2: Make Transition Matrix with Add-1 Smoothing
# ------------------------------------------------------------------------------
load("transitionMatrix.RData")
dimNames <- dimnames(transitionMatrix)
rowNames <- dimNames[[1]]; colNames <- dimNames[[2]]
rm(dimNames) # identical(rowNames, colNames)

for (rowName in rowNames) {
  for (colName in colNames) {
    transitionMatrix[rowName, colName] <- transitionMatrix[rowName, colName] + 1
  }
}
rm(colName); rm(rowName); 

transitionMatrixAddOneSmoothing <- transitionMatrix
save(transitionMatrixAddOneSmoothing, file = "transitionMatrixAddOneSmoothing.RData")
rm(rowNames); rm(colNames); rm(transitionMatrix); rm(transitionMatrixAddOneSmoothing)
gc()

# ------------------------------------------------------------------------------
# PART 3: Compute probability for Transition Matrix
# ------------------------------------------------------------------------------
load("transitionMatrixAddOneSmoothing.RData")
dimNames <- dimnames(transitionMatrixAddOneSmoothing)
rowNames <- dimNames[[1]]; colNames <- dimNames[[2]]
rm(dimNames)

for (rowName in rowNames) {
  sumOfRow <- sum(transitionMatrixAddOneSmoothing[rowName, ])
  for (colName in colNames) {
    transitionMatrixAddOneSmoothing[rowName, colName] <-
      transitionMatrixAddOneSmoothing[rowName, colName] / sumOfRow
  }
}
rm(sumOfRow); rm(rowNames); rm(colNames);
rm(rowName); rm(colName);
transitionMatrixProbability <- transitionMatrixAddOneSmoothing
save(transitionMatrixProbability, file = "transitionMatrixProbability.RData")
rm(transitionMatrixAddOneSmoothing); rm(transitionMatrixProbability); gc()