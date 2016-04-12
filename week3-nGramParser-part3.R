# ------------------------------------------------------------------------------
# PART 5: Build Markov model
# ------------------------------------------------------------------------------
library("markovchain")

load("transitionMatrixProbability.RData")
markovChainModel <- new("markovchain", transitionMatrix = transitionMatrixProbability)
save(markovChainModel, file = "markovChainModel.RData")
rm(transitionMatrixProbability); rm(markovChainModel); gc()
