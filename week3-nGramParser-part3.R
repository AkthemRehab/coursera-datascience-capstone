library("markovchain")
# ------------------------------------------------------------------------------
# PART 5: Build Markov model
# ------------------------------------------------------------------------------

load("transitionMatrixProbability.RData")
markovChainModel <- new("markovchain", transitionMatrix = transitionMatrixProbability)
save(markovChainModel, file = "markovChainModel.RData")
rm(transitionMatrixProbability); rm(markovChainModel); gc()

# ------------------------------------------------------------------------------
# PART 6: Build Markov model based reference
# ------------------------------------------------------------------------------
load("dormantroot/transitionMatrix.RData")
markovChainModelReference <- new("markovchain", transitionMatrix = transitionMatrix)
save(markovChainModelReference, file = "markovChainModelReference.RData")
rm(transitionMatrix); rm(markovChainModelReference);
