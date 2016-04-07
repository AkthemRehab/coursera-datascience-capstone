library(parallel)
library(foreach)
library(doParallel)

startParallelProcessing <- function() {
  noOfCores <- detectCores() - 1
  cluster <- makeCluster(noOfCores)
  registerDoParallel(cluster)
  cluster
}

stopParallelProcessing <- function(cluster) {
  stopCluster(cluster)
  stopImplicitCluster()
}

###########################################
## Sample code for parallel processing
###########################################
# cluster <- startParallelProcessing()
# variableToUse <- NA
# clusterExport(cluster, "variableToUse")
# clusterEvalQ(cluster, library(ggplot2))
# result <- foreach(x = c(1:4),
#                   .combine = c) %dopar% 
#   x^2
# result
# stopParallelProcessing(cluster)
