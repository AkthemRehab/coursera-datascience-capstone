library(parallel)
library(foreach)
library(doParallel)

noOfCores <- detectCores() - 1
cluster <- makeCluster(noOfCores)
registerDoParallel(cluster)

variableToUse <- NA
clusterExport(cluster, "variableToUse")
clusterEvalQ(cluster, library(ggplot2))

result <- foreach(x = c(1:4),
        .combine = c) %dopar% 
  x^2

stopCluster(cluster)
stopImplicitCluster()

result
