source("./week3-parallelProcessing.R")

setJvmOption <- function() {
  options(java.parameters = "-Xmx8192m" )
}

gramTokenizer <- function(corpus, n) {
  setJvmOption()
  RWeka::NGramTokenizer(corpus, RWeka::Weka_control(min = n, max = n))
}

makeNGrams <- function(ovid) {
  cluster <- startParallelProcessing()
  clusterEvalQ(cluster, function() { 
    setJvmOption()
    library(RWeka)
  })
  
  print(paste("START TIME:", Sys.time()))
  
  ngrams <- foreach(x = c(1:2),
                    .combine = list,
                    .multicombine = TRUE,
                    .export = "ovid") %dopar% gramTokenizer(ovid, x)
  
  stopParallelProcessing(cluster)
  
  print(paste("END TIME:", Sys.time()))
  
  gc()
}