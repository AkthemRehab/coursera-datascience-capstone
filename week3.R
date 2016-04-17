options(java.parameters = "-Xmx8192m" )
options(mc.cores = 3)
library(ggplot2); library(slam);

filePathSep <- "\\"
fileNameSep <- "."
swiftKeyDirectory <- ".\\data\\Coursera-SwiftKey"
finalDirectory <- paste(swiftKeyDirectory, "final", sep = filePathSep)
outputDirectory <- paste(swiftKeyDirectory, "output", sep = filePathSep) 
localesAvail <- c("de_DE", "en_US", "fi_FI", "ru_RU")
locales <- localesAvail[2]
supportedContexts <- c("blogs", "news", "twitter")
contexts <- c(supportedContexts[2], supportedContexts[3])
fileExt <- "txt"
set.seed(55669)

source("./week3-sampleData.R")
makeSampleFiles(0.01) # 3%

source("./week3-constructCorpus.R")
enUsOutputDirectory <- paste(outputDirectory, locales, sep = filePathSep)
ovid <- makeCorpus(enUsOutputDirectory)
ovid <- transformCorpus(ovid)
# ovid <- tagDocumentWithId(ovid)
save(ovid, file="corpus.RData"); rm(ovid)

source("./week3-nGramMaker.R")
load("corpus.RData")
ngrams <- makeNGrams(ovid, c(1:2))
oneGram <- ngrams[[1]]; biGram <- ngrams[[2]]; triGram <- ngrams[[3]];
# fourGram <- ngrams[[4]]; fiveGram <- ngrams[[5]]
rm(ngrams); gc()
save(oneGram, file = "oneGram.RData")
save(biGram, file = "biGram.RData")
save(triGram, file = "triGram.RData")
# save(fourGram, file = "fourGram.RData")
# save(fiveGram, file = "fiveGram.RData")
rm(oneGram); rm(biGram); rm(triGram); 
# rm(fourGram); rm(fiveGram); 
gc()

source("./week3-nGramAnalysis.R")
load("oneGram.RData"); load("biGram.RData"); load("triGram.RData"); 
load("fourGram.RData"); load("fiveGram.RData")
oneGramDf <- sortGramDf(sanitizeGramDf(data.frame(table(oneGram))))
biGramDf <- sortGramDf(sanitizeGramDf(data.frame(table(biGram))))
triGramDf <- sortGramDf(sanitizeGramDf(data.frame(table(triGram))))
fourGramDf <- sortGramDf(sanitizeGramDf(data.frame(table(fourGram))))
fiveGramDf <- sortGramDf(sanitizeGramDf(data.frame(table(fiveGram))))
rm(oneGram); rm(biGram); rm(triGram); rm(fourGram); rm(fiveGram); gc()

reductionRows <- c(1: 30)
oneGramDfReduced <- oneGramDf[reductionRows, ]
biGramDfReduced <- biGramDf[reductionRows, ]
triGramDfReduced <- triGramDf[reductionRows, ]
fourGramDfReduced <- fourGramDf[reductionRows, ]
fiveGramDfReduced <- fiveGramDf[reductionRows, ]

plotNgram(oneGramDfReduced, "Top 30 1-Gram", "1-Gram", "Count of 1-Gram")
plotNgram(biGramDfReduced, "Top 30 2-Grams", "2-Grams", "Count of 2-Grams")
plotNgram(triGramDfReduced, "Top 30 3-Grams", "3-Grams", "Count of 3-Grams")
plotNgram(fourGramDfReduced, "Top 30 4-Grams", "4-Grams", "Count of 4-Grams")
plotNgram(fiveGramDfReduced, "Top 30 5-Grams", "5-Grams", "Count of 5-Grams")
