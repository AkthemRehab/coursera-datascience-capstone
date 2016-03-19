library(slam)
library(reshape2)
library(tm)
library(ggplot2)
library(wordcloud)
library(RWeka)

set.seed(55669)

filePathSep <- "\\"
fileNameSep <- "."
swiftKeyDirectory <- ".\\data\\Coursera-SwiftKey"
finalDirectory <- paste(swiftKeyDirectory, "final", sep = filePathSep)
outputDirectory <- paste(swiftKeyDirectory, "output", sep = filePathSep) 
locales <- c("de_DE", "en_US", "fi_FI", "ru_RU")
locales <- locales[2]
contexts <- c("blogs", "news", "twitter")
fileExt <- "txt"

makeFqnOutputFilePath <- function(locale, context) {
  localeDirectory <- paste(outputDirectory, locale, sep = filePathSep)
  dir.create(localeDirectory, showWarnings = FALSE, recursive = TRUE)
  fileName <- paste(locale, context, fileExt, sep = fileNameSep)
  fqnOutputFileName <- paste(localeDirectory, fileName, sep = filePathSep)
  fqnOutputFileName
}

makeReducedData <- function(fileName, factor = 0.01) {
  connection <- file(fileName, "rb")
  contents <- readLines(connection, encoding = "UTF-8", skipNul = TRUE)
  newContents <- sample(contents, length(contents) * factor)
  on.exit(close(connection))
  newContents
}

writeDataToFile <- function(fileName, data, printFileName = FALSE) {
  write(data, file = fileName) # over write file
  if(printFileName == TRUE) print(fileName)
}

makeSampleFiles <- function() {
  for (locale in locales) {
    for (context in contexts) {
      fileName <- paste(locale, context, fileExt, sep = fileNameSep)
      fullQualifiedFileName <- paste(finalDirectory, locale, fileName, sep = filePathSep)
      if (file.exists(fullQualifiedFileName) == TRUE) {
        writeDataToFile(
          makeFqnOutputFilePath(locale, context), 
          makeReducedData(fullQualifiedFileName))
      } else {
        stop("File not found!") 
      }
    }
  }
}

######################################################
## Produce sample file with 1% worth of orginal data
######################################################
makeSampleFiles()

######################################################
## Construct Corpus object based on directory source
######################################################
enUsOutputDirectory <- paste(outputDirectory, locales, sep = filePathSep)

makeCorpus <- function(d) {
  dirSource <- DirSource(directory = d, encoding = "UTF-8")
  ovid <- VCorpus(dirSource, readerControl = list(language = "eng"))
  on.exit(close(dirSource))
  ovid
}

ovid <- makeCorpus(enUsOutputDirectory)

#########################################################
## Cleaning the text documents within the Corpus object
########################################################
transformCorpus <- function(corpus) {
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, stemDocument)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, PlainTextDocument)
}

ovid <- transformCorpus(ovid)

tagDocumentWithId <- function(corpus) {
  for(i in c(1 : length(corpus))) {
    DublinCore(corpus[[i]], "id") <- i
  }
  corpus
}

ovid <- tagDocumentWithId(ovid)

###################################################################
## Construct DocumentTermMatrix and vice versa from Corpus object 
###################################################################
Sys.time()
documentTermMatrix <- DocumentTermMatrix(ovid) # This will take a while
Sys.time()

#########################################################
## Remove sparse data
########################################################
termDocumentMatrix <- as.TermDocumentMatrix(documentTermMatrix)
termDocumentMatrix2 <- removeSparseTerms(termDocumentMatrix, 0.1) # Important !!!

#########################################################
## Word Cloud Analysis
########################################################
termDocumentMatrix3 <- as.matrix(termDocumentMatrix2)
termDocumentMatrix4 <- melt(termDocumentMatrix3, value.name = "Count")
termDocumentMatrix5 <- aggregate(Count ~ Terms, data = termDocumentMatrix4, sum)
termDocumentMatrix6 <- termDocumentMatrix5[order(termDocumentMatrix5$Count, decreasing = TRUE), ]
termDocumentMatrix6$Terms <- as.character(termDocumentMatrix6$Terms)

wordcloud(termDocumentMatrix6$Terms, termDocumentMatrix6$Count, 
          random.order = FALSE, rot.per = 0.35,
          max.words = 150, colors = brewer.pal(6, "Dark2"))

#########################################################
## N Gram Analysis
########################################################

gramTokenizer <- function(n) {
  NGramTokenizer(ovid, Weka_control(min = n, max = n, delimiters = " \\r\\n\\t.,;:\"()?!"))
}

oneGram <- gramTokenizer(1)
biGram <- gramTokenizer(2)
triGram <- gramTokenizer(3)

oneGramDf <- data.frame(table(oneGram))
biGramDf <- data.frame(table(biGram))
triGramDf <- data.frame(table(triGram))

sanitizeGramDf <- function(df) {
  newDf <- data.frame(Term = as.character(df[, 1]), Count = df[, 2])
  newDf
}

oneGramDf <- sanitizeGramDf(oneGramDf)
biGramDf <- sanitizeGramDf(biGramDf)
triGramDf <- sanitizeGramDf(triGramDf)

sortGramDf <- function(df) {
  df[order(df$Count, decreasing = TRUE), ]
}

oneGramDf <- sortGramDf(oneGramDf)
biGramDf <- sortGramDf(biGramDf)
triGramDf <- sortGramDf(triGramDf)

reductionRows <- c(1: 30)
oneGramDfReduced <- oneGramDf[reductionRows, ]
biGramDfReduced <- biGramDf[reductionRows, ]
triGramDfReduced <- triGramDf[reductionRows, ]

plotNgram <- function(df, titleLabel, xLabel, yLabel) {
  plot1 <- ggplot(df, aes(x = reorder(Term, -Count), y = Count))
  plot1 <- plot1 + geom_bar(stat = "identity")
  plot1 <- plot1 + ggtitle(titleLabel)
  plot1 <- plot1 + labs(x = xLabel, y = yLabel)
  plot1 <- plot1 + theme(axis.text.x = element_text(angle = 45, size = 14, hjust = 1), 
                         plot.title = element_text(size = 20, face = "bold"))
  plot1
}

plotNgram(oneGramDfReduced, "Top 30 1-Gram", "1-Gram", "Count of 1-Gram")
plotNgram(biGramDfReduced, "Top 30 2-Grams", "2-Grams", "Count of 2-Grams")
plotNgram(triGramDfReduced, "Top 30 3-Grams", "3-Grams", "Count of 3-Grams")
