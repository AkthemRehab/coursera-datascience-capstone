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

makeReducedData <- function(fileName, factor = 0.7) {
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
        # print(fullQualifiedFileName) 
        # print(makeFqnOutputFilePath(locale, context))
        writeDataToFile(
          makeFqnOutputFilePath(locale, context), 
          makeReducedData(fullQualifiedFileName))
      } else {
        stop("File not found!") 
      }
    }
  }
}

system.time(
  makeSampleFiles())

library(tm)
enUsOutputDirectory <- paste(outputDirectory, locales, sep = filePathSep)

makeCorpus <- function(d) {
  dirSource <- DirSource(directory = d, encoding = "UTF-8")
  ovid <- VCorpus(dirSource, readerControl = list(language = "eng"))
  on.exit(close(dirSource))
  ovid
}

ovid <- makeCorpus(enUsOutputDirectory)

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

Sys.time()
documentTermMatrix <- DocumentTermMatrix(ovid) # This will take a while
Sys.time() # Approx 12 minutes; After more transformations, approx 8 minutes

# class(documentTermMatrix) [1] "DocumentTermMatrix"    "simple_triplet_matrix"
termDocumentMatrix <- as.TermDocumentMatrix(documentTermMatrix)
termDocumentMatrix2 <- removeSparseTerms(termDocumentMatrix, 0.1) # Important !!!

library(slam)
library(reshape2)

termDocumentMatrix3 <- as.matrix(termDocumentMatrix2)
termDocumentMatrix4 <- melt(termDocumentMatrix3, value.name = "Count")
# names(termDocumentMatrix4) # [1] "Terms" "Docs"  "Count"
termDocumentMatrix5 <- termDocumentMatrix4[order(termDocumentMatrix4$Count, decreasing = TRUE), ]
head(termDocumentMatrix5, 100)

library(ggplot2)
top20 <- c(1: 20)
plot1 <- ggplot(termDocumentMatrix5[, top20], aes(Terms, Count))
plot1 <- plot1 + geom_bar(stat = "identity")
plot1

termDocumentMatrix6 <- termDocumentMatrix5[termDocumentMatrix5$count > 7000, ]
associations <- findAssocs(termDocumentMatrix2, as.character(termDocumentMatrix6$Terms), 1.00)
# class(associations) # [1] "list"
termDocumentMatrix6$Terms[1]
associations[[as.character(termDocumentMatrix6$Terms[1])]]

library(wordcloud)
set.seed(1000)
plot2 <- ""
wordcloud(termDocumentMatrix5$Terms, termDocumentMatrix5$count, 
          max.words = 100, colors = brewer.pal(6, "Dark2"))

# library(cluster)
# termDocumentMatrix6 <- termDocumentMatrix5[termDocumentMatrix5$count > 7000, ]
# termDocumentMatrix6
# complete.cases(termDocumentMatrix6)
# d <- dist(as.matrix(termDocumentMatrix6))

# plot3 <- ggplot(termDocumentMatrix5, aes(x = Docs, y = Terms, fill = log10(count)))
# plot3 <- plot3 + geom_title(color = "white")
# plot3 <- plot3 + scale_fill_gradient(high = "#FF0000", low = "#FFFFFF")
# plot3

library(rJava)
library(RWeka)
oneGram <- NGramTokenizer(ovid, Weka_control(min = 1, max = 1, delimiters = " \\r\\n\\t.,;:\"()?!")))
