filePathSep <- "\\"
fileNameSep <- "."
swiftKeyDirectory <- ".\\data\\Coursera-SwiftKey"
finalDirectory <- paste(swiftKeyDirectory, "final", sep = filePathSep)
outputDirectory <- paste(swiftKeyDirectory, "output", sep = filePathSep) 
locales <- c("de_DE", "en_US", "fi_FI", "ru_RU")
contexts <- c("blogs", "news", "twitter")
fileExt <- "txt"

makeFqnOutputFilePath <- function(locale, context) {
  localeDirectory <- paste(outputDirectory, locale, sep = filePathSep)
  dir.create(localeDirectory, showWarnings = FALSE, recursive = TRUE)
  fileName <- paste(locale, context, fileExt, sep = fileNameSep)
  fqnOutputFileName <- paste(localeDirectory, fileName, sep = filePathSep)
  fqnOutputFileName
}

makeReducedData <- function(fileName, factor = 0.1) {
  connection <- file(fileName, "rb")
  contents <- readLines(connection, encoding = "UTF-8")
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
enUsTwitterFileName <- paste(locales[2], contexts[3], fileExt, sep = fileNameSep)
enUsOutputDirectory <- paste(outputDirectory, locales[2], sep = filePathSep)
enUsTwitterFqPath <- paste(enUsOutputDirectory, enUsTwitterFileName, sep = filePathSep)

tryRun <- function() {
  dirSource <- DirSource(directory = enUsOutputDirectory, encoding = "UTF-8")
  ovid <- VCorpus(dirSource, readerControl = list(language = "eng"))
  on.exit(close(dirSource))
  ovid
}
system.time(
  tryRun()) 
# user  system elapsed 
# 6.64    0.02    6.66

ovid <- tryRun()
class(ovid) # [1] "VCorpus" "Corpus"
inspect(ovid)
