filePathSep <- "\\"
fileNameSep <- "."
swiftKeyDirectory <- ".\\data\\Coursera-SwiftKey"
finalDirectory <- paste(swiftKeyDirectory, "final", sep = filePathSep)
outputDirectory <- paste(swiftKeyDirectory, "output", sep = filePathSep) 
localesAvail <- c("de_DE", "en_US", "fi_FI", "ru_RU")
locales <- localesAvail[2]
contexts <- c("blogs", "news", "twitter")
fileExt <- "txt"

getFileInfo <- function(directory) {
  df <- data.frame(name = c(), size = c())
  for (locale in locales) {
    for (context in contexts) {
      fileName <- paste(locale, context, fileExt, sep = fileNameSep)
      fullQualifiedFileName <- paste(directory, locale, fileName, sep = filePathSep)
      if (file.exists(fullQualifiedFileName) == TRUE) {
        fInfo <- file.info(fullQualifiedFileName)
        fileSizeInMb <- paste(round(fInfo$size / 1024 / 1024, 2), "MB")
        df <- rbind(df, data.frame(name = fileName, size = fileSizeInMb))
      } else {
        stop("File not found!") 
      }
    }
  }
  df
}

# Test
# getFileInfo(finalDirectory)

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
