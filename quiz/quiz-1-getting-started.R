dataDirectory <- ".\\data\\Coursera-SwiftKey\\final"
availableLocale <- c("de_DE", "en_US", "fi_FI", "ru_RU")
# availableLocale[1] # [1] "de_DE"

##
## Q1 
##
fileName <- paste(dataDirectory, availableLocale[2], "en_US.blogs.txt", sep="\\")
fileInfo <- file.info(fileName)
# class(fileInfo) # [1] "data.frame"
?file.info # File size in bytes
fileInfo[, "size"] / 1024^2 "[1] 200.4242"

##
## Q2
## 1. http://www.r-bloggers.com/easy-way-of-determining-number-of-linesrecords-in-a-given-large-file-using-r/
##
fileName <- paste(dataDirectory, availableLocale[2], "en_US.twitter.txt", sep="\\")
allLines <- readLines(fileName)
length(allLines) # [1] 2360148
class(allLines) # [1] "character"

##
## Q3
##
dt <- data.frame(line = allLines)
