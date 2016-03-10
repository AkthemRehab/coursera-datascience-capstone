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
enUsTwitterFileName <- paste(dataDirectory, availableLocale[2], "en_US.twitter.txt", sep="\\")
enUsTwitter <- readLines(enUsTwitterFileName)
length(enUsTwitter) # [1] 2360148
# class(enUsTwitter) # [1] "character"

##
## Q3
##
q3 <- function(data) {
  longestLength <- 0
  for (l in data) {
    currLength <- nchar(l)
    if (currLength > longestLength) {
      longestLength = currLength
    }
  }
  longestLength
}

enUsBlogsFileName <- paste(dataDirectory, availableLocale[2], "en_US.blogs.txt", sep="\\")
enUsBlogs <- readLines(enUsBlogsFileName)
q3(enUsBlogs) # [1] 40835

enUsNewsFileName <- paste(dataDirectory, availableLocale[2], "en_US.news.txt", sep="\\")
enUsNews <- readLines(enUsNewsFileName)
q3(enUsNews) # [1] 5760

##
## Q4
## grepl reference http://www.regular-expressions.info/rlanguage.html
##
q4 <- function(data) {
  numOfLove <- 0
  numOfHate <- 0
  for (l in data) {
    if (grepl("love", l)) {
      numOfLove <- numOfLove + 1
    } else if(grepl("hate", l)) {
      numOfHate <- numOfHate + 1
    }
  }
  print(paste(numOfLove, "/", numOfHate, sep=" "))
  numOfLove / numOfHate
}
q4(enUsTwitter)
# [1] "90956 / 20725"
# [1] 4.388709

##
## Q5
## http://stackoverflow.com/questions/14348777/how-to-subset-a-vector-of-sentences-containing-a-given-word
##
enUsTwitter[grep("biostats", enUsTwitter)]\
# [1] "i know how you feel.. i have biostats on tuesday and i have yet to study =/"

##
## Q6
##
length(grep("A computer once beat me at chess, but it was no match for me at kickboxing", enUsTwitter))
