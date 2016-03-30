# http://stackoverflow.com/questions/31316274/implementing-n-grams-for-next-word-prediction

install.packages("tau")
library(tau)
f <- function(queryHistoryTab, query, n = 2) {
  trigrams <- sort(textcnt(rep(tolower(names(queryHistoryTab)), queryHistoryTab), method = "string", 
                           n = length(scan(text = query, what = "character", quiet = TRUE)) + 1))
  query <- tolower(query)
  idx <- which(substr(names(trigrams), 0, nchar(query)) == query)
  res <- head(names(sort(trigrams[idx], decreasing = TRUE)), n)
  res <- substr(res, nchar(query) + 2, nchar(res))
  return(res)
}

history <- c("Can of beer" = 3, "can of Soda" = 2, "A can of water" = 1, "Buy me a can of soda, please" = 2)
class(history) # "numeric"
head(history)
f(history, "Can of")
# [1] "soda" "beer"
names(history) # [1] "Can of beer"                  "can of Soda"                  "A can of water"               "Buy me a can of soda, please"
rep(tolower(names(history)), history)
sort(textcnt(rep(tolower(names(history)), history), method = "string", 
             n = length(scan(text = "1", what = "character", quiet = TRUE)) + 1))


