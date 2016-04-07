library(ggplot2)

sanitizeGramDf <- function(df) {
  newDf <- data.frame(Term = as.character(df[, 1]), Count = df[, 2])
  newDf$Term <- as.character(newDf$Term)
  newDf
}

sortGramDf <- function(df) {
  df[order(df$Count, decreasing = TRUE), ]
}

plotNgram <- function(df, titleLabel, xLabel, yLabel) {
  plot1 <- ggplot(df, aes(x = reorder(Term, -Count), y = Count))
  plot1 <- plot1 + geom_bar(stat = "identity")
  plot1 <- plot1 + ggtitle(titleLabel)
  plot1 <- plot1 + labs(x = xLabel, y = yLabel)
  plot1 <- plot1 + theme(axis.text.x = element_text(angle = 45, size = 14, hjust = 1), 
                         plot.title = element_text(size = 20, face = "bold"))
  plot1
}

makeNgramPlots <- function() {
  # TODO: 
}