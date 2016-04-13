deployToShinyIo <- FALSE
sourceDirectory <- "D:/development/datasciencecoursera/10-data-science-capstone"
buildDirectory <- paste(sourceDirectory, "data-science-capstone", sep = "/")
wwwDirectory <- paste(buildDirectory, "www", sep = "/")
wwwImageDirectory <- paste(buildDirectory, "www/image", sep = "/")

buildShinyApp <- function(deploy = FALSE) {
  resource <- function(name, file) {
    list("name" = name, "file" = file)
  }
  
  if (file.exists(buildDirectory))
    unlink(buildDirectory, recursive = TRUE, force = TRUE)
  
  dir.create(buildDirectory)
  dir.create(wwwDirectory)
  dir.create(wwwImageDirectory)
  
  resources <- list(
    resource("ui", "ui.R"),
    resource("server", "server.R"),
    resource("markovChainModel", "markovChainModelReference.RData"),
    resource("functions", "week4-MarkovChain.R"),
    resource("www/bootstrap.css", "www/bootstrap.css"),
    resource("www/image/background2.jpg", "www/image/background2.jpg"))
  
  for (r in resources) {
    print(paste("Copying ", r$name, " [", r$file, "]", sep = ""))
    src <- paste(sourceDirectory, r$file, sep = "/")
    dst <- paste(buildDirectory, r$file, sep = "/")
    print(paste(src, " to ", dst, sep = ""))
    file.copy(src, dst, overwrite = TRUE, recursive = TRUE)
  }
  
  return(deploy)
}

deployToShinyIo <- buildShinyApp(TRUE)

if (deployToShinyIo == TRUE) {
  print("About to deploy to shiny io")
  setwd(buildDirectory)
  
  library(rsconnect)
  # rsconnect::setAccountInfo(name='yeongwei', token='22D56B3C9194780E36604090C5193B64', 
  #                          secret='GaV6u3cn+czwxWtTokkb/HTN4rAnR60b/nqWVJy6')
  rsconnect::setAccountInfo(name='yeongwei116', token='C27A612098447A57639A934EFD01F74D', 
                            secret='iD566PLAtz3VeLX0TmuHHSmO81/hg1I2xLel4Mfo')
  deployApp(account="yeongwei116")
}