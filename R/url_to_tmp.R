#' @title Loads file of listing remapR from URL to ~/tmp
#' @author Alexey Solovyev
#' @description Loads listing-file and creates ~/tmp.
#'
#' @param url.in The path to a listing-file.
#' @param tmp.to The name of listing-file.
#'
#' @return Data.Frame from the listing-file.
#'
#' @usage loadFromUrlToTmp(url.in, tmp.to)
#'
#' @examples
#' myFrame <- loadFromUrlToTmp("http://tagc.univ-mrs.fr/remap/download/remapR", "remapR.csv")
#'
#' @export
loadFromUrlToTmp <- function(url.in="http://tagc.univ-mrs.fr/remap/download/remapR",
                             tmp.to="remapR.csv") {
  # from
  myUrl <- url.in
  myFile <- tmp.to
  filePath <- file.path(myUrl, myFile)
  # to
  toDir <- path.expand("~/tmp")
  dir.create(toDir, showWarnings=FALSE)


  # download and import
  # message("it is test message")

  fileExists <- file.exists(file.path(toDir, myFile))

  input <- "Y"
  if (fileExists) {
    input <- readline(prompt=paste("The file ", myFile, " already exists. Do you want to overwrite this file Y/N : "))
    while (input != "Y" && input != "N") {
      input <- readline(prompt="Please type Y or N and press Enter : ")
    }
  }

  if (input == "Y"){
    utils::download.file(filePath, file.path(toDir, myFile))
    dir.create(paste(toDir, "/RData/Input", sep = ""), showWarnings=FALSE)
    dir.create(paste(toDir, "/RData/Output", sep = ""), showWarnings=FALSE)
    dir.create(paste(toDir, "/RData/Output/Full data", sep = ""), showWarnings=FALSE)
    dir.create(paste(toDir, "/RData/Output/Random data", sep = ""), showWarnings=FALSE)
  }


  myFrame <- read.csv(file=file.path(toDir, myFile),
                      colClasses=c("character","character","character","character","character"))
  # class(myFrame)
  return(myFrame)
}
