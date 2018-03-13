#' @title Load file remapR from URL to tmp
#' @author Alexey Solovyev
#' @description Load files.
#'
#' @param genome The name of the species to import the chromosomes from.
#'
#' @return The path to a file that contains the chromosome lengths.
#'
#' @usage loadChromFile <- function(genome)
#'
#' @examples
#' hg19ChromFile <- loadChromFile("hg19")
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
  # myFrame
  return(myFrame)
}
