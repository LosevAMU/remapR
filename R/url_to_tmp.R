#' @title Load file from URL to tmp
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
loadFromUrlToTmp <- function(url.in="http://pedagogix-tagc.univ-mrs.fr/remap/download/remapR",
                             tmp.to="remapR.csv") {
  # from
  myUrl <- url.in
  myFile <- tmp.to
  filePath <- file.path(myUrl, myFile)
  # to
  toDir <- path.expand("~/tmp")
  dir.create(toDir, showWarnings=FALSE)
  # download and import
  utils::download.file(filePath, file.path(toDir, myFile))

  myFrame <- read.csv(file=file.path(toDir, myFile),
                      colClasses=c("character","character","character","character","character"))
  # myFrame
  return(myFrame)
}
