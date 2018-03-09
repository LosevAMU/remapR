#' @title Load chromosomes sizes file
#' @author Alexey Solovyev
#' @description Load preloaded chromosome sizes files.
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
combineRData <- function(dirFrom="", dirTo="") {

  if (dirFrom == "") {
    dirFrom <- path.expand("~/tmp/RData/Input")
  }

  if (dirTo == "") {
    dirTo <- path.expand("~/tmp/RData/Output")
  }

  totFrame <- data.frame()
  # i <- "outaa.RData"
  for (i in grep("RData$", list.files(dirFrom), value = TRUE)) {
    print(i)
    myFileAdd <- file.path(dirFrom, i)
    load(myFileAdd)
    totFrame <- rbind(totFrame, myFrame)
  }

  save(totFrame, file=file.path(dirTo, "Peaks.RData"))
  unlink(".RData")
  rm(myFrame)

  camelCaps <- "Done"
  return(camelCaps)
}
