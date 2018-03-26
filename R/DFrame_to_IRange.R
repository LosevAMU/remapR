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
DFrameToIRange <- function(arrayData = "") {


  if (class(arrayData) == "character") {
    message("Use dataframe")
    return(FALSE)
  }
  # load("/home/solovyev/tmp/RData/Output/arrayData.RData")
  myIRtmp <- IRanges::IRanges(start = arrayData[, 2],
                              end = arrayData[, 3])
  return(myIRtmp)
}
