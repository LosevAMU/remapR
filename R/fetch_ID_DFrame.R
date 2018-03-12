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
fetchIDDFrame <- function(dataFrame="", ID="GSE56086") {

  if (class(dataFrame) == "data.frame") {
    return(dataFrame[which(dataFrame$ID == ID), ])
  }

    return(fetchIDFiles(dirFrom=dataFrame, ID=ID))

}
