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
fetchID <- function(arrayData="", ID="GSE56086") {
  if (class(arrayData) == "character") {
    return(fetchIDFiles(dirFrom=arrayData, ID=ID))
  }
  if (class(arrayData) == "data.frame") {
    return(fetchIDDFrame(dataFrame=arrayData, ID=ID))
  }
  print("I cannot return any values because you use wrong arrayData")
  return("I cannot return any values because you use wrong arrayData")
}
