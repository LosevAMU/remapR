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
fetchTF <- function(arrayData="", TF="foxa1") {
  if (class(arrayData) == "character") {
    return(fetchTFFiles(dirFrom=arrayData, TF=TF))
  }
  if (class(arrayData) == "data.frame") {
    return(fetchTFDFrame(dataFrame=arrayData, TF=TF))
  }
  print("I cannot return any values because you use wrong arrayData")
  return("I cannot return any values because you use wrong arrayData")
}
