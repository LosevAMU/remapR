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
fetchCell <- function(arrayData="", cell="vcap_shctr_r1881") {
  if (class(arrayData) == "character") {
    return(fetchCellFiles(dirFrom=arrayData, cell=cell))
  }
  if (class(arrayData) == "data.frame") {
    return(fetchCellDFrame(dataFrame=arrayData, cell=cell))
  }
  print("I cannot return any values because you use wrong arrayData")
  return("I cannot return any values because you use wrong arrayData")
}
