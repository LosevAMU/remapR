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
fetchCellDFrame <- function(dataFrame="", cell="vcap_shctr_r1881") {

  if (class(dataFrame) == "data.frame") {
    return(dataFrame[which(dataFrame$Cell_Mod == cell), ])
  }

  return(fetchCellFiles(dirFrom=dataFrame, cell=cell))

}
