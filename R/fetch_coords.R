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
fetchCoords <- function(arrayData = "",
                        nameChromosome = "chr1",
                        begin = 605400,
                        end = 629900,
                        firstCut = "in",
                        secondCut = "in") {
  if (class(arrayData) == "character") {
    return(fetchCoordsFiles(dirFrom = arrayData,
                            nameChromosome = nameChromosome,
                            begin = begin,
                            end = end,
                            firstCut = firstCut,
                            secondCut = secondCut))
  }
  if (class(arrayData) == "data.frame") {
    return(fetchCoordsDFrame(dataFrame = arrayData,
                             nameChromosome = nameChromosome,
                             begin = begin,
                             end = end,
                             firstCut = firstCut,
                             secondCut = secondCut))
  }
  print("I cannot return any values because you use wrong arrayData")
  return("I cannot return any values because you use wrong arrayData")
}
