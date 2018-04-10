#' @title Search by address
#' @author Alexey Solovyev
#' @description Function selects in given address.
#'
#' @param arrayData Data or path to data, by default "~/tmp/RData/Output".
#' @param nameChromosome Name of chromosome.
#' @param begin Position of starting of searching on chromosome.
#' @param end Position of ending of searching on chromosome.
#' @param firstCut Can be "in" or "out". Condition of searching. "in" means that begin of MACS-peak can be before param "begin".
#' "out" means that begin of MACS-peak can be only after param "begin".
#' @param secondCut Can be "in" or "out". Condition of searching. "in" means that end of MACS-peak can be after param "end".
#' "out" means that end of MACS-peak can be only before param "end".
#'
#' @return Data Frame (class = "data.frame") of peaks in this address.
#'
#' @usage fetchCoords(arrayData = "", nameChromosome = "chr21", begin = "", end = "",
#' firstCut = "in", secondCut = "in")
#'
#' @examples
#' myFrame <- fetchCoords(arrayData = "", nameChromosome = "chr21", begin = "",
#' end = "", firstCut = "in", secondCut = "in")
#'
#' @export
fetchCoords <- function(arrayData = "",
                        nameChromosome = "chr21",
                        begin = "",
                        end = "",
                        firstCut = "in",
                        secondCut = "in") {
  if (class(arrayData) == "character") {
    return(remapR::fetchCoordsFiles(dirFrom = arrayData,
                            nameChromosome = nameChromosome,
                            begin = begin,
                            end = end,
                            firstCut = firstCut,
                            secondCut = secondCut))
  }
  if (class(arrayData) == "data.frame") {
    return(remapR::fetchCoordsDFrame(dataFrame = arrayData,
                             nameChromosome = nameChromosome,
                             begin = begin,
                             end = end,
                             firstCut = firstCut,
                             secondCut = secondCut))
  }
  print("I cannot return any values because you use wrong arrayData")
  return("I cannot return any values because you use wrong arrayData")
}
