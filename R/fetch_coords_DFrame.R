#' @title Search by address
#' @author Alexey Solovyev
#' @description Function selects in given address.
#'
#' @param dataFrame Data.
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
#' @usage fetchCoordsDFrame(dataFrame = "", nameChromosome = "chr21", begin = 605400, end = 629900,
#' firstCut = "in", secondCut = "in")
#'
#' @examples
#' myFrame <- fetchCoordsDFrame(dataFrame = "", nameChromosome = "chr21", begin = 605400,
#' end = 6299000, firstCut = "in", secondCut = "in")
#'
#' @export
fetchCoordsDFrame <- function(dataFrame = "",
                             nameChromosome = "chr21",
                             begin = 605400,
                             end = 629900,
                             firstCut = "in",
                             secondCut = "in") {
  annexe <- data.frame()
  if (!(firstCut %in% c("in", "out"))|!(secondCut %in% c("in", "out"))) {
    message("Choose 'firstCut' and 'secondCut' from the following: ", paste( c("'in'", "'out'"), collapse=" "))
    stop()
  }

  dirFrom <- path.expand("~/tmp/RData/Output")



  if (begin > end) {
    message("The 'begin' must be less than the 'end'.")
    stop()
  }

  if (class(dataFrame) == "data.frame") {

    fileNameAnnexe <- paste(dirFrom, "/annexe.RData", sep = "")
    load(fileNameAnnexe)

  if (nameChromosome == ""|(!(nameChromosome %in% annexe$Chromosome))) {


    listChr <- paste(annexe$Chromosome, collapse=" ")
    message("Choose from the following chromosomes: ", listChr)
    stop()
  }

  # myFileOpen <- file.path(dirFrom, paste(nameChromosome, ".RData", sep = ""))
  frameTmp <- dataFrame
  # rm(list = c("chr"))
  if ((firstCut == "in") & (secondCut == "in")) {
    frameRep <- frameTmp[which(frameTmp$Begin <= end & frameTmp$End >= begin), ]
  }

  if ((firstCut == "out") & (secondCut == "out")) {
    frameRep <- frameTmp[which(frameTmp$Begin >= begin & frameTmp$End <= end), ]
  }

  if ((firstCut == "in") & (secondCut == "out")) {
    frameRep <- frameTmp[which(frameTmp$End >= begin & frameTmp$End <= end), ]
  }

  if ((firstCut == "out") & (secondCut == "in")) {
    frameRep <- frameTmp[which(frameTmp$Begin >= begin & frameTmp$Begin <= end), ]
  }

  return(frameRep)
  }

  return(fetchCoordsFiles(dirFrom = dataFrame,
                          nameChromosome = nameChromosome,
                          begin = begin,
                          end = end,
                          firstCut = firstCut,
                          secondCut = secondCut))
}
