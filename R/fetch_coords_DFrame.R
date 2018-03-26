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
fetchCoordsDFrame <- function(dataFrame = "",
                             nameChromosome = "chr1",
                             begin = 605400,
                             end = 629900,
                             firstCut = "in",
                             secondCut = "in") {
  if (!(firstCut %in% c("in", "out"))|!(secondCut %in% c("in", "out"))) {
    message("Choisissez parmi les firstCut et secondCut parmi suivantes ", paste( c("'in'", "'out'"), collapse=" "))
    stop()
  }

  dirFrom <- path.expand("~/tmp/RData/Output")



  if (begin > end) {
    message("Le début doit être inferieur que la fin.")
    stop()
  }

  if (class(dataFrame) == "data.frame") {

    fileNameAnnexe <- paste(dirFrom, "/annexe.RData", sep = "")
    load(fileNameAnnexe)

  if (nameChromosome == ""|(!(nameChromosome %in% annexe$Chromosome))) {


    listChr <- paste(annexe$Chromosome, collapse=" ")
    message("Choisissez parmi les chromosomes suivantes ", listChr)
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
