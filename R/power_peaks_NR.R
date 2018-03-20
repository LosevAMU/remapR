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
powerPeaksNR <- function(dirFrom = "",
                         nameChromosome = "chr21",
                         TF="AR") {
  if (dirFrom == "") {
    dirFrom <- path.expand("~/tmp/RData/Output")
  }

  fileNameAnnexe <- paste(dirFrom, "/annexe.RData", sep = "")
  load(fileNameAnnexe)

  if (nameChromosome == ""|(!(nameChromosome %in% annexe$Chromosome))) {
    listChr <- paste(annexe$Chromosome, collapse=" ")
    message("Choisissez parmi les chromosomes suivantes ", listChr)
    stop()
  }

  minl <- annexe[annexe$Chromosome == nameChromosome, "minLim"]
  maxl <- annexe[annexe$Chromosome == nameChromosome, "maxLim"]

  frameTmp <- remapR:::fetchCoordsFiles(nameChromosome = nameChromosome,
                                  begin = minl,
                                  end = maxl,
                                  firstCut = "out",
                                  secondCut = "out")

  frameTmp <- remapR::fetchTF(arrayData = frameTmp, TF = TF)
  # library(IRanges)

  myIR <-IRanges::IRanges(start = frameTmp$Begin,
                          end = frameTmp$End)
  myIR <- IRanges::reduce(myIR)
  redondance <- c()
  # i <- 1

  for (i in seq(length(myIR))) {
    begin <- IRanges::start(myIR[i])
    end <- IRanges::end(myIR[i])
    x <- remapR:::fetchCoords(arrayData = frameTmp,
                              nameChromosome = nameChromosome,
                              begin = begin,
                              end = end,
                              firstCut = "in",
                              secondCut = "in")
    if (nrow(x) > 100) {
      print(nrow(x))
    }
    redondance <- c(redondance, nrow(x))
  }

  frameRep <- data.frame(IRanges::start(myIR), IRanges::end(myIR), redondance)
  return(frameRep)
}
