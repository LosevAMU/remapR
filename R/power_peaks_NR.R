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
powerPeaksNR <- function(arrayData = "",
                         nameChromosome = "chr21",
                         begin = "",
                         end = "",
                         firstCut = "in",
                         secondCut = "in",
                         TF="AR") {
  annexe <- data.frame()
  if (class(arrayData) == "character") {

    if (arrayData == "") {
      arrayData <- path.expand("~/tmp/RData/Output")
    }

    fileNameAnnexe <- paste(arrayData, "/annexe.RData", sep = "")
    load(fileNameAnnexe)

    if (nameChromosome == ""|(!(nameChromosome %in% annexe$Chromosome))) {
      listChr <- paste(annexe$Chromosome, collapse=" ")
      message("Choisissez parmi les chromosomes suivantes ", listChr)
      stop()
    }

    if (class(begin) == "character") {
      begin <- annexe[annexe$Chromosome == nameChromosome, "minLim"]
    }

    if (class(end) == "character") {
      end <- annexe[annexe$Chromosome == nameChromosome, "maxLim"]
    }

    frameTmp <- remapR::fetchCoordsFiles(dirFrom = arrayData,
                                    nameChromosome = nameChromosome,
                                    begin = begin,
                                    end = end,
                                    firstCut = firstCut,
                                    secondCut = secondCut)
  }

  if (class(arrayData) == "data.frame") {

    if (class(begin) == "character") {
      message("If you use data.frame you have to determine the start of interval")
      stop()
    }

    if (class(end) == "character") {
      message("If you use data.frame you have to determine the end of interval")
      stop()
    }

    frameTmp <- fetchCoordsDFrame(dataFrame = arrayData,
                             nameChromosome = nameChromosome,
                             begin = begin,
                             end = end,
                             firstCut = firstCut,
                             secondCut = secondCut)
  }

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
    rowWow <- nrow(x)
    if (rowWow > 100) {
      message("Wow. I see the power ", rowWow, " in the ", nameChromosome, "-th chromosome.")
    }
    redondance <- append(redondance, rowWow)
  }

  frameRep <- data.frame(Chromosome=nameChromosome,
                         Start=IRanges::start(myIR),
                         End=IRanges::end(myIR),
                         TF,
                         Redondance=redondance)
  return(frameRep)
}
