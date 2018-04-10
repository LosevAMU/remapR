#' @title How many MACS-peaks are in region for given TF
#' @author Alexey Solovyev
#' @description Function counts quantity of peaks of given TF in region.
#'
#' @param arrayData Data or path to data, by default "~/tmp/RData/Output".
#' @param nameChromosome Name of chromosome.
#' @param begin Position of starting of searching on chromosome.
#' @param end Position of ending of searching on chromosome.
#' @param firstCut Can be "in" or "out". Condition of searching. "in" means that begin of MACS-peak can be before param "begin".
#' "out" means that begin of MACS-peak can be only after param "begin".
#' @param secondCut Can be "in" or "out". Condition of searching. "in" means that end of MACS-peak can be after param "end".
#' "out" means that end of MACS-peak can be only before param "end".
#' @param TF Name of TF.
#'
#' @return Data Frame (class = "data.frame") of quantity of peaks in this address.
#'
#' @usage powerPeaksNR(list of params)
#'
#' @examples
#' myFrame <- powerPeaksNR(arrayData = "", nameChromosome = "chr21", begin = "", end = "",
#' firstCut = "in", secondCut = "in", TF="AR")
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
  # head(frameRep)
  return(frameRep)
}
