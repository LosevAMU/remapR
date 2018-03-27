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

fractionCellTF <- function(arrayData = "",
                           nameChromosome = "chr21",
                           begin = "",
                           end = "",
                           firstCut = "in",
                           secondCut = "in",
                           massTF = 50,
                           powerNR = 3,
                           limitPart = 0.2){

  arrBrut <- fetchCoords(arrayData = arrayData,
                         nameChromosome = nameChromosome,
                         begin = begin,
                         end = end,
                         firstCut = firstCut,
                         secondCut = secondCut)
  # head(arrBrut)
  # rm(list = ls()[!grepl("var.name", ls())])

  beginInt <- arrBrut$Begin[1]
  endInt <- arrBrut$End[nrow(arrBrut)]

  if (class(massTF) == "numeric"){
    TFFreq <- remapR::frequencyTF(arrayData = arrBrut)
    TFselected <- head(TFFreq$TF, massTF)

  } else {
    TFselected <- massTF
  }

  allTFBS <- remapR::TFBSsOnChromosome(arrayData = arrBrut,
                                       nameChromosome = nameChromosome,
                                       begin = beginInt,
                                       end = endInt,
                                       firstCut = firstCut,
                                       secondCut = secondCut,
                                       massTF = TFselected,
                                       powerNR = powerNR)
  # head(allTFBS)
  IRsorted <- remapR::DFrameToIRange(allTFBS)
  IRsorted <- reduce(IRsorted)
  nlignes <- length(IRsorted)
  # i <- 1
  myLCell <- data.frame()

  for (i in seq(nlignes)) {
    message(i, " from ", nlignes)
    tmpTot <- arrBrut[which(arrBrut$Begin <= end(IRsorted[i]) & arrBrut$End >= start(IRsorted[i])), ]
    listCell <- unique(tmpTot$Cell_Mod)
    listTF <- unique(tmpTot$TF)
    j <- listCell[1]
    tablTmp <- data.frame()
    vectorCell <- c()
    vectorNom <- c()
    vectorProc <- c()
    for (j in listCell) {
      vectorCell <- c(vectorCell, j)
      vectorNom <- c(vectorNom, length(unique(tmpTot[tmpTot$Cell_Mod == j, ]$TF)))
    }

    tablTmp <- data.frame(Chromosome = tmpTot[1, "Chromosome"],
                          start = start(IRsorted[i]),
                          end = end(IRsorted[i]),
                          Cell_Mod = vectorCell,
                          Quantity_Cell = length(listCell),
                          Quantity_TF_Total = length(listTF),
                          Quantity_TF_Unique= vectorNom,
                          stringsAsFactors = FALSE)
    tablTmp$Ratio <- tablTmp$Quantity_TF_Unique/length(listTF)
    # boxplot(tablTmp$Quantity_TF_Unique)
    abber <- summary(tablTmp$Quantity_TF_Unique)[5] + 1.5 * (summary(tablTmp$Quantity_TF_Unique)[5] - summary(tablTmp$Quantity_TF_Unique)[2])
    # abber
    # head(tablTmp, 20)
    if (abber == 1) {
      abber <- summary(tablTmp$Quantity_TF_Unique)[6] * 0.7
    }
    # abber
    tablTmp <- tablTmp[((tablTmp$Ratio >= limitPart)&(tablTmp$Quantity_TF_Unique > 1))|
                         ((tablTmp$Quantity_TF_Unique >= abber)&(tablTmp$Quantity_TF_Unique > 1)), ]
    # head(tablTmp, 20)
    if (nrow(tablTmp) == 0) {
      next()
      } else {
        myLCell <- rbind(myLCell, tablTmp)
      }
    # i<-i+1
  }

  return(myLCell)
  # dirFrom <- path.expand("~/tmp/RData/Output")
  # fileNameOut <- paste(dirFrom, "/dataCellTF_Ratio.RData", sep = "")
  # save(myLCell, file = fileNameOut)
  # tmpsave <- myLCell[myLCell$Cell_Mod == "K562" &myLCell$Percent > 0.8, ]
  # myLCell[myLCell$start == 10413370, ]
  # head(myLCell, 20)



}
