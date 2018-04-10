#' @title Counts array of intersections of Cis-regulatory elements (CRE)
#' @author Alexey Solovyev
#' @description For every TF the function counts how many TFs can be found on CRE of given TF.
#'
#' @param arrayData Array of MACS-peaks in .bed or .RData format. Default folder is "~/tmp/RData/Output".
#' @param nameChromosome Name of chromosome.
#' @param begin Position of starting of searching on chromosome.
#' @param end Position of ending of searching on chromosome.
#' @param firstCut Can be "in" or "out". Condition of searching. "in" means that begin of MACS-peak can be before param "begin".
#' "out" means that begin of MACS-peak can be only after param "begin".
#' @param secondCut Can be "in" or "out". Condition of searching. "in" means that end of MACS-peak can be after param "end".
#' "out" means that end of MACS-peak can be only before param "end".
#' @param massTF can be number or list of TFs. Number means minimal quantity of presence of MACS-peaks in data, in other words the most frequent TFs.
#'
#' @param powerNR Hom many times TFs can be found in one location.
#' @param tableNormal True or False. The way of presentation of the table: normalized (TRUE) or in the absolute values (FALSE).
#'
#'
#' @return The square table of intersection of the CREs.
#'
#' @usage arrayIntersections( list of params )
#'
#' @examples
#' array <- arrayIntersections <- function(arrayData = "", nameChromosome = "chr21",
#' begin = "", end = "", firstCut = "in", secondCut = "in", massTF = 10000,
#' powerNR = 10, tableNormal = TRUE)
#'
#' @export
arrayIntersections <- function(arrayData = "",
                               nameChromosome = "chr21",
                               begin = "",
                               end = "",
                               firstCut = "in",
                               secondCut = "in",
                               massTF = 10000,
                               powerNR = 10,
                               tableNormal = TRUE) {

  tmpTot <- remapR::TFBSsOnChromosome(arrayData = arrayData,
                                      nameChromosome = nameChromosome,
                                      begin = begin,
                                      end = begin,
                                      firstCut = firstCut,
                                      secondCut = secondCut,
                                      massTF = massTF,
                                      powerNR = powerNR)

  # tmpOrdone <- tmpTot[order(tmpTot[, "start"], decreasing=FALSE), ]
  # tmpTot <- myLTFBS
  # head(tmpTot)
  # head(tmpOrdone)

  TFs <- unique(tmpTot$TF)
  myLen <- length(TFs)

  my.array <- array(0, dim=c(myLen, myLen))
  my.array.norm <- array(0, dim=c(myLen, myLen))
  colnames(my.array) <- TFs
  rownames(my.array) <- TFs
  colnames(my.array.norm) <- TFs
  rownames(my.array.norm) <- TFs

  library(IRanges)

  i <- 1
  for (i in seq(from = 1, to = myLen)) {

    print(TFs[i])

    j <- 1
    for (j in seq(from = 1, to = myLen)) {
      myIR1 <- remapR::DFrameToIRange(arrayData = tmpTot[tmpTot$TF == TFs[i], ])
      myIR2 <- remapR::DFrameToIRange(arrayData = tmpTot[tmpTot$TF == TFs[j], ])
      overLaps <- IRanges::findOverlaps(myIR1, myIR2)
      # length(unique(subjectHits(overLaps)))
      my.array[i, j] <- length(unique(queryHits(overLaps)))
    }
  }

  for (i in seq(from = 1, to = myLen)) {

    for (j in seq(from = 1, to = myLen)) {
      my.array.norm[i, j] <- my.array[i, j] / my.array[i, i]

    }
  }
  if (tableNormal){
    return(my.array.norm)
  }

  return(my.array)

  # dirFrom <- path.expand("~/tmp/RData/Output")
  # fileNameOut <- paste(dirFrom, "/listTFBSor.RData", sep = "")
  # save(tmpTot, file = fileNameOut)

  # dirFrom <- path.expand("~/tmp/RData/Output")
  # pnr <- 10
  # fileNameOut <- paste(dirFrom, "/TFBSs_ordone", pnr, ".bed", sep = "")
  # write.table(tmpOrdone, fileNameOut, sep="\t", row.names=FALSE, quote = FALSE)

}
