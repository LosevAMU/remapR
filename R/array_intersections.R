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
arrayIntersections <- function(arrayData = "",
                               nameChromosome = "chr21",
                               begin = "",
                               end = "",
                               firstCut = "in",
                               secondCut = "in",
                               massTF = head(howManySitesSort$TF, 15),
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
