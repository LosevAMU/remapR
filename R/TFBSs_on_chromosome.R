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
TFBSsOnChromosome <- function(arrayData = "",
                              nameChromosome = "chr21",
                              begin = "",
                              end = "",
                              firstCut = "in",
                              secondCut = "in",
                              massTF = 10000,
                              powerNR = 20) {

  test <- remapR::fetchCoords(arrayData = arrayData,
                              nameChromosome = nameChromosome,
                              begin = begin,
                              end = end,
                              firstCut = firstCut,
                              secondCut = secondCut)
  beginInt <- test$Begin[1]
  endInt <- test$End[nrow(test)]

  if (class(massTF) == "numeric"){
    selectTF <- table(test$TF)
    bignames <- names(selectTF[selectTF>=massTF])
  } else {
    bignames <- massTF
  }
  # i <- "ARNT"
  myLTFBS <- data.frame()
  for (i in bignames) {
    message('We are looking for TFBS of ', i)
    tmpArray <- remapR::fetchTF(arrayData = test, TF = i)
    tmp <- remapR::TFBS(arrayData = tmpArray,
                         nameChromosome = nameChromosome,
                         begin = beginInt,
                         end = endInt,
                         firstCut = firstCut,
                         secondCut = secondCut,
                         TF = i,
                         powerNR = powerNR)
    # head(tmp)
    if (length(tmp) == 0) {next()}
    tmp$TF <- i
    myLTFBS <- rbind(myLTFBS, tmp)
  }

  return(myLTFBS)

}
