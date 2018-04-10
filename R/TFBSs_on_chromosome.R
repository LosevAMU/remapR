#' @title CREs for given list of TFs
#' @author Alexey Solovyev
#' @description Function searches the CREs for given TFs in region.
#'
#' @param arrayData Data or path to data, by default "~/tmp/RData/Output".
#' @param nameChromosome Name of chromosome.
#' @param begin Position of starting of searching on chromosome.
#' @param end Position of ending of searching on chromosome.
#' @param firstCut Can be "in" or "out". Condition of searching. "in" means that begin of MACS-peak can be before param "begin".
#' "out" means that begin of MACS-peak can be only after param "begin".
#' @param secondCut Can be "in" or "out". Condition of searching. "in" means that end of MACS-peak can be after param "end".
#' "out" means that end of MACS-peak can be only before param "end".
#' @param massTF List of TFs or quantity of MACS-peaks to choose CRE.
#' @param powerNR Minimal needed quantity of MACS-peaks to choose CRE.
#'
#' @return Data Frame (class = "data.frame") of set of CREs.
#'
#' @usage TFBSsOnChromosome(arrayData = "", nameChromosome = "chr21", begin = "", end = "",
#' firstCut = "in", secondCut = "in", massTF = 10000, powerNR = 20)
#'
#' @examples
#' myFrame <- TFBSsOnChromosome(arrayData = "", nameChromosome = "chr21", begin = "", end = "",
#' firstCut = "in", secondCut = "in", massTF = 10000, powerNR = 20)
#'
#' myFrame <- TFBSsOnChromosome(arrayData = "", nameChromosome = "chr21", begin = "", end = "",
#' firstCut = "in", secondCut = "in", massTF = c("AR", "FOXA1"), powerNR = 10)
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

  test <- fetchCoords(arrayData = arrayData,
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
    tmpArray <- fetchTF(arrayData = test, TF = i)
    tmp <- TFBS(arrayData = tmpArray,
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
