#' @title How many regions contain the factor
#' @author Alexey Solovyev
#' @description Function counts the quantity of regions where TF can be found.
#'
#' @param arrayData Data from one of Fetches.
#'
#' @return The sorted list of TFs and the quantity of regions.
#'
#' @usage frequencyTF(arrayData)
#'
#' @examples
#' myList <- frequencyTF(arrayData = "")
#'
#' @export
frequencyTF <- function(arrayData = data.frame){

  # arrayData <- remapR::fetchCoords()
  # arrayData <- arrBrut

  listTF <- unique(arrayData$TF)

  vecTF <- c()
  # i <- "FOXJ2"
  for (i in listTF) {

    tmp <- remapR::DFrameToIRange(arrayData = remapR::fetchTF(arrayData = arrayData, TF = i))
    vecTF <- c(vecTF, length(IRanges::reduce(tmp)))
  }

  howManySites <- data.frame(TF = listTF, Freq = vecTF, stringsAsFactors = FALSE)
  howManySitesSort <- howManySites[order(howManySites$Freq, decreasing = TRUE), ]
  # str(howManySitesSort)
  # head(howManySitesSort)

  return(howManySitesSort)
}
