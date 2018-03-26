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
frequencyTF <- function(arrayData = ""){

  # arrayData <- remapR::fetchCoords()

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

  return(howManySitesSort)
}
