#' @title Combines a lot of files .RData in one file
#' @author Alexey Solovyev
#' @description Combines a lot of files .RData in one file.
#'
#' @param dirFrom The path to input files. By default "~/tmp/RData/Input".
#' @param dirTo The path to output file. By default "~/tmp/RData/Output".
#'
#' @return In case of success it returns the word "Done".
#'
#' @usage combineRData(dirFrom="", dirTo="")
#'
#' @examples bedToRData(dirFrom="", dirTo="")
#'
#' @export
combineRData <- function(dirFrom="", dirTo="") {

  if (dirFrom == "") {
    dirFrom <- path.expand("~/tmp/RData/Input")
  }

  if (dirTo == "") {
    dirTo <- path.expand("~/tmp/RData/Output")
  }

  totFrame <- data.frame()
  myFrame <- data.frame()
  # i <- "outaa.RData"
  for (i in grep("RData$", list.files(dirFrom), value = TRUE)) {
    print(i)
    myFileAdd <- file.path(dirFrom, i)
    load(myFileAdd)
    totFrame <- rbind(totFrame, myFrame)
  }

  save(totFrame, file=file.path(dirTo, "Peaks.RData"))
  # unlink(".RData")
  rm(myFrame)

  camelCaps <- "Done"
  return(camelCaps)
}
