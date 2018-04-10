#' @title Select ID
#' @author Alexey Solovyev
#' @description Function selects given ID.
#'
#' @param dirFrom Path to data, by default "~/tmp/RData/Output".
#' @param ID Name of ID.
#'
#' @return Data Frame (class = "data.frame") of peaks of this line.
#'
#' @usage fetchIDFiles(dirFrom = "", ID = "GSE56086")
#'
#' @examples
#' myFrame <- fetchIDFiles(dirFrom = "", ID = "GSE56086")
#'
#' @export
fetchIDFiles <- function(dirFrom = "", ID = "GSE56086") {
  if (dirFrom == "") {
    dirFrom <- path.expand("~/tmp/RData/Output")
  }

  arrayChrom <- gsub(pattern = ".RData", replacement = "", x = grep("^chr", list.files(dirFrom), value = TRUE))
  # arrayChromTMP <- c("chr20", "chrY")
  # genomeLen <- length(arrayChrom)

  # arrayChrom <- arrayChromTMP

  frameRep <- data.frame()

  for (i in arrayChrom) {
    print(i)
    nameFileTest <- i
    fileNameTest <- paste(nameFileTest, ".RData", sep = "")
    dirTest <- dirFrom
    myFileTest <- file.path(dirTest, fileNameTest)
    frameTmp <- get(load(myFileTest))
    rm(list = c("chr"))

    frameRep <- rbind(frameRep, frameTmp[which(frameTmp$ID == ID), ])
  }
  return(frameRep)
}
