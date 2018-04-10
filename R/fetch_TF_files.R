#' @title Select TF
#' @author Alexey Solovyev
#' @description Function selects given TF.
#'
#' @param dirFrom Path to data, by default "~/tmp/RData/Output".
#' @param TF Name of TF.
#'
#' @return Data Frame (class = "data.frame") of peaks of this line.
#'
#' @usage fetchTFFiles(dirFrom = "", TF = "FOXA1")
#'
#' @examples
#' myFrame <- fetchTFFiles(dirFrom = "", TF = "FOXA1")
#'
#' @export
fetchTFFiles <- function(dirFrom = "", TF = "FOXA1") {
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

    frameRep <- rbind(frameRep, frameTmp[which(frameTmp$TF == TF), ])
  }
  return(frameRep)
}
