#' @title Select cell line
#' @author Alexey Solovyev
#' @description Function selects given cell line.
#'
#' @param dirFrom Path to data, by default "~/tmp/RData/Output".
#' @param cell Name of line.
#'
#' @return Data Frame (class = "data.frame") of peaks of this line.
#'
#' @usage fetchCellFiles(dirFrom="", cell="vcap_shctr_r1881")
#'
#' @examples
#' myFrame <- fetchCellFiles(dirFrom = "", cell = "vcap_shctr_r1881")
#'
#' @export
fetchCellFiles <- function(dirFrom="", cell="vcap_shctr_r1881") {
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

    frameRep <- rbind(frameRep, frameTmp[which(frameTmp$Cell_Mod == cell), ])
  }
  return(frameRep)
}
