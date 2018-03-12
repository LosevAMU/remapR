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
fetchCellFiles <- function(dirFrom="", cell="vcap_shctr_r1881") {
  if (dirFrom == "") {
    dirFrom <- path.expand("~/tmp/RData/Output")
  }

  arrayChrom <- gsub(pattern = ".RData", replacement = "", x = grep("^chr", list.files(dirFrom), value = TRUE))
  arrayChromTMP <- c("chr20", "chrY")
  genomeLen <- length(arrayChrom)

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
