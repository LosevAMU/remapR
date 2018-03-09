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
makeAnnexe <- function(dirFrom="", dirTo="") {
  if (dirFrom == "") {
    dirFrom <- path.expand("~/tmp/RData/Output")
  }
  if (dirTo == "") {
    dirTo <- path.expand("~/tmp/RData/Output")
  }

  arrayChrom <- gsub(pattern = ".RData", replacement = "", x = grep("^chr", list.files(dirFrom), value = TRUE))
  arrayChromTMP <- c("chr20", "chrY")
  genomeLen <- length(arrayChrom)

  # arrayChrom <- arrayChromTMP

  minLim <- rep.int(x = 0, times = genomeLen)
  maxLim <- rep.int(x = 0, times = genomeLen)
  count <- rep.int(x = 0, times = genomeLen)

  annexe <- data.frame(Chromosome=arrayChrom, minLim, maxLim)
  row.names(annexe) <- arrayChrom

  for (i in arrayChrom) {
    print(i)
    nameFileTest <- i
    fileNameTest <- paste(nameFileTest, ".RData", sep = "")
    dirTest <- dirFrom
    myFileTest <- file.path(dirTest, fileNameTest)
    frameTmp <- get(load(myFileTest))
    rm(list = c("chr"))
    annexe[i,"minLim"] <- min(frameTmp$Begin)
    annexe[i,"maxLim"] <- max(frameTmp$End)
    annexe[i,"count"] <- nrow(frameTmp)
  }

  fileNameOutput <- paste("annexe", ".RData", sep = "")
  myFileOutput <- file.path(dirTest, fileNameOutput)
  save(annexe, file=myFileOutput)



  camelCaps <- "Done"
  return(camelCaps)
}
