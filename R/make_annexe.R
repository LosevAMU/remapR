#' @title Counts MACS-peaks on chromosome
#' @author Alexey Solovyev
#' @description Function counts MACS-peaks on chromosome.
#'
#' @param dirFrom The path to output annexe-file. By default "~/tmp/RData/Output".
#'
#' @return Table of quantity of MACS-peaks on chromosome. In case of success it returns the word "Done".
#'
#' @usage makeAnnexe(dirFrom)
#'
#' @examples
#' makeAnnexe(dirFrom="")
#'
#' @export
makeAnnexe <- function(dirFrom="") {
  if (dirFrom == "") {
    dirFrom <- path.expand("~/tmp/RData/Output")
  }
  dirTo <- dirFrom

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
