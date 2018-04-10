#' @title Counts some total information
#' @author Alexey Solovyev
#' @description Function counts sum of peaks, TFs and cellular lines for all IDs.
#'
#' @param dirFrom The path to output file. By default "~/tmp/RData/Output".
#'
#' @return Table of quantity of sum of peaks, TFs and cellular lines for all IDs.
#' In case of success it returns the word "Done".
#'
#' @usage makeSummarySeries(dirFrom)
#'
#' @examples
#' makeSummarySeries(dirFrom="")
#'
#' @export
makeSummarySeries <- function(dirFrom="") {
  if (dirFrom == "") {
    dirFrom <- path.expand("~/tmp/RData/Output")
  }
  dirTo <- dirFrom

  arrayChrom <- gsub(pattern = ".RData", replacement = "", x = grep("^chr", list.files(dirFrom), value = TRUE))
  # arrayChromTMP <- c("chr20", "chrY")
  # genomeLen <- length(arrayChrom)

  # arrayChrom <- arrayChromTMP

  # minLim <- rep.int(x = 0, times = genomeLen)
  # maxLim <- rep.int(x = 0, times = genomeLen)
  # count <- rep.int(x = 0, times = genomeLen)
  #
  # annexe <- data.frame(Chromosome=arrayChrom, minLim, maxLim)
  # row.names(annexe) <- arrayChrom

  summarySeries <- data.frame()
  summaryTmp <- data.frame()


  # i <- arrayChrom[1]
  ind_k <- 1

  for (i in arrayChrom) {
    message(i, ' from ', arrayChrom)
    nameFileTest <- i
    fileNameTest <- paste(nameFileTest, ".RData", sep = "")
    dirTest <- dirFrom
    myFileTest <- file.path(dirTest, fileNameTest)
    frameTmp <- get(load(myFileTest))
    rm(list = c("chr"))
    listSeries <- unique(frameTmp$ID)
    # j <- listSeries[1]
    totSer <- length(listSeries)
    summaryTmp <- data.frame()
    for (j in listSeries) {
      # message(ind_k, ' from ', totSer)
      frameTmpCalcule <- frameTmp[frameTmp$ID == j, ]
      summaryTmp[ind_k, "Serie"] <- j
      summaryTmp[ind_k, "Chromosome"] <- i
      summaryTmp[ind_k, "Sum_Peaks"] <- nrow(frameTmpCalcule)
      summaryTmp[ind_k, "TF_Unique"] <- length(unique(frameTmpCalcule$TF))
      summaryTmp[ind_k, "Cell_Unique"] <- length(unique(frameTmpCalcule$Cell_Mod))
      ind_k <- ind_k + 1
    }
    summarySeries <- rbind(summarySeries, summaryTmp)




  }

  fileNameOutput <- paste("summarySeries", ".RData", sep = "")
  myFileOutput <- file.path(dirTest, fileNameOutput)
  save(summarySeries, file=myFileOutput)



  camelCaps <- "Done"
  return(camelCaps)
}
