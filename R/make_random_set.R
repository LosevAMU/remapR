#' @title Constructor of random set
#' @author Alexey Solovyev
#' @description Function constructs a random set of MACS-peaks.
#'
#' @param dirFrom The path to input files. By default "~/tmp/RData/Output".
#' @param dirTo The path to output file. By default "~/tmp/RData/Output/Random data".
#' @param pourcentage Size of random set in percent of original size.
#'
#' @return In case of success it returns the word "Done".
#'
#' @usage makeRandomSet(dirFrom, dirTo, pourcentage)
#'
#' @examples
#' makeRandomSet(dirFrom = "", dirTo = "", pourcentage = 0.0125)
#'
#' @export
makeRandomSet <- function(dirFrom = "", dirTo = "", pourcentage = 0.0125) {
  if (dirFrom == "") {
    dirFrom <- path.expand("~/tmp/RData/Output")
  }

  if (dirTo == "") {
    dirTo <- path.expand("~/tmp/RData/Output/Random data")
  }

  arrayChrom <- gsub(pattern = ".RData", replacement = "", x = grep("^chr", list.files(dirFrom), value = TRUE))
  # arrayChromTMP <- c("chr20", "chrY")
  genomeLen <- length(arrayChrom)

  # arrayChrom <- arrayChromTMP

  for (i in arrayChrom) {
    print(i)
    myFileTest <- i
    load(file.path(dirFrom, paste(myFileTest, ".RData", sep = "")))

    tailFile <- nrow(chr)
    combienLignes <- round(tailFile * pourcentage)
    vecteurDeLignes <- sort(sample(1:tailFile, size = combienLignes))

    randomTable <- chr[vecteurDeLignes,]
    myFileOutput <- file.path(dirTo, paste(myFileTest, ".RData", sep = ""))
    chr <- randomTable
    save(chr, file=myFileOutput)
  }

  remapR::makeAnnexe(dirFrom = dirTo)

  camelCaps <- "Done"
  return(camelCaps)
}
