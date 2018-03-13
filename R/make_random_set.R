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
makeRandomSet <- function(dirFrom="", dirTo="", pourcentage=0.0125) {
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
