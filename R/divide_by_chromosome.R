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
divideByChromosome <- function(dirFrom="", dirTo="") {

  if (dirFrom == "") {
    dirFrom <- path.expand("~/tmp/RData/Output")
  }
  if (dirTo == "") {
    dirTo <- path.expand("~/tmp/RData/Output")
  }
  pathOpen <- file.choose()
  # totFrame <- get(load(pathOpen))
  load(pathOpen)


  myChrom <- unique(totFrame$Chromosome)
  print(myChrom)

  for (i in myChrom) {

    print(i)
    fileChrome <- file.path(dirTo, paste(i, ".RData", sep = ""))
    chr <- totFrame[which(totFrame$Chromosome == i), ]
    save(chr, file=fileChrome)
    rm(chr)

  }
  rm(totFrame)



  camelCaps <- "Done"
  return(camelCaps)
}
