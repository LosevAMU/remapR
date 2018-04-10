#' @title Devides by chromosomes
#' @author Alexey Solovyev
#' @description Function devides given file into set of files by name of chromosome.
#'
#' @param dirTo The path to output files. By default "~/tmp/RData/Output".
#'
#' @return In case of success it returns the word "Done".
#'
#' @usage divideByChromosome(dirTo)
#'
#' @examples
#' divideByChromosome(dirTo="")
#'
#' @export
divideByChromosome <- function(dirTo="") {
  totFrame <- data.frame()

  # if (dirFrom == "") {
  #   dirFrom <- path.expand("~/tmp/RData/Output")
  # }
  if (dirTo == "") {
    dirTo <- path.expand("~/tmp/RData/Output")
  }
  pathOpen <- file.choose()
  totFrame <- get(load(pathOpen))


  myChrom <- unique(totFrame$Chromosome)
  print(myChrom)

  # i <- myChrom[1]
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
