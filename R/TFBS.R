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
TFBS <- function(arrayData = "",
                 nameChromosome = "chr21",
                 begin = 5240000,
                 end = 5245000,
                 firstCut = "in",
                 secondCut = "in",
                 TF = "MAX",
                 powerNR = 3){

  arrayPower <- remapR:::powerPeaksNR(arrayData = arrayData,
                              nameChromosome = nameChromosome,
                              begin = begin,
                              end = end,
                              firstCut = firstCut,
                              secondCut = secondCut,
                              TF = TF)
  # arrayPower <- frameRep


  tablePower <- (arrayPower[arrayPower$Redondance>=powerNR, ])
  if (nrow(tablePower) == 0) {
    message("There are no TFBS with these conditions. Try another, for exemple powerNR = 1")
    repFrame <- data.frame()

    return(repFrame)
  }
  # head(tablePower)

  fullTable <- remapR:::fetchCoords(arrayData = arrayData,
                                    nameChromosome = nameChromosome,
                                    begin = begin,
                                    end = end,
                                    firstCut = firstCut,
                                    secondCut = secondCut)
  fullTable <- remapR:::fetchTF(arrayData = fullTable,
                                TF = TF)

  repFrame <- data.frame()
  i <- 1

  for (i in seq(nrow(tablePower))) {
    message(i, " from ", nrow(tablePower))
    x <- remapR:::fetchCoords(arrayData = fullTable,
                              nameChromosome = nameChromosome,
                              begin = tablePower[i, "Start"],
                              end = tablePower[i, "End"],
                              firstCut = firstCut,
                              secondCut = secondCut)

    # head(x)
    myIR <- IRanges::IRanges(start = x$Begin,
                             end = x$End)

    y <- IRanges::width(myIR)
    # boxplot(y)

    fuse <- 1
    cutY <- summary(y)[5] + 0.01
    while ((summary(y)[6] - summary(y)[5]) > (summary(y)[5] - summary(y)[2])&(fuse < 10)) {
      cutY <- summary(y)[5] + 0.01
      y <- y[y < cutY]
      fuse <- fuse + 1
    }
    if (fuse > 1){
      z <- myIR[IRanges::width(myIR) <= cutY, ]
    } else {z <- myIR}

    # z <- myIR[IRanges::width(myIR) <= cutY, ]

    frameTMP <- data.frame(Chr = nameChromosome, IRanges::reduce(z))
    repFrame <- rbind(repFrame, frameTMP)
  }

  return(repFrame)
}
