#' @title CREs for given TF
#' @author Alexey Solovyev
#' @description Function searches the CREs for given TF in region.
#'
#' @param arrayData Data or path to data, by default "~/tmp/RData/Output".
#' @param nameChromosome Name of chromosome.
#' @param begin Position of starting of searching on chromosome.
#' @param end Position of ending of searching on chromosome.
#' @param firstCut Can be "in" or "out". Condition of searching. "in" means that begin of MACS-peak can be before param "begin".
#' "out" means that begin of MACS-peak can be only after param "begin".
#' @param secondCut Can be "in" or "out". Condition of searching. "in" means that end of MACS-peak can be after param "end".
#' "out" means that end of MACS-peak can be only before param "end".
#' @param TF Name of TF.
#' @param powerNR Minimal needed quantity of MACS-peaks to choose CRE.
#'
#' @return Data Frame (class = "data.frame") of set of CREs.
#'
#' @usage TFBS(arrayData = "", nameChromosome = "chr21", begin = 5240000, end = 52450000,
#' firstCut = "in", secondCut = "in", TF = "AR", powerNR = 3)
#'
#' @examples
#' myFrame <- TFBS(arrayData = "", nameChromosome = "chr21", begin = 5240000, end = 52450000,
#' firstCut = "in", secondCut = "in", TF = "AR", powerNR = 3)
#'
#' @export
TFBS <- function(arrayData = "",
                 nameChromosome = "chr21",
                 begin = 5240000,
                 end = 52450000,
                 firstCut = "in",
                 secondCut = "in",
                 TF = "AR",
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
