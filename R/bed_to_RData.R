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
bedToRData <- function(dirFrom="") {

  dirTo <- path.expand("~/tmp/RData/Input")
  if (dirFrom == "") {
    dirFrom <- path.expand("~/tmp/RData/Input")
  }

  for (i in setdiff(list.files(dirFrom), grep("RData$", list.files(dirFrom), value = TRUE))) {
    print(i)
    fileNameInput <- i
    fileNameOutput <- paste(i, ".RData", sep = "")
    dirInput <- dirFrom
    dirOutput <- dirTo
    myFileInput <- file.path(dirInput, fileNameInput)
    myFileOutput <- file.path(dirInput, fileNameOutput)

    myFrame <- read.table(file = myFileInput,
                           header = FALSE, sep = "",
                           col.names = c("Chromosome",
                                         "Begin",
                                         "End",
                                         "Conditions",
                                         "Score",
                                         "Brin",
                                         "Peack1",
                                         "Peack2",
                                         "Color_RGB"),
                           colClasses = c(NA, NA, NA, "character", NA, NA, NA, NA, "character"))

    myFrame$Score <- NULL
    myFrame$Brin <- NULL
    myFrame$Peack2 <- NULL
    myFrame$Color_RGB <- NULL

    myFrame$ID <- matrix(unlist(strsplit(myFrame$Conditions, "[.]")), ncol=3, byrow=TRUE)[,1]
    myFrame$TF <- matrix(unlist(strsplit(myFrame$Conditions, "[.]")), ncol=3, byrow=TRUE)[,2]
    myFrame$Cell_Mod <- matrix(unlist(strsplit(myFrame$Conditions, "[.]")), ncol=3, byrow=TRUE)[,3]

    myFrame$Conditions <- NULL

    save(myFrame, file=myFileOutput)

  }

  camelCaps <- "Done"
  return(camelCaps)
}
