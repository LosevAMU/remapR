#' @title Select TF
#' @author Alexey Solovyev
#' @description Function selects given TF.
#'
#' @param arrayData Data or path to data, by default "~/tmp/RData/Output".
#' @param TF Name of TF.
#'
#' @return Data Frame (class = "data.frame") of peaks of this line.
#'
#' @usage fetchTF(arrayData = "", TF = "FOXA1")
#'
#' @examples
#' myFrame <- fetchTF(arrayData = "", TF = "FOXA1")
#'
#' @export
fetchTF <- function(arrayData = "", TF = "FOXA1") {
  if (class(arrayData) == "character") {
    return(fetchTFFiles(dirFrom=arrayData, TF=TF))
  }
  if (class(arrayData) == "data.frame") {
    return(fetchTFDFrame(dataFrame=arrayData, TF=TF))
  }
  print("I cannot return any values because you use wrong arrayData")
  return("I cannot return any values because you use wrong arrayData")
}
