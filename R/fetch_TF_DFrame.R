#' @title Select TF
#' @author Alexey Solovyev
#' @description Function selects given TF.
#'
#' @param dataFrame Data frame.
#' @param TF Name of TF.
#'
#' @return Data Frame (class = "data.frame") of peaks of this line.
#'
#' @usage fetchTFDFrame(dataFrame = "", TF = "FOXA1")
#'
#' @examples
#' myFrame <- fetchTFDFrame(dataFrame = "", TF = "FOXA1")
#'
#' @export
fetchTFDFrame <- function(dataFrame = "", TF = "FOXA1") {

  if (class(dataFrame) == "data.frame") {
    return(dataFrame[which(dataFrame$TF == TF), ])
  }

  return(fetchTFFiles(dirFrom=dataFrame, TF=TF))

}
