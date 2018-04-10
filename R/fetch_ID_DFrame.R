#' @title Select ID
#' @author Alexey Solovyev
#' @description Function selects given ID.
#'
#' @param dataFrame Data frame.
#' @param ID Name of ID.
#'
#' @return Data Frame (class = "data.frame") of peaks of this line.
#'
#' @usage fetchIDDFrame(dataFrame = "", ID = "GSE56086")
#'
#' @examples
#' myFrame <- fetchIDDFrame(dataFrame = "", ID = "GSE56086")
#'
#' @export
fetchIDDFrame <- function(dataFrame = "", ID = "GSE56086") {

  if (class(dataFrame) == "data.frame") {
    return(dataFrame[which(dataFrame$ID == ID), ])
  }

    return(fetchIDFiles(dirFrom=dataFrame, ID=ID))

}
