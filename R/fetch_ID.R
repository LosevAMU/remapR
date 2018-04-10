#' @title Select ID
#' @author Alexey Solovyev
#' @description Function selects given ID.
#'
#' @param arrayData Data or path to data, by default "~/tmp/RData/Output".
#' @param ID Name of line.
#'
#' @return Data Frame (class = "data.frame") of peaks of this line.
#'
#' @usage fetchID(arrayData, ID)
#'
#' @examples
#' myFrame <- fetchID(arrayData = "", ID = "GSE56086")
#'
#' @export
fetchID <- function(arrayData = "", ID = "GSE56086") {
  if (class(arrayData) == "character") {
    return(fetchIDFiles(dirFrom=arrayData, ID=ID))
  }
  if (class(arrayData) == "data.frame") {
    return(fetchIDDFrame(dataFrame=arrayData, ID=ID))
  }
  print("I cannot return any values because you use wrong arrayData")
  return("I cannot return any values because you use wrong arrayData")
}
