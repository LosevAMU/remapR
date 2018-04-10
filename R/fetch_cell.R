#' @title Select cell line
#' @author Alexey Solovyev
#' @description Function selects given cell line.
#'
#' @param arrayData Data or path to data, by default "~/tmp/RData/Output".
#' @param cell Name of line.
#'
#' @return Data Frame (class = "data.frame") of peaks of this line.
#'
#' @usage fetchCell(arrayData, cell)
#'
#' @examples
#' myFrame <- fetchCell(arrayData = "", cell = "vcap_shctr_r1881")
#'
#' @export
fetchCell <- function(arrayData = "", cell = "vcap_shctr_r1881") {
  if (class(arrayData) == "character") {
    return(fetchCellFiles(dirFrom=arrayData, cell=cell))
  }
  if (class(arrayData) == "data.frame") {
    return(fetchCellDFrame(dataFrame=arrayData, cell=cell))
  }
  print("I cannot return any values because you use wrong arrayData")
  return("I cannot return any values because you use wrong arrayData")
}
