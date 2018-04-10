#' @title Select cell line
#' @author Alexey Solovyev
#' @description Function selects given cell line.
#'
#' @param arrayData Data frame.
#' @param cell Name of line.
#'
#' @return Data Frame (class = "data.frame") of peaks of this line.
#'
#' @usage fetchCellDFrame(dataFrame, cell)
#'
#' @examples
#' myFrame <- fetchCellDFrame(dataFrame = "", cell = "vcap_shctr_r1881")
#'
#' @export
fetchCellDFrame <- function(dataFrame="", cell="vcap_shctr_r1881") {

  if (class(dataFrame) == "data.frame") {
    return(dataFrame[which(dataFrame$Cell_Mod == cell), ])
  }

  return(fetchCellFiles(dirFrom=dataFrame, cell=cell))

}
