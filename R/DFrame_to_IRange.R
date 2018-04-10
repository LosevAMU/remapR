#' @title DataFrame to IRange
#' @author Alexey Solovyev
#' @description This function converts DataFrame to IRange.
#'
#' @param arrayData The data to convert. Second column of arrayData is start of IRange,
#' third column of arrayData is end of IRange. For exemple, Data from one of Fetches.
#'
#' @return The data in IRange format.
#'
#' @usage DFrameToIRange(arrayData)
#'
#' @examples
#' myIRange  <- DFrameToIRange(data.frame)
#'
#' @export
DFrameToIRange <- function(arrayData = "") {


  if (class(arrayData) == "character") {
    message("Use dataframe")
    return(FALSE)
  }
  # load("/home/solovyev/tmp/RData/Output/arrayData.RData")
  myIRtmp <- IRanges::IRanges(start = arrayData[, 2],
                              end = arrayData[, 3])
  return(myIRtmp)
}
