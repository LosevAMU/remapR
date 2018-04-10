#' @title Verification of loading of package
#' @author Alexey Solovyev
#' @description Help start conversations. This function generates some helpful text that can be used to start
#' conversations in all kinds of awkward social situations.
#'
#' @param who The name of the person you wish to start a
#'     conversation with.
#'
#' @return A line of text to be used when starting conversations.
#'
#' @examples
#' hi("Alexey Solovyev")
#'
#' @export
hiAnonyme <- function(who) {
  paste("hello", who, "you have", nchar(who), "letters in your name!")
}
