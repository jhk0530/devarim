#' Print formatted
#'
#' @param fmt (character) Format
#' @param ... Additional parameters to be sent to sprintf()
#'
#' @keywords internal
#' @noRd
printf <- function(fmt,...) {
  msg <- sprintf(fmt,...)
  #cat(msg)
  msg <- sub("\n","",msg)
  message(msg)
}
