
#' Format a string
#'
#' This function adds formatting information to a character string by storing
#' this information as the character string's attributes.
#' Run the example to see how it works.
#'
#' All formatting specifiers work as if `gpar()` would be called.
#' (It is, behind the screen.)
#'
#' @param s character string to be formatted
#' @param ... formatting specifiers to be forwarded to gpar()
#'
#' @return formatted string
#' @export
#'
#' @examples
#' s <- strformat("Hello, World", fontsize=18, col="red")
#' str(s)  # show object structure
strformat <- function(s, ...) {
  attr(s,"gp") <- gpar(...)
  return(s)
}
