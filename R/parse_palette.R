#' Parse the information from a 'palette' definition table.
#'
#' This function checks the content of a palette definition,
#' and appends some missing columns.
#' It is mainly used internally, but can be invoked by the uses to see what it does.
#'
#' @param palette data.frame containing the palette definition
#'
#' @param verbose logical: print some information?
#'
#' @return modified palette data.frame
#' @export
#'
#' @examples
#' p0 <- data.frame(substance="any", color="red")
#' str(p0)
#' p1 <- parse_palette(p0)
#' str(p1) # Should be the same!
parse_palette <- function(palette, verbose=TRUE) {

  # Basic checking: substance/color
  check_palette(palette)

  # check for uniqueness of substances
  subs <- palette$substance
  ns <- length(subs)
  if (ns>1) {
    for (i in 2:ns) {
      s <- subs[i]
      if (s %in% subs[1:(i-1)]) {
        msg <- sprintf("Duplicate palette entry for substance '%s'", s)
        stop(msg)
      }
    }
  }

  return(palette)
}
