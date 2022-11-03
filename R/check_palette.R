#' Checks the palette data.frame
#'
#' @param palette palette data.frame
#' @keywords internal
#' @noRd
check_palette <- function(palette)
{
  if (! "data.frame" %in% class(palette)) {
    msg <- sprintf("'palette' is not a data.frame, but a %s", paste(class(palette),collapse=","))
    stop(msg, call.=FALSE)
  }
  .check_field(palette, "substance", c("character","factor"), "chr")
  .check_field(palette, "color",     c("character"),          "chr", alias="colour")
}
