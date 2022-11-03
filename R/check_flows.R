#' Checks the flows data.frame
#'
#' @param flows flows data.frame
#' @keywords internal
#' @noRd
check_flows <- function(flows)
{
  if (! "data.frame" %in% class(flows)) {
    msg <- sprintf("'flows' is not a data.frame, but a %s", paste(class(flows),collapse=","))
    stop(msg, call.=FALSE)
  }
  .check_field(flows, "from",      c("character","factor"), "chr")
  .check_field(flows, "to",        c("character","factor"), "chr")
  .check_field(flows, "quantity",  c("numeric","integer"),  "num", alias="qty")
}
