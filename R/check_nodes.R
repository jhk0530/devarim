#' Checks the nodes data.frame
#'
#' @param nodes nodes data.frame
#' @keywords internal
#' @noRd
check_nodes <- function(nodes)
{
  if (! "data.frame" %in% class(nodes)) {
    msg <- sprintf("'nodes' is not a data.frame, but a %s", paste(class(nodes),collapse=","))
    stop(msg, call.=FALSE)
  }
  .check_field(nodes, "ID", c("character","factor"), "chr")
  .check_field(nodes, "x", c("numeric","integer","character"), "num,chr")
  .check_field(nodes, "y", c("numeric","integer","character"), "num,chr")
}
