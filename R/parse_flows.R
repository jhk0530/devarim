#' Parse the information from a 'flows' definition table.
#'
#' This function checks the content of a flows definition,
#' and appends some missing columns.
#' It is mainly used internally, but can be invoked by the uses to see what it does.
#'
#' @param flows data.frame containing the nodes definition
#' @param verbose logical: print some information?
#'
#' @return modified flows data.frame
#' @export
#'
#' @examples
#' Q0 <- data.frame(from="A", to="B", qty=10) # Note 'qty' as alias for quantity
#' str(Q0)
#' Q1 <- parse_flows(Q0)
#' str(Q1)
parse_flows <- function(flows, verbose=FALSE) {

  # Basic check (from/to/quantity)
  check_flows(flows)

  # Allow 'qty' alias for quantity
  if (! "quantity" %in% names(flows)) {
    idx <- match("qty", names(flows))
    if (is.na(idx)) stop("Flows column quantity missing and no alias present. Should not happen!")
    names(flows)[idx] <- "quantity"
  }

  if (! "substance" %in% names(flows)) flows$substance="<any>"
  .check_field(flows, "substance", c("character","factor"), "chr")

  # remove empty lines
  ok    <- is.finite(flows$quantity)
  flows <- flows[ok,]
  nflow <- nrow(flows)

  flows <- fix_NAs(flows) # replace any NA in <chr> columns (tibble issue)

  # fill up empty placeholders for "from", "to" and "substance"
  fill_down <- function(vec) {
    missing <- function(x) { is.na(x) || x=="" }
    stopifnot(!missing(vec[1]))
    for (i in 1:nflow) if (missing(vec[i])) vec[i] <- vec[i-1]
    return(vec)
  }

  flows$from      <- fill_down(flows$from)
  flows$to        <- fill_down(flows$to)
  flows$substance <- fill_down(flows$substance)

  # count
  nnode <- length(unique(c(flows$from, flows$to)))
  nsubs <- length(unique(flows$substance))

  # report and exit
  msg <- sprintf("Parsed %d flows for %d substance types between %d nodes.", nflow, nsubs, nnode)
  if (verbose) message(msg)
  return(flows)
}
