
#' Checks the mass balance of the flows involved
#'
#' For each substance involved, the balance per (internal) node is inspected.
#' If outflow exceed inflow, or vice versa, a message is printed,
#' and the function returns FALSE.
#'
#' @param nodes data.frame containing the nodes definition
#' @param flows data.frame containing the flows definition
#' @param tolerance numeric specifying a tolerance. Default is 0.01 (1%)
#'
#' @return TRUE if balanced,  FALSE if not.
#' @export
#'
#' @examples
#' nodes <- data.frame(ID=c("A","B","C"), x=1:3, y=1:3, dir=c("right","right","stock"))
#' flows <- data.frame(from=c("A","B"), to=c("B","C"), quantity=c(10,10))
#' check_balance(nodes,flows)
check_balance <- function(nodes,flows, tolerance=0.01) {
  check_nodes(nodes)
  nodes <- parse_nodes(nodes)
  check_flows(flows)
  flows <- parse_flows(flows)

  balanced <- TRUE

  subs <- unique(flows$substance)
  ns <- length(subs)
  for (s in subs) {
    # printf("Looking at: %s\n", type)
    idx <- flows$substance == s
    sub_flows <- flows[idx, ]
    flow_nodes <- base::intersect(sub_flows$from, sub_flows$to)
    for (node in flow_nodes) {
      # skip 'stock' nodes
      i <- match(node, nodes$ID)
      if (!is.na(i) && nodes$dir[i] == "stock") next

      idx <- which(sub_flows$to == node)
      inflow <- sum(sub_flows$quantity[idx])
      idx <- which(sub_flows$from == node)
      outflow <- sum(sub_flows$quantity[idx])
      # outflow too high? : shortage
      if (outflow > (1+tolerance)*inflow) {
        abs_shortage <- outflow - inflow
        rel_shortage <- 100 * abs_shortage / inflow
        msg <- sprintf("! Balance error for '%s' in node '%s': Inflow=%.2f; Outflow=%.2f; Shortage=%.2f (%.1f%%)\n",
                       s, node, inflow, outflow, abs_shortage, rel_shortage)
        warning(msg)
        balanced <- FALSE
      }
      # outflow too low? Excess...
      if (outflow < (1-tolerance)*inflow) {
        abs_excess <- inflow - outflow
        rel_excess <- 100 * abs_excess / inflow
        msg <- sprintf("! Balance error for '%s' in node '%s': Inflow=%.2f; Outflow=%.2f; Excess=%.2f (%.1f%%)\n",
                       s, node, inflow, outflow, abs_excess, rel_excess)
        warning(msg)
        balanced <- FALSE
      }
    }
  }

  return(balanced)
}
