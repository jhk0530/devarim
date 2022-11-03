#' Check the consistence of the nodes, flows and palette data.frames
#'
#' @param nodes data.frame containing the nodes definition
#' @param flows data.frame containing the flows definition
#' @param palette data.frame containing the palette definition
#'
#' @return TRUE if all checks are passed; FALSE otherwise.
#' @export
#'
#' @examples
#' nodes <- data.frame(ID=c("A","B"), x=1:2, y=0)
#' flows <- data.frame(from="A", to="B", quantity=10)
#' check_consistency(nodes, flows)
check_consistency <- function(nodes, flows, palette=NULL)
{
  check_nodes(nodes)
  nodes <- parse_nodes(nodes)

  check_flows(flows)
  flows <- parse_flows(flows)

  if (!is.null(palette)) {
    check_palette(palette)
    palette <- parse_palette(palette)
  }

  # Check if all node ID's mentioned in the flows are known nodes
  node_nodes <- unique(nodes$ID)
  for (i in 1:nrow(flows)) {
    # Check "from"
    node <- flows$from[i]
    if (! node %in% node_nodes) {
      browser()
      stop(sprintf("Unknown 'from' node '%s'", node), call.=FALSE)
    }
    # idem "to
    node <- flows$to[i]
    if (! node %in% node_nodes) stop(sprintf("Unknown 'to' node '%s'", node), call.=FALSE)
  }

  # Check for alignment references
  .check_alignment <- function(items, xy, dbg=FALSE) {
    if (dbg) printf("Testing consistency of %s\n", xy)
    if (class(items) != "character") {
      if (dbg) printf("  Non-chr vector; always OK\n")
      return(TRUE)
    }

    abs_pat   <- "^-?[[:digit:]]+([,.][[:digit:]]+)?$" # absolute: e.g. 3.14
    align_pat <- "^\\.?[[:alpha:]][[:alnum:]]*$"         # aligned:  e.g. ".import"
    comp_pat  <- "^\\.?[[:alpha:]][[:print:]]*$"         # computed: e.g. ".import+3"
    for (item in items) {
      if (dbg) printf("  %s", item)
      if (grepl(abs_pat, item)) {
        if (dbg) printf("; absolute value")
        ok <- TRUE # always OK
      } else if (grepl(align_pat, item)) {
        # check if item is known
        ok <- item %in% node_nodes
        if (!ok) {
          msg <- sprintf("%s alignment error: reference to unknown node '%s'", xy, item)
          stop(msg, call.=FALSE)
        }
        if (dbg) printf("; aligned")
      } else if (grepl(comp_pat, item)) {
        # computed item, check if all variables are known
        expr <- parse(text=item)
        vv <- all.vars(expr)
        for (v in vv) {
          if (! v %in% node_nodes) {
            msg <- sprintf("%s alignment error: reference to unknown node '%s' in expression '%s'", xy, v, item)
            stop(msg, call.=FALSE)
          }
        }
        ok <- all(all.vars(expr) %in% node_nodes)
        if (dbg) printf("; computed")
      } else {
        ok <- FALSE # fallthrough case
      }
      if (dbg) printf("; %s\n", ifelse(ok,"OK","NOT OK"))
      if (!ok) stop(sprintf("%s alignment error: '%s'", xy, item), call.=FALSE)
    }
  }
  .check_alignment(nodes$x, "x")
  .check_alignment(nodes$y, "y")

  # Report any nodes that are not used for flows
  flow_nodes <- unique(c(flows$from, flows$to))
  used <- node_nodes %in% flow_nodes
  if (!all(used)) {
    msg <- sprintf("! Not all nodes used for flows: %s\n", paste(node_nodes[!used], collapse=", "))
    message(msg)
  }

  # check colors
  if (!missing(palette)) {
    subs <- unique(flows$substance)
    for (s in subs) {
      if (! s %in% palette$substance) {
        msg <- sprintf("Substance %s not found in color specification", s)
        stop(msg, call.=FALSE)
      }
    }
  }

  TRUE
}
