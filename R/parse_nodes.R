#' Parse the information from a 'nodes' definition table.
#'
#' This function checks the content of a nodes definition,
#' and appends some missing columns.
#' It is mainly used internally, but can be invoked by the uses to see what it does.
#'
#' @param nodes data.frame containing the nodes definition
#' @param verbose logical: print some information?
#'
#' @return modified nodes data.frame
#' @export
#'
#' @examples
#' n0 <- data.frame(ID=c("A","B"), x=1:2, y=0)
#' str(n0)
#' n1 <- parse_nodes(n0)
#' str(n1)
parse_nodes <- function(nodes, verbose=FALSE) {
  check_nodes(nodes) # Basic checking (ID, x, y)

  if (! "label" %in% names(nodes)) nodes$label <- nodes$ID
  .check_field(nodes, "label", c("character","factor"), "chr")

  if (! "dir" %in% names(nodes)) nodes$dir <- "right"
  .check_field(nodes, "dir", c("character","factor"), "chr")

  if (! "label_pos"    %in% names(nodes)) nodes$label_pos    <- "auto"
  if (! "label_anchor" %in% names(nodes)) nodes$label_anchor <- "auto"
  if (! "label_align"  %in% names(nodes)) nodes$label_align  <- "auto"
  .check_field(nodes, "label_pos",    c("character","factor"), "chr")
  .check_field(nodes, "label_anchor", c("character","factor"), "chr")
  .check_field(nodes, "label_align",  c("character","factor"), "chr")

  if (! "mag_pos"    %in% names(nodes)) nodes$mag_pos    <- "auto"
  if (! "mag_anchor" %in% names(nodes)) nodes$mag_anchor <- "auto"
  if (! "mag_align"  %in% names(nodes)) nodes$mag_align  <- "auto"
  .check_field(nodes, "mag_pos",    c("character","factor"), "chr")
  .check_field(nodes, "mag_anchor", c("character","factor"), "chr")
  .check_field(nodes, "mag_align",  c("character","factor"), "chr")

  nodes <- fix_NAs(nodes) # replace any NA in <chr> columns (tibble issue)

  # remove empty lines
  ok <- nchar(nodes$ID)>0
  nodes <- nodes[ok,]

  # replace empty anchor/align entries by "auto"
  for (cn in c("label_anchor","label_align","mag_anchor","mag_align")) {
    idx <- nodes[[cn]] == ""
    nodes[[cn]][idx] <- "auto"
  }

  # Magnitude label: "auto" by default
  idx <- nodes$mag_pos==""
  nodes$mag_pos[idx] <- "auto"

  # make sure x,y columns use 3.14 convention, not 3,14
  if (class(nodes$x)=="character") nodes$x <- gsub(",",".", nodes$x)
  if (class(nodes$y)=="character") nodes$y <- gsub(",",".", nodes$y)

  # Handle hidden nodes
  nodes$hidden <- grepl("^\\.[[:alnum:]_]+", nodes$ID) #check for dot at start
  nodes$ID <- gsub("^\\.","", nodes$ID)                # Remove dot

  # check for uniqueness of nodes
  IDcount <- table(nodes$ID)
  for (i in 1:length(IDcount)) {
    if (IDcount[i] > 1) {
      msg <- sprintf("Duplicate node '%s'", names(IDcount)[i])
      stop(msg, call.=FALSE)
    }
  }

  # Check validity of node directions
  allowable_dirs <- c("left","right","up","down","stock","bottom")
  nnode <- nrow(nodes)
  for (i in 1:nnode) {
    if (! nodes$dir[i] %in% allowable_dirs) {
      msg <- sprintf("Invalid node direction '%s'", nodes$dir[i])
      stop(msg, call.=FALSE)
    }
  }

  # report and exit
  msg <- sprintf("Parsed %d nodes.", nnode)
  if (verbose) message(msg)
  return(nodes)
}
