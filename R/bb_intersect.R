bb_intersect <- function(bb1, bb2) {
  if (bb1$xmax <= bb2$xmin) return(FALSE)
  if (bb1$xmin >= bb2$xmax) return(FALSE)
  if (bb1$ymax <= bb2$ymin) return(FALSE)
  if (bb1$ymin >= bb2$ymax) return(FALSE)
  return(TRUE)
}
