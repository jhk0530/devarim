overlapping <- function(g1, g2, dev_scale) {
  bb1 <- bbox(g1, dev_scale)
  bb2 <- bbox(g2, dev_scale)
  return(bb_intersect(bb1,bb2))
}
