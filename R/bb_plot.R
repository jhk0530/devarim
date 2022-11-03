bb_plot <- function(bb, ...) {
  gp <- grid::gpar(...)
  gp$fill <- NA
  grid::grid.polygon(bb$xx, bb$yy, default.units=bb$units, gp=gp)
}
