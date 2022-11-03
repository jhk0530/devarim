
bbox <- function(g, dev_scale) {
  gw <- grid::widthDetails(g)
  gh <- grid::heightDetails(g)

  hjust <- ifelse(is.null(g$hjust), g$just[1], g$hjust)
  vjust <- ifelse(is.null(g$vjust), g$just[2], g$vjust)

  #message("hjust = ", hjust)
  #message("vjust = ", vjust)

  x0 <- grid::convertX(g$x, "inches") * dev_scale
  y0 <- grid::convertY(g$y, "inches") * dev_scale


  if      (hjust == "left")  xx <- c(x0, x0+gw)
  else if (hjust == "right") xx <- c(x0-gw, x0   )
  else                       xx <- c(x0-gw/2, x0+gw/2)

  if      (vjust == "bottom") yy <- c(y0, y0+gh)
  else if (vjust == "top")    yy <- c(y0-gh, y0)
  else                        yy <- c(y0-gh/2, y0+gh/2)

  xx <- xx*72
  yy <- yy*72

  bb <- list(
    ll=list(x=xx[1],y=yy[1]),
    ur=list(x=xx[2],y=yy[2]),
    xx=c(xx[c(1,2,2,1)]),
    yy=c(yy[c(1,1,2,2)]),
    xmin=xx[1],
    xmax=xx[2],
    ymin=yy[1],
    ymax=yy[2],
    height=gh,
    width=gw,
    units="bigpts"
  )
  bb
}
