# Convert rectangle to polygon
# (lifted from [ggplot2::geom_rect()])
rect_to_poly <- function(xmin, xmax, ymin, ymax) {
  data.frame(
    y = c(ymax, ymax, ymin, ymin, ymax),
    x = c(xmin, xmax, xmax, xmin, xmin)
  )
}

# x-spline coordinates from 2 x bounds, 4 y bounds, and knot position
knots_to_xspl <- function(x0, x1, ymin0, ymax0, ymin1, ymax1, kp0, kp1) {
  x_oneway <- c(x0, x0 + kp0, x1 - kp1, x1)
  data.frame(
    x = c(x_oneway, rev(x_oneway)),
    y = c(ymin0, ymin0, ymin1, ymin1, ymax1, ymax1, ymax0, ymax0),
    shape = rep(c(0, 1, 1, 0), times = 2)
  )
}
