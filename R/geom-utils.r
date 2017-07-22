# Convert rectangle to polygon
# (lifted from \code{\link[ggplot2]{geom_rect}})
rect_to_poly <- function(xmin, xmax, ymin, ymax) {
  data.frame(
    y = c(ymax, ymax, ymin, ymin, ymax),
    x = c(xmin, xmax, xmax, xmin, xmin)
  )
}

# self-adjoin a dataset, pairing some fields and holding others from one end
self_adjoin <- function(data, key, also.by,
                        pair = NULL, keep0 = NULL, keep1 = NULL) {
  # ensure that 'key' is coercible to numeric
  if (is.character(data[[key]])) data[[key]] <- as.factor(data[[key]])
  # self-(inner )join position aesthetics by numeric-coerced 'key' and 'also.by'
  adj <- dplyr::inner_join(
    transform(data,
              link = as.numeric(data[[key]]))[, c("link", also.by, pair)],
    transform(data,
              link = as.numeric(data[[key]]) - 1)[, c("link", also.by, pair)],
    by = c("link", also.by),
    suffix = c("0", "1")
  )
  # side-join non-position aesthetics
  if (!is.null(keep0)) adj <- dplyr::left_join(
    adj,
    transform(data,
              link = as.numeric(data[[key]]))[, c("link", also.by, keep0)],
    by = c("link", also.by)
  )
  if (!is.null(keep1)) adj <- dplyr::left_join(
    adj,
    transform(data,
              link = as.numeric(data[[key]]) - 1)[, c("link", also.by, keep1)],
    by = c("link", also.by)
  )
  adj
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
