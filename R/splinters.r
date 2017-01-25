#' Check that one subset decomposition splinters another
#' 
#' Check that the distinct values of one vector splinter those of another
#' vector, in the sense that the binning defined by the former is finer (or
#' equal to) that of the latter, i.e. each bin in the former is a subset of one
#' in the latter.
#' 
#' @param x Vector of any class; encodes the divisor.
#' @param y Vector of any class; encodes the dividend.
splinters <- function(x, y) {
  y_x <- interaction(x, y)
  all(match(x, unique(x)) == match(y_x, unique(y_x)))
}
