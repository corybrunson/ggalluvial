#' Identify axis fields
#' 
#' Identify elements in a character vector that fit the pattern of axis 
#' aesthetic names, and return their indices in the numerical order of the axis 
#' numbers (with \code{axis} first, if present). Only non-negative integers are
#' allowed.
#' 
#' @param x Character vector
get_axes <- function(x) {
  stopifnot(dplyr::n_distinct(x) == length(x))
  axis_ind <- grep("^axis[0-9]*$", x)
  axis_ind[order(as.numeric(gsub("^axis", "", x[axis_ind])), na.last = FALSE)]
}
