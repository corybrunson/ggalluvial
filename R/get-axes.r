#' Identify axis columns
#' 
#' Identify elements in a character vector that fit the pattern of axis
#' aesthetic names, and return their indices in the numerical order of the axis
#' numbers.
#' 
#' @param x character vector
get_axes <- function(x) {
    axis_ind <- grep("^axis[0-9\\.]*$", x)
    axis_ind[order(as.numeric(gsub("^axis", "", x[axis_ind])))]
}
