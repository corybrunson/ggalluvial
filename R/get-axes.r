#' Identify axis columns
#' 
#' Identify variables in a dataset that fit the pattern of axis aesthetic names,
#' and return their column indices in the numerical order of the axis numbers.
#' 
#' @param data data.frame

get_axes <- function(data) {
    axis_ind <- grep("^axis[0-9\\.]*$", names(data))
    axis_ind[order(as.numeric(gsub("^axis", "", names(data)[axis_ind])))]
}
