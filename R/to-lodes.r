#' Combine multiple axis variables into an axis indicator and an axis value
#' 
#' Take a data frame with several designated variables to be used as axes in an 
#' alluvial diagram, and reshape the data frame so that the axis variable names 
#' constitute a new factor variable and their values comprise another. Other 
#' variables' values will be repeated, and a row-grouping variable can be 
#' introduced. This function invokes \code{\link[tidyr]{gather}}.
#' 
#' @param data Data frame.
#' @param key,value,id Character; names given to the axis (variable), stratum
#'   (value), and alluvium (identifying) variables. Default to "key", "value",
#'   and "id".
#' @param axes Numeric or character vector; which variables to use as axes.
to_lodes <- function(data,
                     key = "key", value = "value", id = "id",
                     axes) {
  
  stopifnot(suppressWarnings(is_alluvial(data, axes = axes)))
  
  if (!is.data.frame(data)) data <- as.data.frame(data)
  
  if (is.character(axes)) {
    axes <- match(axes, names(data))
  } else {
    axes <- names(data)[axes]
  }
  for (i in axes) data[[i]] <- as.character(data[[i]])
  
  data[[id]] <- 1:nrow(data)
  
  tidyr::gather_(data, key = key, value = value, axes, factor_key = TRUE)
}
