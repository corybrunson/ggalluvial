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
#'   (value), and alluvium (identifying) variables.
#' @param axes Numeric or character vector; which variables to use as axes.
#' @export
to_lodes <- function(data,
                     key = "x", value = "stratum", id = "alluvium",
                     axes) {
  
  stopifnot(is_alluvial(data, axes = axes, silent = TRUE))
  
  if (!is.data.frame(data)) data <- as.data.frame(data)
  
  if (is.character(axes)) {
    axes <- match(axes, names(data))
  } else {
    axes <- names(data)[axes]
  }
  strata <- unique(unname(do.call(c, lapply(data[axes],
                                            function(x) levels(as.factor(x))))))
  for (i in axes) data[[i]] <- as.character(data[[i]])
  
  data[[id]] <- 1:nrow(data)
  
  res <- tidyr::gather_(data,
                        key_col = key, value_col = value,
                        gather_col = axes, factor_key = TRUE)
  res[[value]] <- factor(res[[value]], levels = strata)
  
  res
}
