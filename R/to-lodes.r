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
#' @param keep Numeric or character vector; which variables among those passed
#'   to \code{axes} to merge into the reshapen data by \code{id}.
#' @export
to_lodes <- function(data,
                     key = "x", value = "stratum", id = "alluvium",
                     axes, keep = NULL) {
  
  stopifnot(is_alluvial(data, axes = axes, silent = TRUE))
  
  if (!is.data.frame(data)) data <- as.data.frame(data)
  
  if (is.character(axes)) {
    axes <- match(axes, names(data))
  }
  axes <- names(data)[axes]
  if (!is.null(keep)) {
    if (is.character(keep)) {
      keep <- match(keep, names(data))
    }
    keep <- names(data)[keep]
    if (!all(keep %in% axes)) {
      stop("All 'keep' variables must be 'axes' variables.")
    }
  }
  strata <- unique(unname(do.call(c, lapply(data[axes],
                                            function(x) levels(as.factor(x))))))
  
  data[[id]] <- 1:nrow(data)
  if (!is.null(keep)) keep_data <- data[, c(id, keep), drop = FALSE]
  for (i in axes) data[[i]] <- as.character(data[[i]])
  
  res <- tidyr::gather_(data,
                        key_col = key, value_col = value,
                        gather_col = axes, factor_key = TRUE)
  res[[value]] <- factor(res[[value]], levels = strata)
  if (!is.null(keep)) res <- dplyr::left_join(res, keep_data, by = id)
  
  res
}
