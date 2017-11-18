#' Expand an axis indicator and an axis value into multiple axis variables
#' 
#' Take a data frame with axis and axis value variables to be used in an 
#' alluvial diagram, and reshape the data frame so that the axes constitute 
#' separate variables whose values are given by the value variable. This 
#' function invokes \code{\link[tidyr]{spread_}}.
#' 
#' 
#' @param data Data frame.
#' @param key,value,id Numeric or character; the fields corresponding to the
#'   axis (variable), stratum (value), and alluvium (identifying) variables.
#' @export
to_alluvia <- function(data, key, value, id) {
  
  if (missing(key) | missing(value) | missing(id)) {
    stop("Each of 'key', 'value', and 'id' is required.")
  }
  
  stopifnot(is_alluvial(data, key = key, value = value, id = id, silent = TRUE))
  
  if (is.numeric(key)) key <- names(data)[key]
  if (is.numeric(value)) value <- names(data)[value]
  
  # check that remaining columns are fixed by id
  n_id <- dplyr::n_distinct(data[[id]])
  n_row <- nrow(unique(data[, setdiff(names(data), c(key, value)),
                            drop = FALSE]))
  if (!(n_id == n_row))
    stop("Non-'key'/'value' fields vary within 'id's.")
  
  res <- tidyr::spread_(data, key = key, value = value)
  res[order(res[[id]]), ]
}
