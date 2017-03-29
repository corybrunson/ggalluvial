#' Check a data frame for alluvial structure
#' 
#' Alluvial diagrams consist of multiple horizontally-distributed columns (axes)
#' representing factor variables, vertical divisions (strata) of these axes
#' representing these variables' values; and splines (alluvial flows) connecting
#' vertical subdivisions (lodes) within strata of adjacent axes representing
#' subsets or amounts of observations that take the corresponding values of the
#' corresponding variables. This function checks a data frame for either of two
#' types of alluvial structure:
#' \itemize{
#'   \item One row per **lode**, wherein each row encodes a subset or amount of
#'         observations having a specific profile of axis values, a `key` field
#'         encodes the axis, a `value` field encodes the value within each axis,
#'         and a `id` column identifies multiple lodes corresponding to the
#'         same subset or amount of observations.
#'   \item One row per **alluvium**, wherein each row encodes a subset or amount
#'         of observations having a specific profile of axis values and a set
#'         `axes` of fields encodes its values at each axis variable.
#' }
#' @param data A data frame
#' @param logical Whether to return a logical value (TRUE, the default) or a
#'   character string indicating the type of alluvial structure ("none",
#'   "lodes", or "alluvia")
is_alluvial <- function(data, ..., logical = TRUE) {
  
  # determine function to use based on variables assigned
  dots <- lazyeval::lazy_dots(...)
  if (is.null(dots$axes)) {
    if (is.null(dots$key) | is.null(dots$value) | is.null(dots$id)) {
      stop("Each of 'key', 'value', and 'id', or else 'axes', is required.")
    }
    is_alluvial.lodes(data = data, ..., logical = logical)
  } else {
    is_alluvial.alluvia(data = data, ..., logical = logical)
  }
}

is_alluvial.lodes <- function(data, key, value, id, weight, logical = TRUE) {
  
  key_col <- tidyr:::col_name(substitute(key))
  value_col <- tidyr:::col_name(substitute(value))
  id_col <- tidyr:::col_name(substitute(id))
  if (!missing(weight)) weight_col <- tidyr:::col_name(substitute(weight))
  
  if (any(duplicated(cbind(data[[key_col]], data[[id_col]])))) {
    return(if (logical) FALSE else "none")
  }
  
  n_pairs <-
    dplyr::n_distinct(data[[key_col]]) * dplyr::n_distinct(data[[id_col]])
  if (nrow(data) < n_pairs) {
    warning("Missing id-axis pairings.")
  }
  
  if (missing(weight)) warning("Lode weights not provided.") else {
    if (splinters(data[[weight_col]], data[[id_col]]))
      warning("Non-constant lode weights within ids.")
  }
  
  if (logical) TRUE else "lodes"
}

is_alluvial.alluvia <- function(data, axes, weight, logical = TRUE) {
  
  weight_col <- if (missing(weight)) {
    NULL
  } else {
    tidyr:::col_name(substitute(weight))
  }
  axis_cols <- if (missing(axes)) {
    setdiff(colnames(data), weight_col)
  } else if (is.character(axes)) {
    stopifnot(all(axes %in% colnames(data)))
    axes
  } else {
    unname(dplyr::select_vars(colnames(data), axes))
  }
  
  n_alluvia <- nrow(dplyr::distinct(data[axis_cols]))
  n_combns <- do.call(prod, lapply(data[axis_cols], dplyr::n_distinct))
  if (n_alluvia < n_combns) {
    warning("Missing alluvia for some stratum combinations.")
  }
  
  if (logical) TRUE else "alluvia"
}
