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
#'   \item One row per \strong{lode}, wherein each row encodes a subset or
#'         amount of observations having a specific profile of axis values, a
#'         \code{key} field encodes the axis, a \code{value} field encodes the
#'         value within each axis, and a \code{id} column identifies multiple
#'         lodes corresponding to the same subset or amount of observations.
#'   \item One row per \strong{alluvium}, wherein each row encodes a subset or
#'         amount of observations having a specific profile of axis values and a
#'         set \code{axes} of fields encodes its values at each axis variable.
#' }
#' If no arguments are assigned to any of these parameters, then
#' \code{is_alluvial} will default to \code{is_alluvial_alluvia} and assume that
#' all fields in \code{data} (other than \code{weight}, if given) are to be
#' treated as axes.
#' @name is_alluvial
#' @param data A data frame.
#' @param ... Additional parameters used to determine method and passed
#'   thereto. All or none of \code{key}, \code{value}, and \code{id}, or else
#'   optionally \code{axes}, and (in either case) optionally \code{weight}.
#' @param logical Whether to return a logical value (TRUE, the default) or a 
#'   character string indicating the type of alluvial structure ("none", 
#'   "lodes", or "alluvia")
#' @param silent Whether to print warning messages.
#' @param key,value,id Numeric or character; the fields of \code{data}
#'   corresponding to the axis (variable), stratum (value), and alluvium
#'   (identifying) variables.
#' @param axes Numeric or character vector; the field(s) of \code{data}
#'   corresponding to the axi(e)s (variable(s)).
#' @param weight Optional numeric or character; the fields of \code{data}
#'   corresponding to alluvium or lode weights (heights when plotted).
#' @example inst/examples/ex-is-alluvial.r
#' @export
is_alluvial <- function(data, ..., logical = TRUE, silent = FALSE) {
  
  # determine method based on arguments given
  dots <- lazyeval::lazy_dots(...)
  if (!is.null(dots$key) | !is.null(dots$value) | !is.null(dots$id)) {
    if (!is.null(dots$axes)) {
      stop("Arguments to 'key', 'value', and 'id' are mutually exclusive ",
           "with an argument to 'axes'.")
    }
    is_alluvial_lodes(data = data, ..., logical = logical, silent = silent)
  } else {
    is_alluvial_alluvia(data = data, ..., logical = logical, silent = silent)
  }
}

#' @rdname is_alluvial
#' @export
is_alluvial_lodes <- function(
  data,
  key, value, id,
  weight = NULL,
  logical = TRUE, silent = FALSE
) {
  
  if (missing(key) | missing(value) | missing(id)) {
    stop("Each of 'key', 'value', and 'id' is required.")
  }
  
  if (any(duplicated(cbind(data[[key]], data[[id]])))) {
    if (!silent) warning("Duplicated id-axis pairings.")
  }
  
  n_pairs <-
    dplyr::n_distinct(data[[key]]) * dplyr::n_distinct(data[[id]])
  if (nrow(data) < n_pairs) {
    if (!silent) warning("Missing id-axis pairings.")
  }
  
  if (!is.null(weight)) {
    if (!is.numeric(data[[weight]])) {
      message("Lode weights are non-numeric.")
      return(if (logical) FALSE else "none")
    }
  }
  
  if (logical) TRUE else "lodes"
}

#' @rdname is_alluvial
#' @export
is_alluvial_alluvia <- function(
  data,
  axes,
  weight = NULL,
  logical = TRUE, silent = FALSE
) {
  
  if (is.null(weight)) {
    weight <- NULL
  } else {
    if (!is.numeric(data[[weight]])) {
      message("Alluvium weights are non-numeric.")
      return(if (logical) FALSE else "none")
    }
  }
  
  if (is.numeric(weight)) weight <- names(data)[weight]
  axes <- if (missing(axes)) {
    setdiff(names(data), weight)
  } else if (is.character(axes)) {
    match(axes, names(data))
  } else {
    names(data)[axes]
  }
  
  n_alluvia <- nrow(dplyr::distinct(data[axes]))
  n_combns <- do.call(prod, lapply(data[axes], dplyr::n_distinct))
  if (n_alluvia < n_combns) {
    if (!silent) warning("Missing alluvia for some stratum combinations.")
  }
  
  if (logical) TRUE else "alluvia"
}
