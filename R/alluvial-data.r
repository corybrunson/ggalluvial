#' Check for alluvial structure and convert between alluvial formats
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
#' 

#' \code{to_lodes} takes a data frame with several designated variables to be 
#' used as axes in an alluvial diagram, and reshapes the data frame so that the 
#' axis variable names constitute a new factor variable and their values 
#' comprise another. Other variables' values will be repeated, and a 
#' row-grouping variable can be introduced. This function invokes 
#' \code{\link[tidyr]{gather_}}.
#' 
#' \code{to_alluvia} takes a data frame with axis and axis value variables to be
#' used in an alluvial diagram, and reshape the data frame so that the axes
#' constitute separate variables whose values are given by the value variable.
#' This function invokes \code{\link[tidyr]{spread_}}.
#' 

#' @name alluvial-data
#' @param data A data frame.
#' @param ... Additional parameters used to determine method and passed
#'   thereto. All or none of \code{key}, \code{value}, and \code{id}, or else
#'   optionally \code{axes}, and (in either case) optionally \code{weight}.
#' @param logical Whether to return a logical value or a character string
#'   indicating the type of alluvial structure ("none", "lodes", or "alluvia")
#' @param silent Whether to print warning messages.
#' @param key,value,id Numeric or character; the fields of \code{data}
#'   corresponding to the axis (variable), stratum (value), and alluvium
#'   (identifying) variables.
#' @param axes Numeric or character vector; the field(s) of \code{data}
#'   corresponding to the axi(e)s (variable(s)).
#' @param weight Optional numeric or character; the fields of \code{data}
#'   corresponding to alluvium or lode weights (heights when plotted).
#' @param keep Either a numeric or character vector; which variables among those
#'   passed to \code{axes} to merge into the reshapen data by \code{id}, or
#'   logical, indicating whether to merge all (\code{TRUE}) or none
#'   (\code{FALSE}) of these variables.
#' @example inst/examples/ex-alluvial-data.r
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

#' @rdname alluvial-data
#' @export
is_alluvial_lodes <- function(data,
                              key, value, id,
                              weight = NULL,
                              logical = TRUE, silent = FALSE) {
  
  key <- ensure_columns(key, data)
  value <- ensure_columns(value, data)
  id <- ensure_columns(id, data)
  
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

#' @rdname alluvial-data
#' @export
is_alluvial_alluvia <- function(data,
                                axes = NULL,
                                weight = NULL,
                                logical = TRUE, silent = FALSE) {
  
  if (is.null(weight)) {
    weight <- NULL
  } else {
    if (!is.numeric(data[[weight]])) {
      message("Alluvium weights are non-numeric.")
      return(if (logical) FALSE else "none")
    }
  }
  
  if (!is.null(weight)) {
    weight <- ensure_columns(weight, data)
  }
  if (is.null(axes)) {
    axes <- setdiff(names(data), weight)
  }
  axes <- ensure_columns(axes, data)
  
  n_alluvia <- nrow(dplyr::distinct(data[axes]))
  n_combns <- do.call(prod, lapply(data[axes], dplyr::n_distinct))
  if (n_alluvia < n_combns) {
    if (!silent) warning("Missing alluvia for some stratum combinations.")
  }
  
  if (logical) TRUE else "alluvia"
}

#' @rdname alluvial-data
#' @export
to_lodes <- function(data,
                     key = "x", value = "stratum", id = "alluvium",
                     axes, keep = FALSE) {
  
  stopifnot(is_alluvial(data, axes = axes, silent = TRUE))
  
  if (!is.data.frame(data)) data <- as.data.frame(data)
  
  axes <- ensure_columns(axes, data)
  if (is.logical(keep)) {
    keep <- if (keep) axes else NULL
  } else {
    keep <- ensure_columns(keep, data)
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

#' @rdname alluvial-data
#' @export
to_alluvia <- function(data, key, value, id) {
  
  key <- ensure_columns(key, data)
  value <- ensure_columns(value, data)
  id <- ensure_columns(id, data)
  
  stopifnot(is_alluvial(data, key = key, value = value, id = id, silent = TRUE))
  
  # check that remaining columns are fixed by id
  n_id <- dplyr::n_distinct(data[[id]])
  n_row <- nrow(unique(data[, setdiff(names(data), c(key, value)),
                            drop = FALSE]))
  if (!(n_id == n_row))
    stop("Non-'key'/'value' fields vary within 'id's.")
  
  res <- tidyr::spread_(data, key = key, value = value)
  res[order(res[[id]]), ]
}

ensure_columns <- function(x, data) {
  if (is.character(x)) {
    x <- match(x, names(data))
  }
  names(data)[x]
}
