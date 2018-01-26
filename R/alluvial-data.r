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
#' @param diffuse A numeric or character vector indicating which variables among
#'   those passed to \code{axes} to merge into the reshapen data by \code{id}. 
#'   Alternatively, a logical value indicating whether to merge all 
#'   (\code{TRUE}) or none (\code{FALSE}) of these variables.
#' @param distill A logical value indicating whether to include variables, other
#'   than those passed to \code{key} and \code{value}, that vary within values 
#'   of \code{id}. Alternatively, a function (or its name) to be used to distill
#'   each such variable to a single value. In addition to existing functions,
#'   \code{distill} accepts the character values \code{"first"} (used if
#'   \code{distill} is \code{TRUE}), \code{"last"}, and \code{"most"} (which
#'   returns the modal value).
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
  
  key <- ensure_vars(key, data)
  value <- ensure_vars(value, data)
  id <- ensure_vars(id, data)
  
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
    weight <- ensure_vars(weight, data)
  }
  if (is.null(axes)) {
    axes <- setdiff(names(data), weight)
  }
  axes <- ensure_vars(axes, data)
  
  n_alluvia <- nrow(dplyr::distinct(data[axes]))
  n_combns <- do.call(prod, lapply(data[axes], dplyr::n_distinct))
  if (n_alluvia < n_combns) {
    if (!silent) message("Missing alluvia for some stratum combinations.")
  }
  
  if (logical) TRUE else "alluvia"
}

#' @rdname alluvial-data
#' @export
to_lodes <- function(data,
                     key = "x", value = "stratum", id = "alluvium",
                     axes, diffuse = FALSE) {
  
  stopifnot(is_alluvial(data, axes = axes, silent = TRUE))
  
  if (!is.data.frame(data)) data <- as.data.frame(data)
  
  axes <- ensure_vars(axes, data)
  if (is.logical(diffuse)) {
    diffuse <- if (diffuse) axes else NULL
  } else {
    diffuse <- ensure_vars(diffuse, data)
    if (!all(diffuse %in% axes)) {
      stop("All 'diffuse' variables must be 'axes' variables.")
    }
  }
  strata <- unique(unlist(lapply(lapply(data[axes], as.factor), levels)))
  
  data[[id]] <- 1:nrow(data)
  if (!is.null(diffuse)) diffuse_data <- data[, c(id, diffuse), drop = FALSE]
  for (i in axes) data[[i]] <- as.character(data[[i]])
  
  res <- tidyr::gather(data,
                       key = rlang::UQ(key), value = rlang::UQ(value),
                       rlang::UQ(axes),
                       factor_key = TRUE)
  res[[value]] <- factor(res[[value]], levels = strata)
  if (!is.null(diffuse)) {
    res <- merge(res, diffuse_data, by = id, all.x = TRUE, all.y = FALSE)
  }
  
  res
}

#' @rdname alluvial-data
#' @export
to_alluvia <- function(data, key, value, id,
                       distill = FALSE) {
  
  key <- ensure_vars(key, data)
  value <- ensure_vars(value, data)
  id <- ensure_vars(id, data)
  
  stopifnot(is_alluvial(data, key = key, value = value, id = id, silent = TRUE))
  
  # handle any variables that vary within 'id's
  uniq_id <- length(unique(data[[id]]))
  uniq_data <- unique(data[, -match(c(key, value), names(data)), drop = FALSE])
  if (! uniq_id == nrow(uniq_data)) {
    distill_vars <- names(which(sapply(
      setdiff(names(uniq_data), id),
      function(x) nrow(unique(uniq_data[, c(id, x), drop = FALSE]))
    ) > uniq_id))
    if (is.logical(distill)) {
      if (distill) {
        distill <- most
      } else {
        warning("The following variables vary within 'id's ",
                "and will be dropped: ",
                paste(distill_vars, collapse = ", "))
        distill <- NULL
      }
    } else if (is.character(distill)) {
      distill <- get(distill)
    }
    if (!is.null(distill)) {
      stopifnot(is.function(distill))
      message("Distilled variables: ",
              paste(distill_vars, collapse = ", "))
      distill_data <- stats::aggregate(
        data[, match(distill_vars, names(data))],
        data[, id, drop = FALSE],
        distill
      )
      if (length(distill_vars) == 1) names(distill_data)[-1] <- distill_vars
    }
    data <- data[, -match(distill_vars, names(data)), drop = FALSE]
  } else {
    distill <- NULL
  }
  
  res <- tidyr::spread(data, key = rlang::UQ(key), value = rlang::UQ(value))
  if (!is.null(distill)) {
    res <- merge(distill_data, res, by = id, all.x = FALSE, all.y = TRUE)
  }
  
  res
}

ensure_vars <- function(x, data) {
  if (is.character(x)) {
    x <- match(x, names(data))
  }
  names(data)[x]
}

# distilling functions
first <- dplyr::first
last <- dplyr::last
most <- function(x) {
  x[which(factor(x) == names(which.max(table(factor(x)))))[1]]
}
