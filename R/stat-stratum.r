#' Stratum positions
#' 
#' Given a dataset with alluvial structure, \code{stat_stratum} calculates the
#' centroids of the strata at each axis, together with their weights (heights).
#' 
#' @section Aesthetics:
#' \code{stat_stratum} understands the following aesthetics:
#' \itemize{
#'   \item \code{x}
#'   \item \code{stratum}
#'   \item \code{alluvium}
#'   \item \code{axis[0-9]*} (\code{axis1}, \code{axis2}, etc.)
#'   \item \code{weight}
#'   \item \code{group}
#' }
#' Currently, \code{group} is ignored.
#' Use \code{x}, \code{stratum}, and \code{alluvium} for data in lode form and 
#' \code{axis[0-9]*} for data in alluvium form (see \code{\link{is_alluvial}});
#' arguments to parameters inconsistent with the data format will be ignored.
#' 
#' @name stat-stratum
#' @import ggplot2
#' @seealso \code{\link{geom_stratum}} for the corresponding geom,
#'   \code{\link{stat_alluvium}} and \code{\link{geom_alluvium}} for
#'   alluvial flows, and
#'   \code{\link{ggalluvial}} for a shortcut method.
#' @inheritParams layer
#' @param decreasing Logical; whether to arrange the strata at each axis
#'   in the order of the variable values (NA, the default),
#'   with the largest on top (FALSE), or
#'   with the largest on bottom (TRUE).
#' @example inst/examples/ex-stratum.r
#' @usage NULL
#' @export
stat_stratum <- function(mapping = NULL,
                         data = NULL,
                         geom = "stratum",
                         decreasing = NA,
                         show.legend = NA,
                         inherit.aes = TRUE,
                         na.rm = FALSE,
                         ...) {
  layer(
    stat = StatStratum,
    mapping = mapping,
    data = data,
    geom = geom,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      decreasing = decreasing,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname stat-stratum
#' @usage NULL
#' @export
StatStratum <- ggproto(
  "StatStratum", Stat,
  
  setup_data = function(data, params) {
    
    # assign uniform weight if not provided
    if (is.null(data$weight)) {
      data$weight <- rep(1, nrow(data))
    }
    
    type <- get_alluvial_type(data)
    if (type == "none") {
      stop("Data is not in a recognized alluvial form ",
           "(see `?is_alluvial` for details).")
    }
    
    if (params$na.rm) {
      data <- na.omit(object = data)
    } else {
      data <- na_keep(data = data, type = type)
    }
    
    # ensure that data is in lode form
    if (type == "alluvia") {
      axis_ind <- get_axes(names(data))
      data <- to_lodes(data = data,
                       key = "x", value = "stratum", id = "alluvium",
                       axes = axis_ind)
      # positioning requires numeric 'x'
      data$x <- as.numeric(as.factor(data$x))
    }
    
    data
  },
  
  compute_panel = function(self, data, scales,
                           decreasing = NA) {
    
    # remove empty lodes (including labels)
    data <- subset(data, weight > 0)
    
    # nullify 'group' and 'alluvium' fields
    data <- transform(data,
                      group = NULL,
                      alluvium = NULL)
    
    # introduce label (if absent)
    if (is.null(data$label)) data <- transform(data,
                                               label = stratum)
    
    # aggregate data by 'x' and 'stratum'
    data <- auto_aggregate(data = data, by = c("x", "stratum"))
    
    # aggregate 'weight' by 'x' and 'y' (lose 'group')
    #data <- aggregate(x = data$weight,
    #                  by = data[, c("x", "stratum")],
    #                  FUN = sum)
    #names(data) <- c("x", "label", "weight")
    
    # sort according to 'decreasing' parameter
    if (!is.na(decreasing)) {
      data <- if (decreasing) {
        dplyr::arrange(data, PANEL, x, weight)
      } else {
        dplyr::arrange(data, PANEL, x, weight)
      }
    }
    
    # cumulative weights
    data$y <- NA
    for (xx in unique(data$x)) {
      ww <- which(data$x == xx)
      data$y[ww] <- cumsum(data$weight[ww]) - data$weight[ww] / 2
    }
    
    # y bounds
    transform(data,
              ymin = y - weight / 2,
              ymax = y + weight / 2)
  }
)

# automatically summarize over numeric, character, and factor fields
auto_aggregate <- function(data, by) {
  agg <- aggregate(x = rep(1, nrow(data)),
                   by = data[, by],
                   FUN = unique)
  agg[[3]] <- NULL
  agg_vars <- setdiff(names(data), by)
  for (var in agg_vars) {
    agg2 <- aggregate(x = data[[var]],
                      by = data[, by],
                      FUN = if (var %in% c("size", "linetype",
                                           "fill", "color", "alpha",
                                           "PANEL", "group")) {
                        only
                      } else {
                        agg_fn(data[[var]])
                      })
    names(agg2) <- c(by, var)
    agg <- merge(agg, agg2, by = by, all = TRUE)
  }
  agg[do.call(order, agg[, by]), ]
}

# single unique value, or else NA
only <- function(x) {
  uniq <- unique(x)
  if (length(uniq) == 1) {
    uniq
  } else {
    NA
  }
}

# select aggregation function based on variable type
agg_fn <- function(x) {
  if (is.character(x) | is.factor(x)) {
    function(y) {
      uniq <- unique(y)
      if (length(uniq) == 1) as.character(uniq) else NA
    }
  } else if (is.numeric(x)) {
    sum
  } else {
    function(y) NA
  }
}
