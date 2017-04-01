#' Variable axes with strata of values
#' 
#' Given a dataset with alluvial structure, \code{stat_stratum} calculates the
#' centroids of the strata for each axis, together with their weights (heights)
#' and widths.
#' 
#' @section Aesthetics:
#' \code{stat_stratum} understands the following aesthetics (required aesthetics
#' are in bold):
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
#' arguments to parameters inconsistent with the data form will be ignored.
#' @name stat-stratum
#' @import ggplot2
#' @seealso \code{\link{geom-stratum}} for the corresponding geom.
#' @inheritParams layer
#' @param width The width of each stratum, as a proportion of the separation
#'   between their centers. Defaults to 1/3.
#' @param axis_width Deprecated; alias for \code{width}.
#' @example inst/examples/ex-stat-stratum.r
#' @usage NULL
#' @export
stat_stratum <- function(mapping = NULL,
                         data = NULL,
                         geom = "stratum",
                         show.legend = NA,
                         inherit.aes = TRUE,
                         width = 1/3, axis_width = NULL,
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
      width = width, axis_width = axis_width,
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
  
  default_aes = aes(weight = 1),
  
  setup_params = function(data, params) {
    
    if (!is.null(params$axis_width)) {
      warning("Parameter 'axis_width' is deprecated; use 'width' instead.")
      params$width <- params$axis_width
      params$axis_width <- NULL
    }
    
    params
  },
  
  setup_data = function(data, params) {
    message("setup_data input")
    print(data)
    
    # ensure that data is in (more flexible) lode form
    axis_ind <- get_axes(names(data))
    if (length(axis_ind) > 0) {
      stopifnot(is_alluvial_alluvia(data, axes = axis_ind, weight = "weight"))
      data <- to_lodes(data = data,
                       key = "x", value = "stratum", id = "alluvium",
                       axes = axis_ind)
      # positioning requires numeric 'x'
      data$x <- as.numeric(as.factor(data$x))
    } else {
      if (is.null(data$x) | is.null(data$stratum) | is.null(data$alluvium)) {
        stop("Parameters 'x', 'stratum', and 'alluvium' are required" ,
             "for data in lode form.")
      }
      stopifnot(is_alluvial_lodes(data,
                                  key = "x", value = "stratum", id = "alluvium",
                                  weight = "weight"))
    }
    
    # incorporate any missing values into factor levels
    if (params$na.rm) {
      data <- na.omit(data)
    } else {
      if (is.factor(data$y)) {
        data$stratum <- addNA(data$stratum, ifany = TRUE)
      } else {
        data$stratum[is.na(data$stratum)] <- "NA"
      }
    }
    message("setup_data output")
    print(data)
    data
  },
  
  compute_panel = function(data, scales,
                           width = 1/3, axis_width = NULL) {
    message("compute_panel input")
    print(data)
    
    # remove empty elements (including labels)
    data <- subset(data, weight > 0)
    
    # aggregate 'weight' by 'x' and 'y' (lose 'group')
    data <- aggregate(x = data$weight,
                      by = data[, c("x", "stratum")],
                      FUN = sum)
    names(data) <- c("x", "label", "weight")
    
    # cumulative weights
    data$y <- NA
    for (xx in unique(data$x)) {
      ww <- which(data$x == xx)
      data$y[ww] <- cumsum(data$weight[ww]) - data$weight[ww] / 2
    }
    # width
    data$width <- width
    #data$ymin <- data$ymax - data$weight
    #data$y <- data$ymax - data$weight / 2
    
    message("compute_panel output")
    print(data)
    data
  }
)
