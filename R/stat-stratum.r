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
#' @example inst/examples/ex-stratum.r
#' @usage NULL
#' @export
stat_stratum <- function(mapping = NULL,
                         data = NULL,
                         geom = "stratum",
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
    
    # ensure that data is in (more flexible) lode form
    axis_ind <- get_axes(names(data))
    if (length(axis_ind) > 0) {
      stopifnot(is_alluvial_alluvia(data, axes = axis_ind))
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
      stopifnot(is_alluvial_lodes(
        data,
        key = "x", value = "stratum", id = "alluvium"
      ))
    }
    
    # incorporate any missing values into factor levels
    if (params$na.rm) {
      data <- na.omit(data)
    } else {
      if (is.factor(data$stratum)) {
        data$stratum <- addNA(data$stratum, ifany = TRUE)
      } else {
        data$stratum[is.na(data$stratum)] <- "NA"
      }
    }
    
    data
  },
  
  compute_panel = function(data, scales) {
    
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
    # y bounds
    transform(data,
              ymin = y - weight / 2,
              ymax = y + weight / 2)
  }
)
