#' Alluvial flows
#' 
#' \code{stat_alluvium} calculates the depth of each group at each axis. 
#' \code{geom_alluvium} plots an x-spline for each group through the axes at 
#' these depths.
#' 
#' @section Aesthetics:
#' \code{geom_alluvium} understands the following aesthetics
#'   (required aesthetics are in bold):
#' \itemize{
#'   \item \strong{\code{axis[0-9]*}} (\code{axis1}, \code{axis2}, etc.)
#'   \item \code{alpha}
#'   \item \code{colour}
#'   \item \code{fill}
#'   \item \code{group}
#'   \item \code{linetype}
#'   \item \code{size}
#' }
#' Currently, \code{group} is ignored.
#' 
#' @name geom-alluvium
#' @import ggplot2
#' @seealso \code{\link{stat_stratum}} and \code{\link{geom_stratum}} for
#'   intra-axis boxes, 
#'   \code{\link{alluvium_ts}} for a time series implementation, and 
#'   \code{\link{ggalluvial}} for a shortcut method.
#' @inheritParams layer
#' @param width The width of each variable axis, as a proportion of the
#'   separation between axes. Defaults to 1/3.
#' @param axis_width Deprecated; alias for \code{width}.
#' @param knot.pos The horizontal distance between a variable axis 
#'   (\code{width/2} from its center) and the control point of the x-spline, as 
#'   a proportion of the separation between the strata. (Must be between 0 and 
#'   0.5.). Defaults to 1/6.
#' @param ribbon_bend Deprecated; alias for \code{knot.pos}.
#' @example inst/examples/ex-alluvium.r
#' @usage NULL
#' @export
geom_alluvium <- function(mapping = NULL,
                          data = NULL,
                          stat = "alluvium",
                          width = 1/3, axis_width = NULL,
                          knot.pos = 1/6, ribbon_bend = NULL,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          ...) {
  layer(
    geom = GeomAlluvium,
    mapping = mapping,
    data = data,
    stat = stat,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      knot.pos = knot.pos, ribbon_bend = ribbon_bend,
      width = width, axis_width = axis_width,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom-alluvium
#' @usage NULL
#' @export
GeomAlluvium <- ggproto(
  "GeomAlluvium", Geom,
  
  default_aes = aes(size = .5, linetype = 1, colour = 0,
                    fill = "gray", alpha = .5),
  
  setup_params = function(data, params) {
    
    if (!is.null(params$axis_width)) {
      warning("Parameter 'axis_width' is deprecated; use 'width' instead.")
      params$width <- params$axis_width
      params$axis_width <- NULL
    }
    if (!is.null(params$width)) {
      if (params$width < 0 | params$width > 1) {
        warning("Argument to parameter 'width' is not between 0 and 1, ",
                "and will be ignored.")
        params$width <- 1/3
      }
    }
    
    if (!is.null(params$ribbon_bend)) {
      warning("Parameter 'ribbon_bend' is deprecated; use 'knot.pos' instead.")
      params$knot.pos <- params$ribbon_bend
      params$ribbon_bend <- NULL
    }
    if (params$knot.pos < 0 | params$knot.pos > .5) {
      warning("Argument to parameter 'knot.pos' is not between 0 and .5, ",
              "and will be ignored.")
      params$knot.pos <- 1/6
    }
    
    params
  },
  
  setup_data = function(data, params) {
    
    # positioning parameters
    transform(data,
              width = params$width,
              xmin = x - params$width / 2,
              xmax = x + params$width / 2,
              knot.pos = params$knot.pos)
  },
  
  draw_group = function(data, panel_scales, coord,
                        width = 1/3, axis_width = NULL,
                        knot.pos = 1/6, ribbon_bend = NULL) {
    
    first_row <- data[1, setdiff(names(data),
                                 c("x", "xmin", "xmax",
                                   "y", "ymin", "ymax",
                                   "width", "knot.pos")),
                      drop = FALSE]
    rownames(first_row) <- NULL
    
    if (nrow(data) == 1) {
      # spline coordinates (one axis)
      spline_data <- data.frame(
        x = data$x + data$width / 2 * c(-1, 1, 1, -1),
        y = data$ymin + first_row$weight * c(0, 0, 1, 1),
        shape = rep(0, 4)
      )
    } else {
      # spline coordinates (more than one axis)
      w_oneway <- rep(data$width, c(3, rep(4, nrow(data) - 2), 3))
      k_oneway <- rep(data$knot.pos, c(3, rep(4, nrow(data) - 2), 3))
      x_oneway <- rep(data$x, c(3, rep(4, nrow(data) - 2), 3)) +
        w_oneway / 2 * c(-1, rep(c(1, 1, -1, -1), nrow(data) - 1), 1) +
        k_oneway * (1 - w_oneway) * c(0, rep(c(0, 1, -1, 0), nrow(data) - 1), 0)
      y_oneway <- rep(data$ymin, c(3, rep(4, nrow(data) - 2), 3))
      shape_oneway <- c(0, rep(c(0, 1, 1, 0), nrow(data) - 1), 0)
      spline_data <- data.frame(
        x = c(x_oneway, rev(x_oneway)),
        y = c(y_oneway, rev(y_oneway) + first_row$weight),
        shape = rep(shape_oneway, 2)
      )
    }
    data <- data.frame(first_row, spline_data)
    
    # transform (after calculating spline paths)
    coords <- coord$transform(data, panel_scales)
    
    # graphics object
    grid::xsplineGrob(
      x = coords$x, y = coords$y, shape = coords$shape,
      open = FALSE,
      gp = grid::gpar(
        col = coords$colour, fill = coords$fill, alpha = coords$alpha,
        lty = coords$linetype, lwd = coords$size * .pt
      )
    )
  },
  
  draw_key = draw_key_polygon
)
