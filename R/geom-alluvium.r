#' Alluvial flows
#' 
#' \code{geom_alluvium} plots an x-spline for each group through the axes at 
#' the depths provided by \code{\link{stat_alluvium}}.
#' 
#' @section Aesthetics:
#' \code{geom_alluvium} understands the following aesthetics (required
#' aesthetics are in bold):
#' \itemize{
#'   \item \strong{\code{axis[0-9\\.]}} (\code{axis1}, \code{axis2.5}, etc.)
#'   \item \code{alpha}
#'   \item \code{colour}
#'   \item \code{fill}
#'   \item \code{group}
#'   \item \code{linetype}
#'   \item \code{size}
#' }
#' Currently, \code{group} is ignored.
#' 
#' @name geom_alluvium
#' @import ggplot2
#' @seealso \code{\link{stat_alluvium}} for transforming data into alluvial
#'   parameters, \code{\link{stat_stratum}} and \code{\link{geom_stratum}} for
#'   intra-axis boxes, and \code{\link{ggalluvial}} for a shortcut method.
#' @inheritParams layer
#' @param ribbon_bend The horizontal distance between a variable axis 
#'   (\code{axis_width/2} from its center) and the control point of the 
#'   x-spline, also as a proportion of the separation between the axes.
#' @example inst/examples/alluvium.r
#' @usage NULL
#' @export
GeomAlluvium <- ggproto(
  "GeomAlluvium", Geom,
  default_aes = aes(size = .5, linetype = 1,
                    colour = 0, fill = "gray", alpha = .5),
  setup_data = function(data, params) data,
  draw_group = function(data, panel_scales, coord,
                        ribbon_bend = 1/6) {
    
    first_row <- data[1, setdiff(names(data), c("x", "xmin", "xmax",
                                                "y", "ymin", "ymax",
                                                "width")),
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
      x_oneway <- rep(data$x, c(3, rep(4, nrow(data) - 2), 3)) +
        rep(data$width, c(3, rep(4, nrow(data) - 2), 3)) / 2 *
        c(-1, rep(c(1, 1, -1, -1), nrow(data) - 1), 1) +
        ribbon_bend * c(0, rep(c(0, 1, -1, 0), nrow(data) - 1), 0)
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

#' @rdname geom_alluvium
#' @usage NULL
#' @export
geom_alluvium <- function(mapping = NULL, data = NULL, stat = "alluvium",
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                          ...) {
  layer(
    geom = GeomAlluvium, mapping = mapping, data = data, stat = stat,
    position = "identity", show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
