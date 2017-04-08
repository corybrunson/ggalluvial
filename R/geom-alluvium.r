#' Alluvial flows
#' 
#' \code{geom_alluvium} receives a dataset of the horizontal (\code{x}) and 
#' vertical (\code{y}, \code{ymin}, \code{ymax}) positions of the \strong{lodes}
#' of an alluvial diagram, the intersections of the alluvia with the strata. It
#' reconfigures these into alluvial segments connecting pairs of corresponding
#' lodes in adjacent strata and plots filled x-splines between each such pair,
#' using a provided knot position parameter \code{knot.pos}, and filled
#' rectangles at either end, using a provided \code{width}.
#' 
#' @section Aesthetics:
#' \code{geom_alluvium} understands the following aesthetics
#'   (required aesthetics are in bold):
#' \itemize{
#'   \item \code{\strong{x}}
#'   \item \code{\strong{y}}
#'   \item \code{\strong{ymin}}
#'   \item \code{\strong{ymax}}
#'   \item \code{alpha}
#'   \item \code{colour}
#'   \item \code{fill}
#'   \item \code{linetype}
#'   \item \code{size}
#'   \item \code{group}
#' }
#' Currently, \code{group} is ignored.
#' 
#' @name geom-alluvium
#' @import ggplot2
#' @seealso \code{\link{stat_stratum}} and \code{\link{geom_stratum}} for 
#'   intra-axis boxes, \code{\link{alluvium_ts}} for a time series
#'   implementation, and \code{\link{ggalluvial}} for a shortcut method.
#' @inheritParams layer
#' @param aes.flow Character; how inter-lode flows assume aesthetics from lodes.
#'   Options are "forward" and "backward".
#' @param width Numeric; the width of each stratum, as a proportion of the
#'   distance between axes. Defaults to 1/3.
#' @param axis_width Deprecated; alias for \code{width}.
#' @param knot.pos The horizontal distance between a stratum (\code{width/2}
#'   from its axis) and the knot of the x-spline, as a proportion of the
#'   separation between strata. Defaults to 1/6.
#' @param ribbon_bend Deprecated; alias for \code{knot.pos}.
#' @example inst/examples/ex-alluvium.r
#' @usage NULL
#' @export
geom_alluvium <- function(mapping = NULL,
                          data = NULL,
                          stat = "alluvium",
                          aes.flow = "forward",
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
      aes.flow = aes.flow,
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
    
    if (!is.null(params$ribbon_bend)) {
      warning("Parameter 'ribbon_bend' is deprecated; use 'knot.pos' instead.")
      params$knot.pos <- params$ribbon_bend
      params$ribbon_bend <- NULL
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
  
  draw_panel = function(data, panel_scales, coord,
                        aes.flow = "forward",
                        width = 1/3, axis_width = NULL,
                        knot.pos = 1/6, ribbon_bend = NULL) {
    
    # pair lodes with neighbors
    flow_pos <- c("x", "xmin", "xmax", "width",
                  "y", "ymin", "ymax", "weight",
                  "knot.pos",
                  "flow", "alluvium")
    flow_aes <- setdiff(names(data), c(flow_pos, "stratum", "PANEL", "group"))
    flows <- dplyr::inner_join(transform(data, flow = x)[, flow_pos],
                               transform(data, flow = x - 1)[, flow_pos],
                               by = c("flow", "alluvium"),
                               suffix = c("0", "1"))
    
    # bring in aesthetics from appropriate side
    data <- dplyr::left_join(
      flows,
      if (aes.flow == "forward") {
        transform(data, flow = x)[, c(flow_aes, "flow", "alluvium")]
      } else {
        transform(data, flow = x - 1)[, c(flow_aes, "flow", "alluvium")]
      },
      by = c("flow", "alluvium")
    )
    
    # construct spline grobs
    xspls <- plyr::alply(data, 1, function(row) {
      
      # spline paths and aesthetics
      xspl <- knots_to_xspl(row$xmax0, row$xmin1,
                            row$ymin0, row$ymax0, row$ymin1, row$ymax1,
                            row$knot.pos0, row$knot.pos1)
      aes <- as.data.frame(row[flow_aes],
                           stringsAsFactors = FALSE)[rep(1, 8), ]
      f_data <- cbind(xspl, aes)
      
      # transform (after calculating spline paths)
      f_coords <- coord$transform(f_data, panel_scales)
      
      # single spline grob
      grid::xsplineGrob(
        x = f_coords$x, y = f_coords$y, shape = f_coords$shape,
        open = FALSE,
        gp = grid::gpar(
          col = f_coords$colour, fill = f_coords$fill, alpha = f_coords$alpha,
          lty = f_coords$linetype, lwd = f_coords$size * .pt
        )
      )
    })
    
    # combine spline grobs
    grob <- do.call(grid::grobTree, xspls)
    grob$name <- grid::grobName(grob, "xspline")
    grob
  },
  
  draw_key = draw_key_polygon
)

# x-spline coordinates from 2 x bounds, 4 y bounds, and knot position
knots_to_xspl <- function(x0, x1, ymin0, ymax0, ymin1, ymax1, kp0, kp1) {
  x_oneway <- c(x0, x0 + kp0, x1 - kp1, x1)
  data.frame(
    x = c(x_oneway, rev(x_oneway)),
    y = c(ymin0, ymin0, ymin1, ymin1, ymax1, ymax1, ymax0, ymax0),
    shape = rep(c(0, 1, 1, 0), times = 2)
  )
}
