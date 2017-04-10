#' Alluvia
#' 
#' \code{geom_alluvium} receives a dataset of the horizontal (\code{x}) and 
#' vertical (\code{y}, \code{ymin}, \code{ymax}) positions of the \strong{lodes}
#' of an alluvial diagram, the intersections of the alluvia with the strata.
#' It plots both the lodes themselves, using \code{\link{geom_lode}}, and the
#' flows between them, using \code{\link{geom_flow}}.
#' 
#' @section Aesthetics:
#' \code{geom_alluvium} understands the following aesthetics
#'   (required aesthetics are in bold):
#' \itemize{
#'   \item \strong{\code{x}}
#'   \item \strong{\code{y}}
#'   \item \strong{\code{ymin}}
#'   \item \strong{\code{ymax}}
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
#' @inheritParams geom_lode
#' @inheritParams geom_flow
#' @param ribbon_bend Deprecated; alias for \code{knot.pos}.
#' @example inst/examples/ex-alluvium.r
#' @usage NULL
#' @export
geom_alluvium <- function(mapping = NULL,
                      data = NULL,
                      stat = "alluvium",
                      width = 1/3, axis_width = NULL,
                      knot.pos = 1/6, ribbon_bend = NULL,
                      aes.flow = "forward",
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
      width = width, axis_width = axis_width,
      knot.pos = knot.pos, ribbon_bend = ribbon_bend,
      aes.flow = aes.flow,
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
  
  default_aes = aes(size = .5, linetype = 1,
                    colour = 0, fill = "gray", alpha = .5),
  
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
              xmin = x - params$width / 2,
              xmax = x + params$width / 2,
              knot.pos = params$knot.pos)
  },
  
  draw_panel = function(self, data, panel_params, coord,
                        width = 1/3, axis_width = NULL,
                        aes.flow = "forward",
                        knot.pos = 1/6, ribbon_bend = NULL) {
    
    # construct lode and flow grobs
    grob <- grid::grobTree(
      GeomLode$draw_panel(data, panel_params, coord),
      GeomFlow$draw_panel(data, panel_params, coord)
    )
    grob$name <- grid::grobName(grob, "xspline")
    grob
  },
  
  draw_key = draw_key_polygon
)
