#' Flows between lodes or strata
#' 
#' \code{geom_flow} receives a dataset of the horizontal (\code{x}) and 
#' vertical (\code{y}, \code{ymin}, \code{ymax}) positions of the \strong{lodes}
#' of an alluvial diagram, the intersections of the alluvia with the strata.
#' It reconfigures these into alluvial segments connecting pairs of
#' corresponding lodes in adjacent strata and plots filled x-splines between
#' each such pair, using a provided knot position parameter \code{knot.pos}, and
#' filled rectangles at either end, using a provided \code{width}.
#' @template geom-aesthetics
#' 

#' @import ggplot2
#' @family alluvial geom layers
#' @seealso \code{\link[ggplot2]{layer}} for additional arguments and
#'   \code{\link{stat_alluvium}} and
#'   \code{\link{stat_flow}} for the corresponding stats.
#' @inheritParams geom_alluvium
#' @param aes.flow Character; how inter-lode flows assume aesthetics from lodes.
#'   Options are "forward" and "backward".
#' @example inst/examples/ex-geom-flow.r
#' @export
geom_flow <- function(mapping = NULL,
                      data = NULL,
                      stat = "flow",
                      position = "identity",
                      width = 1/3, axis_width = NULL,
                      knot.pos = 1/6, ribbon_bend = NULL,
                      aes.flow = "forward",
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE,
                      ...) {
  
  aes.flow <- match.arg(aes.flow, c("forward", "backward"))
  
  layer(
    geom = GeomFlow,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
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

#' @rdname ggalluvial-ggproto
#' @usage NULL
#' @export
GeomFlow <- ggproto(
  "GeomFlow", Geom,
  
  required_aes = c("x", "y", "ymin", "ymax"),
  
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
    
    # exclude one-sided flows
    data <- data[complete.cases(data), ]
    
    # adjoin data with itself by alluvia along adjacent axes
    flow_pos <- intersect(names(data), c("x", "xmin", "xmax", "width",
                                         "y", "ymin", "ymax", "weight",
                                         "knot.pos"))
    flow_aes <- intersect(names(data), c("size", "linetype",
                                         "colour", "fill", "alpha"))
    flow_fore <- if (aes.flow != "backward") flow_aes else NULL
    flow_back <- if (aes.flow != "forward") flow_aes else NULL
    data <- self_adjoin(data, "x", "alluvium", pair = flow_pos,
                        keep0 = flow_fore, keep1 = flow_back)
    
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
      f_coords <- coord$transform(f_data, panel_params)
      
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
