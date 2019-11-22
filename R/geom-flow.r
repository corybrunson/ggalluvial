#' Flows between lodes or strata
#'
#' `geom_flow` receives a dataset of the horizontal (`x`) and
#' vertical (`y`, `ymin`, `ymax`) positions of the **lodes**
#' of an alluvial plot, the intersections of the alluvia with the strata.
#' It reconfigures these into alluvial segments connecting pairs of
#' corresponding lodes in adjacent strata and plots filled x-splines between
#' each such pair, using a provided knot position parameter `knot.pos`, and
#' filled rectangles at either end, using a provided `width`.
#' @template geom-aesthetics
#' @template defunct-geom-params
#'

#' @import ggplot2
#' @family alluvial geom layers
#' @seealso [ggplot2::layer()] for additional arguments and
#'   [stat_alluvium()] and
#'   [stat_flow()] for the corresponding stats.
#' @inheritParams geom_alluvium
#' @param aes.flow Character; how inter-lode flows assume aesthetics from lodes.
#'   Options are "forward" and "backward".
#' @example inst/examples/ex-geom-flow.r
#' @export
geom_flow <- function(mapping = NULL,
                      data = NULL,
                      stat = "flow",
                      position = "identity",
                      width = 1/3,
                      knot.pos = 1/6,
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
      width = width,
      knot.pos = knot.pos,
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
  
  setup_data = function(data, params) {
    
    width <- params$width
    if (is.null(width)) {
      width <- 1/3
    }
    
    knot.pos <- params$knot.pos
    if (is.null(knot.pos)) knot.pos <- 1/6
    
    # positioning parameters
    transform(data,
              xmin = x - width / 2,
              xmax = x + width / 2,
              knot.pos = knot.pos)
  },
  
  draw_panel = function(self, data, panel_params, coord,
                        width = 1/3, aes.flow = "forward", knot.pos = 1/6) {
    
    # exclude one-sided flows
    data <- data[complete.cases(data), ]
    
    # adjoin data with itself by alluvia along adjacent axes
    flow_pos <- intersect(names(data), c("x", "xmin", "xmax", "width",
                                         "y", "ymin", "ymax", "knot.pos"))
    flow_aes <- intersect(names(data), c("size", "linetype",
                                         "colour", "fill", "alpha"))
    flow_fore <- if (aes.flow != "backward") flow_aes else NULL
    flow_back <- if (aes.flow != "forward") flow_aes else NULL
    data <- self_adjoin(
      data = data, key = "x", by = "alluvium",
      link = flow_pos,
      keep.x = flow_fore, keep.y = flow_back,
      suffix = c(".0", ".1")
    )
    
    # aesthetics (in prescribed order)
    aesthetics <- intersect(.color_diff_aesthetics, names(data))
    # arrange data by aesthetics for consistent (reverse) z-ordering
    data <- data[do.call(order, lapply(
      data[, c("step", aesthetics)],
      function(x) factor(x, levels = unique(x))
    )), ]
    
    # construct spline grobs
    xspls <- plyr::alply(data, 1, function(row) {
      
      # spline paths and aesthetics
      xspl <- knots_to_xspl(row$xmax.0, row$xmin.1,
                            row$ymin.0, row$ymax.0, row$ymin.1, row$ymax.1,
                            row$knot.pos.0, row$knot.pos.1)
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
