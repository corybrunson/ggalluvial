#' Lodes at intersections of alluvia and strata
#' 
#' \code{geom_alluvium} receives a dataset of the horizontal (\code{x}) and 
#' vertical (\code{y}, \code{ymin}, \code{ymax}) positions of the \strong{lodes}
#' of an alluvial diagram, the intersections of the alluvia with the strata.
#' It plots rectangles for these lodes of a provided \code{width}.
#' @template geom-aesthetics
#' 

#' @import ggplot2
#' @family alluvial geom layers
#' @seealso \code{\link[ggplot2]{layer}} for additional arguments and
#'   \code{\link{stat_alluvium}} and
#'   \code{\link{stat_stratum}} for the corresponding stats.
#' @inheritParams ggplot2::layer
#' @template layer-params
#' @param stat The statistical transformation to use on the data;
#'    override the default.
#' @param width Numeric; the width of each stratum, as a proportion of the
#'   distance between axes. Defaults to 1/3.
#' @param axis_width Deprecated; alias for \code{width}.
#' @example inst/examples/ex-geom-lode.r
#' @export
geom_lode <- function(mapping = NULL,
                      data = NULL,
                      stat = "alluvium",
                      position = "identity",
                      width = 1/3, axis_width = NULL,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE,
                      ...) {
  layer(
    geom = GeomLode,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      width = width, axis_width = axis_width,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggalluvial-ggproto
#' @usage NULL
#' @export
GeomLode <- ggproto(
  "GeomLode", Geom,
  
  required_aes = c("x", "y", "ymin", "ymax"),
  
  default_aes = aes(size = .5, linetype = 1,
                    colour = 0, fill = "gray", alpha = .5),
  
  setup_params = function(data, params) {
    
    if (!is.null(params$axis_width)) {
      warning("Parameter 'axis_width' is deprecated; use 'width' instead.")
      params$width <- params$axis_width
      params$axis_width <- NULL
    }
    
    params
  },
  
  setup_data = function(data, params) {
    
    transform(data,
              xmin = x - params$width / 2,
              xmax = x + params$width / 2)
  },
  
  draw_panel = function(data, panel_params, coord,
                        width = 1/3, axis_width = NULL) {
    # taken from GeomRect
    
    lode_aes <- setdiff(
      names(data), c("x", "y", "xmin", "xmax", "ymin", "ymax")
    )
    
    # construct polygon grobs
    polys <- plyr::alply(data, 1, function(row) {
      
      poly <- rect_to_poly(row$xmin, row$xmax, row$ymin, row$ymax)
      aes <- as.data.frame(row[lode_aes],
                           stringsAsFactors = FALSE)[rep(1, 5), ]
      
      GeomPolygon$draw_panel(cbind(poly, aes, group = 1), panel_params, coord)
    })
    
    # combine polygon grobs
    grob <- do.call(grid::grobTree, polys)
    grob$name <- grid::grobName(grob, "bar")
    grob
  },
  
  draw_key = draw_key_polygon
)
