#' Lodes at intersections of alluvia and strata
#'
#' `geom_alluvium` receives a dataset of the horizontal (`x`) and vertical (`y`,
#' `ymin`, `ymax`) positions of the **lodes** of an alluvial plot, the
#' intersections of the alluvia with the strata. It plots rectangles for these
#' lodes of a provided `width`.
#' @template geom-aesthetics
#' @template defunct-geom-params
#'

#' @import ggplot2
#' @family alluvial geom layers
#' @seealso [ggplot2::layer()] for additional arguments and
#'   [stat_alluvium()] and
#'   [stat_stratum()] for the corresponding stats.
#' @inheritParams ggplot2::layer
#' @template layer-params
#' @param stat The statistical transformation to use on the data;
#'    override the default.
#' @param width Numeric; the width of each stratum, as a proportion of the
#'   distance between axes. Defaults to 1/3.
#' @example inst/examples/ex-geom-lode.r
#' @export
geom_lode <- function(mapping = NULL,
                      data = NULL,
                      stat = "alluvium",
                      position = "identity",
                      width = 1/3,
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
      width = width,
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
  
  default_aes = aes(size = .5, linewidth = .5, linetype = 1,
                    colour = "transparent", fill = "gray", alpha = .5),
  
  setup_data = function(data, params) {
    
    width <- params$width
    if (is.null(width)) width <- 1/3
    
    transform(data,
              xmin = x - width / 2,
              xmax = x + width / 2)
  },
  
  draw_panel = function(data, panel_params, coord,
                        width = 1/3) {
    # taken from GeomRect
    
    lode_aes <- setdiff(
      names(data), c("x", "y", "xmin", "xmax", "ymin", "ymax")
    )
    
    # construct polygon grobs
    polys <- lapply(split(data, seq_len(nrow(data))), function(row) {
      
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
