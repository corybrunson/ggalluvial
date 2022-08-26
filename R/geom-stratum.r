#' Strata at axes
#'
#' `geom_stratum` receives a dataset of the horizontal (`x`) and vertical (`y`,
#' `ymin`, `ymax`) positions of the strata of an alluvial plot. It plots
#' rectangles for these strata of a provided `width`.
#' @template geom-aesthetics
#' @template defunct-geom-params
#'

#' @import ggplot2
#' @family alluvial geom layers
#' @seealso [ggplot2::layer()] for additional arguments and
#'   [stat_stratum()] for the corresponding stat.
#' @inheritParams geom_lode
#' @example inst/examples/ex-geom-stratum.r
#' @export
geom_stratum <- function(mapping = NULL,
                         data = NULL,
                         stat = "stratum",
                         position = "identity",
                         show.legend = NA,
                         inherit.aes = TRUE,
                         width = 1/3,
                         na.rm = FALSE,
                         ...) {
  layer(
    geom = GeomStratum,
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
GeomStratum <- ggproto(
  "GeomStratum", GeomRect,
  
  required_aes = c("x", "y", "ymin", "ymax"),
  
  default_aes = aes(size = .5, linewidth = .5, linetype = 1,
                    colour = "black", fill = "white", alpha = 1),
  
  setup_data = function(data, params) {
    
    width <- params$width
    if (is.null(width)) width <- 1/3
    
    transform(data,
              xmin = x - width / 2,
              xmax = x + width / 2)
  },
  
  draw_panel = function(self, data, panel_params, coord,
                        width = 1/3) {
    # taken from GeomRect
    
    strat_aes <- setdiff(
      names(data), c("x", "y", "xmin", "xmax", "ymin", "ymax")
    )
    
    # construct polygon grobs
    polys <- lapply(split(data, seq_len(nrow(data))), function(row) {
      
      poly <- rect_to_poly(row$xmin, row$xmax, row$ymin, row$ymax)
      aes <- as.data.frame(row[strat_aes],
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
