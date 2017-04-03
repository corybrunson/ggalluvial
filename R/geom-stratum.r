#' Variable axes with strata of values
#' 
#' Given a dataset with alluvial structure, \code{geom_stratum} calculates the
#' centroids of the strata for each axis, together with their weights (heights)
#' and widths.
#' 
#' @section Aesthetics:
#' \code{geom_stratum} understands the following aesthetics (required aesthetics
#' are in bold):
#' \itemize{
#'   \item \code{x}
#'   \item \code{y}
#'   \item \code{group}
#'   \item \code{axis[0-9]*} (\code{axis1}, \code{axis2}, etc.)
#'   \item \code{weight}
#' }
#' Use \code{x}, \code{y}, and \code{group} for data in lode form and 
#' \code{axis[0-9]*} for data in alluvium form (see \code{\link{is_alluvial}});
#' arguments to parameters inconsistent with the data form will be ignored.
#' @name geom-stratum
#' @import ggplot2
#' @seealso \code{\link{stat-stratum}} for the corresponding geom.
#' @inheritParams layer
#' @param width The width of each stratum, as a proportion of the separation
#'   between their centers. Defaults to 1/3.
#' @param axis_width Deprecated; alias for \code{width}.
#' @example inst/examples/ex-stratum.r
#' @usage NULL
#' @export
geom_stratum <- function(mapping = NULL,
                         data = NULL,
                         stat = "stratum",
                         show.legend = NA,
                         inherit.aes = TRUE,
                         width = 1/3, axis_width = NULL,
                         na.rm = FALSE,
                         ...) {
  layer(
    geom = GeomStratum,
    mapping = mapping,
    data = data,
    stat = stat,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      width = width, axis_width = axis_width,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom-stratum
#' @usage NULL
#' @export
GeomStratum <- ggproto(
  "GeomStratum", GeomRect,
  
  default_aes = aes(size = .5, linetype = 1,
                    colour = "black", fill = "white", alpha = 1),
  
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
    
    aesthetics <- setdiff(
      names(data), c("x", "y", "xmin", "xmax", "ymin", "ymax")
    )
    
    polys <- plyr::alply(data, 1, function(row) {
      
      poly <- rect_to_poly(row$xmin, row$xmax, row$ymin, row$ymax)
      aes <- as.data.frame(row[aesthetics],
                           stringsAsFactors = FALSE)[rep(1,5), ]
      
      GeomPolygon$draw_panel(cbind(poly, aes, group = 1), panel_params, coord)
    })
    
    grob <- do.call(grid::grobTree, polys)
    grob$name <- grid::grobName(grob, "bar")
    grob
  },
  
  draw_key = draw_key_polygon
)

# Convert rectangle to polygon
# (lifted from \code{\link[ggplot2]{geom_rect}})
# @keyword internal
rect_to_poly <- function(xmin, xmax, ymin, ymax) {
  data.frame(
    y = c(ymax, ymax, ymin, ymin, ymax),
    x = c(xmin, xmax, xmax, xmin, xmin)
  )
}
