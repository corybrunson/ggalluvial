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
#' @example inst/examples/ex-geom-stratum.r
#' @usage NULL
#' @export
geom_stratum <- function(mapping = NULL,
                         data = NULL,
                         stat = "stratum",
                         show.legend = NA,
                         inherit.aes = TRUE,
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
  
  setup_data = function(data, params) {
    
    transform(data,
              xmin = x - width / 2, xmax = x + width / 2,
              ymin = y - weight / 2, ymax = y + weight / 2)
  },
  
  draw_key = draw_key_polygon
)
