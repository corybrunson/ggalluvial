#' Variable axes and strata
#' 
#' \code{geom_stratum} stacks a box for each level of a variable at its axis.
#' 
#' @section Aesthetics:
#' \code{geom_stratum} understands the following aesthetics (required
#' aesthetics are in bold):
#' \itemize{
#'   \item \strong{\code{xmin}}
#'   \item \strong{\code{xmax}}
#'   \item \strong{\code{ymin}}
#'   \item \strong{\code{ymax}}
#'   \item \code{alpha}
#'   \item \code{colour}
#'   \item \code{fill}
#'   \item \code{group}
#'   \item \code{linetype}
#'   \item \code{size}
#' }
#' Currently, \code{group} is ignored.
#' 
#' @name geom_stratum
#' @import ggplot2
#' @seealso \code{\link{stat_stratum}} for transforming data into stratal
#'   parameters, \code{\link{stat_alluvium}} \code{\link{geom_alluvium}} for
#'   inter-axis flows and \code{\link{ggalluvial}} for a shortcut method.
#' @inheritParams layer
#' @example inst/examples/stratum.r
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

#' @rdname geom_stratum
#' @usage NULL
#' @export
geom_stratum <- function(mapping = NULL, data = NULL, stat = "stratum",
                         na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                         ...) {
  layer(
    geom = GeomStratum, mapping = mapping, data = data, stat = stat,
    position = "identity", show.legend = show.legend,
    inherit.aes = inherit.aes, params = list(na.rm = na.rm, ...)
  )
}
