#' Variable axes and strata
#' 
#' \code{stat_stratum} calculates the centers of the levels at each axis. 
#' \code{geom_stratum} stacks a box for each level of a variable at its axis.
#' 
#' @section Aesthetics:
#' \code{stat_stratum} understands only the \code{group} aesthetic, but it is
#' currently ignored.
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
#' @name stratum
#' @import ggplot2
#' @seealso \code{\link{alluvium}} for inter-axis flows and
#'   \code{\link{ggalluvial}} for a shortcut method.
#' @inheritParams layer
#' @example inst/examples/stratum.r
#' @usage NULL
#' @export
StatStratum <- ggproto(
  "StatStratum", Stat,
  setup_data = function(data, params) {
    
    # assign uniform weight if not provided
    if (is.null(data$weight)) {
      data$weight <- rep(1, nrow(data))
      message("No argument provided for 'weight'; assuming uniform row weights")
    }
    
    # aggregate over axes by weight
    data <- aggregate(
      formula = as.formula(paste("weight ~",
                                 paste(setdiff(names(data), "weight"),
                                       collapse = "+"))),
      data = data, FUN = sum
    )
    
    axis_ind <- get_axes(names(data))
    # stack data with cumulative frequencies by panel
    res_data <- do.call(rbind, lapply(unique(data$PANEL), function(p) {
      p_data <- subset(data, PANEL == p)
      do.call(rbind, lapply(1:length(axis_ind), function(i) {
        agg <- aggregate(x = p_data$weight, by = p_data[axis_ind[i]],
                         FUN = sum)
        names(agg) <- c("label", "weight")
        cbind(pos = i, agg, cumweight = cumsum(agg$weight), PANEL = p)
      }))
    }))
    
    # remove empty elements
    res_data <- res_data[res_data$weight > 0, ]
    
    # assign each row its own group (no pre-existing group field)
    cbind(res_data, group = 1:nrow(res_data))
  },
  setup_params = function(data, params) {
    
    if (!is.null(data$x) || !is.null(params$x) ||
        !is.null(data$y) || !is.null(params$y)) {
      stop("stat_stratum() does not accept x or y aesthetics")
    }
    
    params
  },
  compute_group = function(data, scales,
                           width = 1/3) {
    
    rownames(data) <- NULL
    rect_data <- data.frame(x = data$pos,
                            y = (data$cumweight - data$weight / 2),
                            width = width)
    data.frame(data, rect_data)
  }
)

#' @rdname stratum
#' @usage NULL
#' @export
stat_stratum <- function(mapping = NULL, data = NULL, geom = "stratum",
                         na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                         width = 1/3, ...) {
  layer(
    stat = StatStratum, data = data, mapping = mapping, geom = geom,
    position = "identity", show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, width = width, ...)
  )
}

#' @rdname stratum
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

#' @rdname stratum
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
