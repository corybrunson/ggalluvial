#' Alluvial flows for time series
#' 
#' \code{stat_alluvium_ts} calculates the depth of each category at each time. 
#' \code{geom_alluvium_ts} plots an x-spline for each category across these 
#' times.
#' 
#' @section Aesthetics:
#' \code{stat_alluvium_ts} understands the following aesthetics (required
#' aesthetics are in bold):
#' \itemize{
#'   \item \strong{\code{x}}
#'   \item \strong{\code{group}}
#'   \item \strong{\code{weight}}
#' }
#' \code{geom_alluvium_ts} understands the following aesthetics (required
#' aesthetics are in bold):
#' \itemize{
#'   \item \code{alpha}
#'   \item \code{colour}
#'   \item \code{fill}
#'   \item \code{group}
#'   \item \code{linetype}
#'   \item \code{size}
#' }
#' 
#' @name alluvium_ts
#' @import ggplot2
#' @seealso \code{\link{ggalluvial}} for a shortcut method.
#' @inheritParams layer
#' @param decreasing Logical; whether to stack the depths at each x-value with
#'   the largest on top (FALSE, default), with the largest on bottom (TRUE), or
#'   in the order of the grouping variable values (NA).
#' @param knot.pos The horizontal distance between a measurement time and the
#'   control point of the x-spline, as a proportion of the separation between
#'   times. (Must be between 0 and 0.6.)
#' @example inst/examples/alluvium-ts.r
#' @usage NULL
#' @export
StatAlluviumTs <- ggproto(
  "StatAlluviumTs", Stat,
  required_aes = c("x", "group", "weight"),
  setup_data = function(data, params) {
    aggregate(
      formula = as.formula(paste("weight ~",
                                 paste(setdiff(names(data), "weight"),
                                       collapse = "+"))),
      data = data,
      FUN = sum
    )
  },
  compute_panel = function(data, scales, params,
                           decreasing = FALSE) {
    # fill in missing values (as zeros)
    data <- merge(
      data,
      with(data, expand.grid(group = unique(group), x = unique(x))),
      all.y = TRUE
    )
    # sort data by x and weight or group
    slice_sort <- if (is.na(decreasing)) {
      data$group
    } else if (!decreasing) {
      data$weight
    } else if (decreasing) {
      -data$weight
    } else {
      1:nrow(data)
    }
    data <- data[order(data$x, slice_sort), ]
    # cumulative weights
    data <- as.data.frame(dplyr::mutate(
      dplyr::group_by(data, x),
      ymax = cumsum(weight)
    ))
    data
  }
)

#' @rdname alluvium_ts
#' @usage NULL
#' @export
stat_alluvium_ts <- function(mapping = NULL,
                             data = NULL,
                             geom = "alluvium_ts",
                             position = "identity",
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE,
                             ...) {
  layer(
    stat = StatAlluviumTs,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname alluvium_ts
#' @usage NULL
#' @export
GeomAlluviumTs <- ggproto(
  "GeomAlluviumTs", Geom,
  default_aes = aes(size = .5, linetype = 1,
                    colour = 0, fill = "gray", alpha = .5),
  setup_data = function(data, params) data,
  draw_group = function(data, panel_scales, coord,
                        knot.pos = 1/6) {
    
    first_row <- data[1,
                      setdiff(names(data), c("x", "ymax", "weight")),
                      drop = FALSE]
    rownames(first_row) <- NULL
    
    # spline coordinates
    x_forward <- rep(data$x, c(3, rep(4, nrow(data) - 2), 3)) +
      knot.pos * c(0, rep(c(0, 1, -1, 0), nrow(data) - 1), 0)
    y_forward <- rep(data$ymax - data$weight, c(3, rep(4, nrow(data) - 2), 3))
    y_backward <- rev(rep(data$ymax, c(3, rep(4, nrow(data) - 2), 3)))
    shape_forward <- c(0, rep(c(0, 1, 1, 0), times = nrow(data) - 1), 0)
    spline_data <- data.frame(
      x = c(x_forward, rev(x_forward)),
      y = c(y_forward, y_backward),
      shape = rep(shape_forward, 2)
    )
    # recombine with first row elements
    data <- data.frame(first_row, spline_data)
    # transform (after calculating spline paths)
    coords <- coord$transform(data, panel_scales)
    # graphics object
    grid::xsplineGrob(
      x = coords$x, y = coords$y, shape = coords$shape,
      open = FALSE,
      gp = grid::gpar(
        col = coords$colour, fill = coords$fill, alpha = coords$alpha,
        lty = coords$linetype, lwd = coords$size * .pt
      )
    )
  },
  draw_key = draw_key_polygon
)

#' @rdname alluvium_ts
#' @usage NULL
#' @export
geom_alluvium_ts <- function(mapping = NULL,
                             data = NULL,
                             stat = "alluvium_ts",
                             knot.pos = 1/6,
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE,
                             ...) {
  layer(
    geom = GeomAlluviumTs,
    mapping = mapping,
    data = data,
    stat = stat,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      knot.pos = knot.pos,
      na.rm = na.rm,
      ...
    )
  )
}
