#' Alluvial flows
#' 
#' \code{stat_alluvium} calculates the depth of each group at each axis. 
#' \code{geom_alluvium} plots an x-spline for each group through the axes at 
#' these depths.
#' 
#' @section Aesthetics:
#' \code{stat_alluvium} understands only the \code{group} 
#'   aesthetic, but it is currently ignored.
#' \code{geom_alluvium} understands the following aesthetics
#'   (required aesthetics are in bold):
#' \itemize{
#'   \item \strong{\code{axis[0-9\\.]}} (\code{axis1}, \code{axis2.5}, etc.)
#'   \item \code{alpha}
#'   \item \code{colour}
#'   \item \code{fill}
#'   \item \code{group}
#'   \item \code{linetype}
#'   \item \code{size}
#' }
#' Currently, \code{group} is ignored.
#' 
#' @name alluvium
#' @import ggplot2
#' @seealso \code{\link{stratum}} for intra-axis boxes, 
#'   \code{\link{alluvium_ts}} for a time series implementation, and 
#'   \code{\link{ggalluvial}} for a shortcut method.
#' @inheritParams layer
#' @param lode.guidance The function to prioritize the axis variables for 
#'   ordering the lodes within each stratum. Defaults to "zigzag", other options
#'   include "rightleft", "leftright", "rightward", and "leftward" (see 
#'   \code{\link{lode-guidance-functions}}).
#' @param bind.by.aes Whether to prioritize aesthetics before axes (other than 
#'   the index axis) when ordering the lodes within each stratum. Defaults to 
#'   FALSE.
#' @param lode.ordering A list (of length the number of axes) of integer vectors
#'   (each of length the number of rows of \code{data}) or NULL entries 
#'   (indicating no imposed ordering), or else a numeric matrix of corresponding
#'   dimensions, giving the preferred ordering of alluvia at each axis. This 
#'   will be used to order the lodes within each stratum by sorting the lodes 
#'   first by stratum and then by the provided vectors.
#' @param ribbon_bend The horizontal distance between a variable axis 
#'   (\code{width/2} from its center) and the control point of the x-spline,
#'   also as a proportion of the separation between the axes.
#' @example inst/examples/alluvium.r
#' @usage NULL
#' @export
StatAlluvium <- ggproto(
  "StatAlluvium", Stat,
  setup_data = function(data, params) {
    
    # assign uniform weight if not provided
    if (is.null(data$weight)) {
      data$weight <- rep(1, nrow(data))
      message("No argument provided for 'weight'; assuming uniform row weights")
    }
    
    # override existing group assignment; assign each row its own group
    data$group <- 1:nrow(data)
    data
  },
  setup_params = function(data, params) {
    
    if (!is.null(data$x) || !is.null(params$x) ||
        !is.null(data$y) || !is.null(params$y)) {
      stop("stat_alluvium() does not accept x or y aesthetics")
    }
    
    if (!is.null(params$lode.ordering)) {
      if (is.list(params$lode.ordering)) {
        # replace any null entries with uniform NA vectors
        wh.null <- which(sapply(params$lode.ordering, is.null))
        for (w in wh.null) params$lode.ordering[[w]] <- rep(NA, nrow(data))
        # convert list to array (requires equal-length numeric entries)
        params$lode.ordering <- do.call(cbind, params$lode.ordering)
      }
      # check that array has correct dimensions
      stopifnot(dim(params$lode.ordering) ==
                  c(nrow(data), length(get_axes(names(data)))))
    }
    
    params
  },
  compute_panel = function(data, scales, params,
                           lode.guidance = "zigzag",
                           bind.by.aes = FALSE,
                           lode.ordering = NULL,
                           width = 1/3) {
    
    axis_ind <- get_axes(names(data))
    data_aes <- setdiff(names(data)[-axis_ind],
                        c("weight", "PANEL", "group"))
    aes_ind <- match(data_aes, names(data))
    
    if (is.null(lode.ordering)) lode_fn <- get(paste0("lode_", lode.guidance))

    # x and y coordinates of center of flow at each axis
    compute_alluvium <- function(i) {
      # depends on whether the user has provided a lode.ordering
      if (is.null(lode.ordering)) {
        # order axis indices
        axis_seq <- axis_ind[lode_fn(n = length(axis_ind), i = i)]
        # combine axis and aesthetic indices
        all_ind <- if (bind.by.aes) {
          c(axis_seq[1], aes_ind, axis_seq[-1])
        } else {
          c(axis_seq, aes_ind)
        }
        # order ribbons according to axes, in above order
        ribbon_seq <- do.call(order, data[all_ind])
      } else {
        ribbon_seq <- order(data[[axis_ind[i]]], lode.ordering[, i])
      }
      # ribbon floors and ceilings along axis
      ymin_seq <- c(0, cumsum(data$weight[ribbon_seq]))
      ymax_seq <- c(cumsum(data$weight[ribbon_seq]), sum(data$weight))
      # ribbon breaks
      cbind(i,
            ymin_seq[order(ribbon_seq)],
            ymax_seq[order(ribbon_seq)])
    }
    
    alluvia <- do.call(rbind, lapply(1:length(axis_ind), compute_alluvium))
    colnames(alluvia) <- c("x", "ymin", "ymax")
    data <- data.frame(data, alluvia)
    
    # widths and x bounds
    data$xmin <- data$x - width / 2
    data$xmax <- data$x + width / 2
    data$width <- width
    
    # y centers
    data$y <- (data$ymin + data$ymax) / 2
    
    data
  }
)

#' @rdname alluvium
#' @usage NULL
#' @export
stat_alluvium <- function(mapping = NULL,
                          data = NULL,
                          geom = "alluvium",
                          position = "identity",
                          width = 1/3,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          ...) {
  layer(
    stat = StatAlluvium,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      width = width,
      ...
    )
  )
}

#' @rdname alluvium
#' @usage NULL
#' @export
GeomAlluvium <- ggproto(
  "GeomAlluvium", Geom,
  default_aes = aes(size = .5, linetype = 1, colour = 0,
                    fill = "gray", alpha = .5),
  setup_data = function(data, params) data,
  draw_group = function(data, panel_scales, coord,
                        ribbon_bend = 1/6) {
    
    first_row <- data[1, setdiff(names(data), c("x", "xmin", "xmax",
                                                "y", "ymin", "ymax",
                                                "width")),
                      drop = FALSE]
    rownames(first_row) <- NULL
    if (nrow(data) == 1) {
      # spline coordinates (one axis)
      spline_data <- data.frame(
        x = data$x + data$width / 2 * c(-1, 1, 1, -1),
        y = data$ymin + first_row$weight * c(0, 0, 1, 1),
        shape = rep(0, 4)
      )
    } else {
      # spline coordinates (more than one axis)
      x_oneway <- rep(data$x, c(3, rep(4, nrow(data) - 2), 3)) +
        rep(data$width, c(3, rep(4, nrow(data) - 2), 3)) / 2 *
        c(-1, rep(c(1, 1, -1, -1), nrow(data) - 1), 1) +
        ribbon_bend * c(0, rep(c(0, 1, -1, 0), nrow(data) - 1), 0)
      y_oneway <- rep(data$ymin, c(3, rep(4, nrow(data) - 2), 3))
      shape_oneway <- c(0, rep(c(0, 1, 1, 0), nrow(data) - 1), 0)
      spline_data <- data.frame(
        x = c(x_oneway, rev(x_oneway)),
        y = c(y_oneway, rev(y_oneway) + first_row$weight),
        shape = rep(shape_oneway, 2)
      )
    }
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

#' @rdname alluvium
#' @usage NULL
#' @export
geom_alluvium <- function(mapping = NULL,
                          data = NULL,
                          stat = "alluvium",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          ...) {
  layer(
    geom = GeomAlluvium,
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
