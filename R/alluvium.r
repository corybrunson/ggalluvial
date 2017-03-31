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
#'   \item \strong{\code{axis[0-9]*}} (\code{axis1}, \code{axis2}, etc.)
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
#' @param width The width of each variable axis, as a proportion of the
#'   separation between axes. Defaults to 1/3.
#' @param axis_width Deprecated; alias for \code{width}.
#' @param knot.pos The horizontal distance between a variable axis 
#'   (\code{width/2} from its center) and the control point of the x-spline, as 
#'   a proportion of the separation between the strata. (Must be between 0 and 
#'   0.5.). Defaults to 1/6.
#' @param ribbon_bend Deprecated; alias for \code{knot.pos}.
#' @example inst/examples/alluvium.r
#' @usage NULL
#' @export
StatAlluvium <- ggproto(
  "StatAlluvium", Stat,
  
  setup_params = function(data, params) {
    
    if (!is.null(data$x) || !is.null(params$x) ||
        !is.null(data$y) || !is.null(params$y)) {
      stop("stat_alluvium() does not accept x or y aesthetics")
    }
    
    if (!is.null(params$axis_width)) {
      warning("Parameter 'axis_width' is deprecated; use 'width' instead.")
      params$width <- params$axis_width
      params$axis_width <- NULL
    }
    if (!is.null(params$width)) {
      if (params$width < 0 | params$width > 1) {
        warning("Argument to parameter 'width' is not between 0 and 1, ",
                "and will be ignored.")
        params$width <- 1/3
      }
    }
    
    if (!is.null(params$ribbon_bend)) {
      warning("Parameter 'ribbon_bend' is deprecated; use 'knot.pos' instead.")
      params$knot.pos <- params$ribbon_bend
      params$ribbon_bend <- NULL
    }
    if (params$knot.pos < 0 | params$knot.pos > .5) {
      warning("Argument to parameter 'knot.pos' is not between 0 and .5, ",
              "and will be ignored.")
      params$knot.pos <- 1/6
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
  
  setup_data = function(data, params) {
    
    if (params$na.rm) {
      data <- na.omit(data)
    } else {
      axis_ind <- get_axes(names(data))
      for (i in axis_ind) {
        if (any(is.na(data[[i]]))) {
          data[[i]] <- addNA(data[[i]], ifany = TRUE)
        }
      }
    }
    
    # assign uniform weight if not provided
    if (is.null(data$weight)) {
      data$weight <- rep(1, nrow(data))
      message("No argument provided for 'weight'; assuming uniform row weights")
    }
    
    # override existing group assignment; assign each row its own group
    data$group <- 1:nrow(data)
    
    # positioning parameters
    data$width <- params$width
    data$knot.pos <- params$knot.pos
    
    data
  },
  
  compute_panel = function(data, scales, params,
                           lode.guidance = "zigzag",
                           bind.by.aes = FALSE,
                           lode.ordering = NULL,
                           width = 1/3, axis_width = NULL,
                           knot.pos = 1/6, ribbon_bend = NULL) {
    
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
        # order lodes according to axes, in above order
        lode_seq <- do.call(order, data[all_ind])
      } else {
        lode_seq <- order(data[[axis_ind[i]]], lode.ordering[, i])
      }
      # lode floors and ceilings along axis
      ymin_seq <- c(0, cumsum(data$weight[lode_seq]))
      ymax_seq <- c(cumsum(data$weight[lode_seq]), sum(data$weight))
      # lode breaks
      cbind(i,
            ymin_seq[order(lode_seq)],
            ymax_seq[order(lode_seq)])
    }
    
    alluvia <- do.call(rbind, lapply(1:length(axis_ind), compute_alluvium))
    colnames(alluvia) <- c("x", "ymin", "ymax")
    data <- data.frame(data, alluvia)
    
    # widths and x bounds
    data$xmin <- data$x - data$width / 2
    data$xmax <- data$x + data$width / 2

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
                          width = 1/3, axis_width = NULL,
                          knot.pos = 1/6, ribbon_bend = NULL,
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
      width = width, axis_width = axis_width,
      knot.pos = knot.pos, ribbon_bend = ribbon_bend,
      na.rm = na.rm,
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
  
  draw_group = function(data, panel_scales, coord) {
    
    first_row <- data[1, setdiff(names(data),
                                 c("x", "xmin", "xmax",
                                   "y", "ymin", "ymax",
                                   "width", "knot.pos")),
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
      w_oneway <- rep(data$width, c(3, rep(4, nrow(data) - 2), 3))
      k_oneway <- rep(data$knot.pos, c(3, rep(4, nrow(data) - 2), 3))
      x_oneway <- rep(data$x, c(3, rep(4, nrow(data) - 2), 3)) +
        w_oneway / 2 * c(-1, rep(c(1, 1, -1, -1), nrow(data) - 1), 1) +
        k_oneway * (1 - w_oneway) * c(0, rep(c(0, 1, -1, 0), nrow(data) - 1), 0)
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
                          width = 1/3, axis_width = NULL,
                          knot.pos = 1/6, ribbon_bend = NULL,
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
      width = width, axis_width = axis_width,
      knot.pos = knot.pos, ribbon_bend = ribbon_bend,
      na.rm = na.rm,
      ...
    )
  )
}
