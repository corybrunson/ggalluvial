#' Alluvial flows
#' 
#' \code{stat_alluvium} calculates the depth of each group at each axis. 
#' \code{geom_alluvium} plots an x-spline for each group through the axes at 
#' these depths.
#' 
#' @section Aesthetics:
#' \code{stat_alluvium} understands only the \code{group} 
#'   aesthetic, but it is currently ignored.
#' 
#' @name stat-alluvium
#' @import ggplot2
#' @seealso \code{\link{stat_stratum}} and \code{\link{geom_stratum}} for
#'   intra-axis boxes, 
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
#' @example inst/examples/ex-alluvium.r
#' @usage NULL
#' @export
stat_alluvium <- function(mapping = NULL,
                          data = NULL,
                          geom = "alluvium",
                          position = "identity",
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
      ...
    )
  )
}

#' @rdname stat-alluvium
#' @usage NULL
#' @export
StatAlluvium <- ggproto(
  "StatAlluvium", Stat,
  
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
    }
    
    # override existing group assignment; assign each row its own group
    data$group <- 1:nrow(data)
    
    data
  },
  
  compute_panel = function(data, scales, params,
                           lode.guidance = "zigzag",
                           bind.by.aes = FALSE,
                           lode.ordering = NULL) {
    
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
    
    # y centers
    data <- transform(data,
                      y = (ymin + ymax) / 2)
    
    data
  }
)
