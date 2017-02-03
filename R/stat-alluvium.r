#' Alluvial flows
#' 
#' \code{stat_alluvium} calculates the depth of each group at each axis, for the
#' purpose of tracing flows using \code{\link{geom_alluvium}}.
#' 
#' @section Aesthetics: \code{stat_alluvium} understands only the \code{group} 
#'   aesthetic, but it is currently ignored.
#'   
#' @name stat_alluvium
#' @import ggplot2
#' @seealso \code{\link{geom_alluvium}} for plotting alluvia from transformed
#'   data, \code{\link{stat_stratum}} and \code{\link{geom_stratum}} for
#'   intra-axis boxes, and \code{\link{ggalluvial}} for a shortcut method.
#' @inheritParams layer
#' @param axis_width The width of each variable axis, as a proportion of the 
#'   separation between axes.
#' @example inst/examples/alluvium.r
#' @usage NULL
#' @export
StatAlluvium <- ggproto(
  "StatAlluvium", Stat,
  setup_data = function(data, params) {
    
    if (is.null(data$weight)) data$weight <- rep(1, nrow(data))
    
    # if group variable does not respect axes, print message and fix it
    interact <- interaction(data[, grep("^axis[0-9\\.]*", names(data))])
    if (!splinters(data$group, interact)) {
      message(paste0("'group' assignments do not respect axis assignments,",
                     " and will be ignored."))
      data$group <- as.numeric(interact)
    }
    
    # aggregate over axes and groups by weight
    aggregate(
      formula = as.formula(paste("weight ~",
                                 paste(setdiff(names(data), "weight"),
                                       collapse = "+"))),
      data = data,
      FUN = sum
    )
  },
  compute_panel = function(data, scales, params,
                           axis_width = 1/3) {
    
    axis_ind <- get_axes(names(data))
    
    # x and y coordinates of center of flow at each axis
    compute_alluvium <- function(i) {
      # order axis indices
      axis_seq <- axis_ind[zigzag(n = length(axis_ind), i = i)]
      # order ribbons according to axes, in above order
      ribbon_seq <- do.call(order, data[axis_seq])
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
    data$xmin <- data$x - axis_width / 2
    data$xmax <- data$x + axis_width / 2
    data$width <- axis_width
    
    # y centers
    data$y <- (data$ymin + data$ymax) / 2
    
    data
  }
)

#' @rdname stat_alluvium
#' @usage NULL
#' @export
stat_alluvium <- function(mapping = NULL, data = NULL, geom = "alluvium",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    stat = StatAlluvium, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
