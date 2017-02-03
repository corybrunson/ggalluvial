#' Variable axes and strata
#' 
#' \code{stat_stratum} calculates the centers of the levels at each axis.
#' 
#' @section Aesthetics: \code{stat_stratum} understands only the \code{group}
#'   aesthetic, but it is currently ignored.
#'   
#' @name stat_stratum
#' @import ggplot2
#' @seealso \code{\link{geom_stratum}} for plotting strata from transformed
#'   data, \code{\link{stat_alluvium}} and \code{\link{geom_alluvium}} for
#'   inter-axis flows and \code{\link{ggalluvial}} for a shortcut method.
#' @inheritParams layer
#' @param axis_width The width of each variable axis, as a proportion of the 
#'   separation between axes.
#' @example inst/examples/stratum.r
#' @usage NULL
#' @export
StatStratum <- ggproto(
  "StatStratum", Stat,
  setup_data = function(data, params) {
    
    if (is.null(data$weight)) data$weight <- rep(1, nrow(data))
    
    data <- aggregate(
      formula = as.formula(paste("weight ~",
                                 paste(setdiff(names(data), "weight"),
                                       collapse = "+"))),
      data = data,
      FUN = sum
    )
    
    axis_ind <- get_axes(names(data))
    # stack axis-aggregated data with cumulative frequencies
    res_data <- do.call(rbind, lapply(unique(data$PANEL), function(p) {
      p_data <- subset(data, PANEL == p)
      do.call(rbind, lapply(1:length(axis_ind), function(i) {
        agg <- aggregate(x = p_data$weight, by = p_data[axis_ind[i]],
                         FUN = sum)
        names(agg) <- c("label", "weight")
        cbind(pos = i, agg, cumweight = cumsum(agg$weight), PANEL = p)
      }))
    }))
    
    # add group
    cbind(res_data, group = 1:nrow(res_data))
  },
  compute_group = function(data, scales,
                           axis_width = 1/3) {
    
    rownames(data) <- NULL
    rect_data <- data.frame(x = data$pos,
                            y = (data$cumweight - data$weight / 2),
                            width = axis_width)
    data.frame(data, rect_data)
  }
)

#' @rdname stat_stratum
#' @usage NULL
#' @export
stat_stratum <- function(mapping = NULL, data = NULL, geom = "stratum",
                         na.rm = FALSE, show.legend = NA,
                         inherit.aes = TRUE, ...) {
  layer(
    stat = StatStratum, data = data, mapping = mapping, geom = geom,
    position = "identity", show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
