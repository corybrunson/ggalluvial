#' Flow positions
#' 
#' Given a dataset with alluvial structure, \code{stat_flow} calculates the
#' centroids (\code{x} and \code{y}) and weights (heights; \code{ymin} and
#' \code{ymax}) of alluvial flows between each pair of adjacent axes.
#' 
#' @section Aesthetics: \code{stat_flow} understands the following aesthetics
#'   (required aesthetics are in bold):
#' \itemize{
#'   \item \code{x}
#'   \item \code{stratum}
#'   \item \code{alluvium}
#'   \item \code{axis[0-9]*} (\code{axis1}, \code{axis2}, etc.)
#'   \item \code{weight}
#'   \item \code{group}
#' }
#' Currently, \code{group} is ignored.
#' Use \code{x}, \code{stratum}, and \code{alluvium} for data in lode form and 
#' \code{axis[0-9]*} for data in alluvium form (see \code{\link{is_alluvial}});
#' arguments to parameters inconsistent with the data format will be ignored.
#' 
#' @name stat-flow
#' @import ggplot2
#' @seealso \code{\link[ggplot2]{layer}} for additional arguments,
#'   \code{\link{geom_flow}} for the corresponding geom,
#'   \code{\link{stat_stratum}} and \code{\link{geom_stratum}} for
#'   intra-axis boxes, and
#'   \code{\link{ggalluvial}} for a shortcut method.
#' @inheritParams stat-stratum
#' @param aggregate.wts Logical; whether to aggregate weights across otherwise
#'   equivalent rows before computing lode and flow positions. Defaults to TRUE.
#' @param aes.bind Whether to prioritize aesthetics before axes (other than the
#'   index axis) when ordering the lodes within each stratum. Defaults to FALSE.
#' @example inst/examples/ex-stat-flow.r
#' @usage NULL
#' @export
stat_flow <- function(mapping = NULL,
                      data = NULL,
                      geom = "flow",
                      decreasing = NA,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE,
                      ...) {
  layer(
    stat = StatAlluvium,
    data = data,
    mapping = mapping,
    geom = geom,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      decreasing = decreasing,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname stat-flow
#' @usage NULL
#' @export
StatFlow <- ggproto(
  "StatFlow", Stat,
  
  setup_data = function(data, params) {
    
    # assign uniform weight if not provided
    if (is.null(data$weight)) {
      data$weight <- rep(1, nrow(data))
    }
    type <- get_alluvial_type(data)
    if (type == "none") {
      stop("Data is not in a recognized alluvial form ",
           "(see `?is_alluvial` for details).")
    }
    
    if (params$na.rm) {
      data <- na.omit(object = data)
    } else {
      data <- na_keep(data = data, type = type)
    }
    
    # ensure that data is in lode form
    if (type == "alluvia") {
      axis_ind <- get_axes(names(data))
      data <- to_lodes(data = data,
                       key = "x", value = "stratum", id = "alluvium",
                       axes = axis_ind)
      # positioning requires numeric 'x'
      data$x <- as.numeric(as.factor(data$x))
    }
    
    data
  },
  
  compute_panel = function(self, data, scales,
                           decreasing = NA,
                           aggregate.wts = TRUE,
                           aes.bind = FALSE) {
    print(tail(data))
    # convert data to flow format
    flow_pos <- c("x", "stratum", "weight")
    flow_aes <- setdiff(names(data),
                        c(flow_pos, "alluvium", "PANEL", "group"))
    flow_aes_fore <- if (aes.flow == "forward") flow_aes else NULL
    flow_aes_back <- if (aes.flow == "backward") flow_aes else NULL
    data <- self_adjoin(data, "x", "alluvium", pair = flow_pos,
                        keep0 = flow_aes_fore, keep1 = flow_aes_back)
    
    # aggregate over extraneous axis variables
    group_cols <- setdiff(names(data), c("alluvium", "weight0", "weight1"))
    dots <- lapply(group_cols, as.symbol)
    data <- as.data.frame(dplyr::summarize(dplyr::group_by_(data,
                                                            .dots = dots),
                                           weight0 = sum(weight0),
                                           weight1 = sum(weight1)))
    
    # sort according to 'decreasing' parameter
    if (!is.na(decreasing)) {
      data <- if (decreasing) {
        dplyr::arrange(data, PANEL, link, weight0)
      } else {
        dplyr::arrange(data, PANEL, link, weight0)
      }
    }
    
    # cumulative weights
    data$y0 <- NA; data$y1 <- NA
    for (ll in unique(data$link)) {
      ww <- which(data$link == ll)
      data$y0[ww] <- cumsum(data$weight0[ww]) - data$weight0[ww] / 2
      data$y1[ww] <- cumsum(data$weight1[ww]) - data$weight1[ww] / 2
    }
    
    
  }
)

data <- to_lodes(as.data.frame(Titanic), axes = 1:3)
names(data) <- c("fill", "weight", "alluvium", "x", "stratum")
