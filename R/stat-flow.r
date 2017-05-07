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
#' @param aes.bind Whether to prioritize aesthetics before axes (other than the
#'   index axis) when ordering the lodes within each stratum. Defaults to FALSE.
#' @example inst/examples/ex-stat-flow.r
#' @usage NULL
#' @export
stat_flow <- function(mapping = NULL,
                      data = NULL,
                      geom = "flow",
                      position = "identity",
                      decreasing = NA,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE,
                      ...) {
  layer(
    stat = StatFlow,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
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
  
  required_aes = c("x"),
  
  setup_data = function(data, params) {
    
    # assign 'alluvium' to 'stratum' if 'stratum' not provided
    if (is.null(data$stratum) & !is.null(data$alluvium)) {
      data <- transform(data, stratum = alluvium)
    }
    
    # assign uniform weight if not provided
    if (is.null(data$weight)) {
      data$weight <- rep(1, nrow(data))
    }
    type <- get_alluvial_type(data)
    if (type == "none") {
      stop("Data is not in a recognized alluvial form ",
           "(see `help(is_alluvial)` for details).")
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
    
    # aesthetics
    aesthetics <- setdiff(names(data),
                          c("weight", "PANEL", "group",
                            "alluvium", "x", "stratum"))
    
    # sort within axes by weight according to 'decreasing' parameter
    if (!is.na(decreasing)) {
      deposits <- aggregate(x = data$weight * (1 - decreasing * 2),
                            by = data[, c("x", "stratum"), drop = FALSE],
                            FUN = sum)
      names(deposits)[3] <- "deposit"
    }
    
    # stack starts and ends of flows, using 'alluvium' to link them
    # 'side' will become a field of 'adj'
    x_ran <- range(data$x)
    data$alluvium <- as.numeric(as.factor(data$alluvium))
    alluvium_max <- max(data$alluvium)
    data <- dplyr::bind_rows(
      transform(dplyr::filter(data, x != x_ran[2]),
                link = as.numeric(x),
                alluvium = alluvium + alluvium_max * (as.numeric(x) - 1),
                side = I("start")),
      transform(dplyr::filter(data, x != x_ran[1]),
                link = as.numeric(x) - 1,
                alluvium = alluvium + alluvium_max * (as.numeric(x) - 2),
                side = I("end"))
    )
    data$side <- factor(data$side, levels = c("start", "end"))
    stopifnot(all(table(data$alluvium) == 2))
    
    # distinguish aesthetics for flow partition versus for flow interpolation
    n_ports <- dplyr::n_distinct(data[, c("link", "side", "stratum")])
    aes_partition <- aesthetics[which(sapply(aesthetics, function(x) {
      dplyr::n_distinct(data[, c("link", "side", "stratum", x)])
    }) > n_ports)]
    aes_interpolate <- setdiff(aesthetics, aes_partition)
    
    # group flows between common strata
    adj <- tidyr::spread(data[, c("link", "alluvium", "stratum", "side")],
                         key = side, value = stratum)
    adj <- transform(adj,
                     flow = interaction(link, start, end, drop = TRUE))
    data <- merge(data,
                  adj[, c("alluvium", "flow")],
                  by = "alluvium", all.x = TRUE, all.y = FALSE)
    # evidently quicker, but clunkier
    #flow <- adj$flow
    #names(flow) <- adj$alluvium
    #data$flow <- flow[as.character(data$alluvium)]
    data$alluvium <- as.numeric(interaction(data[, c(aes_partition, "flow")],
                                            drop = TRUE))
    # aggregate alluvial segments within flows
    group_cols <- setdiff(names(data), c("weight", "group"))
    dots <- lapply(group_cols, as.symbol)
    data <- as.data.frame(dplyr::summarize(dplyr::group_by_(data, .dots = dots),
                                           weight = sum(weight)))
    stopifnot(all(table(data$alluvium) == 2))
    data <- transform(data,
                      group = alluvium)
    
    # sort in preparation for calculating cumulative weights
    if (!is.na(decreasing)) {
      data <- merge(data, deposits, all.x = TRUE, all.y = FALSE)
    }
    sort_fields <- c(
      "link", "x",
      if (is.na(decreasing)) "stratum" else "deposit",
      if (aes.bind) {
        c(aes_partition, "flow")
      } else {
        c("flow", aes_partition)
      },
      "alluvium", "side"
    )
    data <- data[do.call(order, data[, sort_fields]), ]
    # calculate cumulative weights
    data$y <- NA
    for (ll in unique(data$link)) for (ss in unique(data$side)) {
      ww <- which(data$link == ll & data$side == ss)
      data$y[ww] <- cumsum(data$weight[ww]) - data$weight[ww] / 2
    }
    # y bounds
    transform(data,
              ymin = y - weight / 2,
              ymax = y + weight / 2)
  }
)
