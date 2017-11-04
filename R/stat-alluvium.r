#' Alluvial positions
#' 
#' Given a dataset with alluvial structure, \code{stat_alluvium} calculates the 
#' centroids (\code{x} and \code{y}) of the \strong{lodes}, the intersections of
#' the alluvia with the strata, together with their weights (heights; 
#' \code{ymin} and \code{ymax}). It leverages the \code{group} aesthetic for 
#' plotting purposes (for now).
#' 
#' @section Aesthetics:
#' \code{stat_alluvium} requires one of two sets of aesthetics:
#' \itemize{
#'   \item \code{x}, \code{alluvium}, and (optionally) \code{stratum}
#'   \item any number of \code{axis[0-9]*} (\code{axis1}, \code{axis2}, etc.)
#' }
#' Use \code{x}, \code{alluvium}, and \code{stratum} for data in lodes format
#' and \code{axis[0-9]*} for data in alluvia format
#' (see \code{\link{is_alluvial}}).
#' Arguments to parameters inconsistent with the format will be ignored.
#' Additionally, \code{stat_alluvium} accepts the following optional aesthetics:
#' \itemize{
#'   \item \code{weight}
#'   \item \code{group}
#' }
#' \code{weight} controls the vertical dimensions of the alluvia
#' and are aggregated across equivalent observations.
#' \code{group} is used internally; arguments are ignored.
#' 
#' @import ggplot2
#' @seealso \code{\link[ggplot2]{layer}} for additional arguments,
#'   \code{\link{geom_alluvium}} for the corresponding geom,
#'   \code{\link{stat_stratum}} and \code{\link{geom_stratum}} for
#'   intra-axis boxes, and
#'   \code{\link{ggalluvial}} for a shortcut method.
#' @inheritParams stat_flow
#' @param aggregate.wts Logical; whether to aggregate weights across otherwise
#'   equivalent rows before computing lode and flow positions. Defaults to TRUE.
#' @param lode.guidance The function to prioritize the axis variables for 
#'   ordering the lodes within each stratum. Defaults to "zigzag", other options
#'   include "rightleft", "leftright", "rightward", and "leftward" (see 
#'   \code{\link{lode-guidance-functions}}).
#' @param lode.ordering A list (of length the number of axes) of integer vectors
#'   (each of length the number of rows of \code{data}) or NULL entries 
#'   (indicating no imposed ordering), or else a numeric matrix of corresponding
#'   dimensions, giving the preferred ordering of alluvia at each axis. This 
#'   will be used to order the lodes within each stratum by sorting the lodes 
#'   first by stratum and then by the provided vectors.
#' @example inst/examples/ex-stat-alluvium.r
#' @export
stat_alluvium <- function(mapping = NULL,
                          data = NULL,
                          geom = "alluvium",
                          position = "identity",
                          decreasing = NA,
                          reverse = TRUE,
                          aggregate.wts = TRUE,
                          lode.guidance = "zigzag",
                          lode.ordering = NULL,
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
      decreasing = decreasing,
      reverse = reverse,
      aggregate.wts = TRUE,
      lode.guidance = lode.guidance,
      lode.ordering = lode.ordering,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggalluvial-ggproto
#' @usage NULL
#' @export
StatAlluvium <- ggproto(
  "StatAlluvium", Stat,
  
  required_aes = c("x"),
  
  setup_params = function(data, params) {
    
    if (!is.null(params$lode.ordering)) {
      if (is.list(params$lode.ordering)) {
        # replace any null entries with uniform NA vectors
        wh_null <- which(sapply(params$lode.ordering, is.null))
        len <- unique(sapply(params$lode.ordering[wh_null], length))
        if (length(len) > 1) stop("Lode orderings have different lengths.")
        for (w in wh_null) params$lode.ordering[[w]] <- rep(NA, len)
        # convert list to array (requires equal-length numeric entries)
        params$lode.ordering <- do.call(cbind, params$lode.ordering)
      }
    }
    
    params
  },
  
  setup_data = function(data, params) {
    
    # assign 'alluvium' to 'stratum' if 'stratum' not provided
    if (is.null(data$stratum) & !is.null(data$alluvium)) {
      data <- transform(data, stratum = alluvium)
    }
    
    # assign uniform weight if not provided
    if (is.null(data$weight)) {
      data$weight <- rep(1, nrow(data))
    } else if (any(is.na(data$weight))) {
      stop("Data contains NA weights.")
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
      data <- to_lodes(data = data, axes = axis_ind)
      # positioning requires numeric 'x'
      data <- dplyr::arrange(data, x, stratum, alluvium)
      data$x <- cumsum(!duplicated(data$x))
    }
    
    data
  },
  
  compute_panel = function(data, scales,
                           decreasing = NA, reverse = TRUE,
                           aggregate.wts = TRUE,
                           lode.guidance = "zigzag",
                           aes.bind = FALSE,
                           lode.ordering = NULL) {
    
    # aggregate up to weight, respecting 'na.rm' parameter
    if (aggregate.wts) {
      # http://stackoverflow.com/a/28182288/4556798
      group_cols <- setdiff(names(data), "weight")
      dots <- lapply(group_cols, as.symbol)
      data <- as.data.frame(dplyr::summarize(dplyr::group_by_(data,
                                                              .dots = dots),
                                             weight = sum(weight)))
    }
    
    # introduce any missing rows
    #if (fill.zeros) {
    #  grid_data <- expand.grid(x = unique(data$x),
    #                           alluvium = unique(data$alluvium))
    #  data <- merge(data, grid_data, all = TRUE)
    #}
    
    # sort data by 'x' then 'alluvium' (to match 'alluv')
    data <- data[do.call(order, data[, c("x", "alluvium")]), ]
    
    if (is.null(lode.ordering)) {
      lode_fn <- get(paste0("lode_", lode.guidance))
    } else {
      # check that array has correct dimensions
      stopifnot(dim(lode.ordering) ==
                  c(dplyr::n_distinct(data$alluvium),
                    dplyr::n_distinct(data$x)))
    }
    
    # aesthetic fields
    aes_col <- setdiff(names(data),
                       c("x", "stratum", "alluvium",
                         "weight", "PANEL", "group"))
    # put axis fields into alluvial form, according to 'decreasing' parameter
    data$deposit <- if (is.na(decreasing)) {
      if (reverse) dplyr::desc(data$stratum) else data$stratum
    } else {
      if (decreasing) -data$weight else data$weight
    }
    alluv <- alluviate(data, "x", "deposit", "alluvium")
    data$deposit <- NULL
    # sort by 'alluvium' (to match 'data')
    alluv <- alluv[order(alluv$alluvium), ]
    # axis and aesthetic indices
    axis_ind <- which(!(names(alluv) %in% names(data)))
    # sort within axes by stratum according to 'reverse' parameter
    arr_fun <- if (reverse) dplyr::desc else identity
    
    # vertical positions of flows at each axis
    position_lodes <- function(i) {
      # defined rows
      wh_def <- which(!is.na(alluv[[axis_ind[i]]]))
      # depends on whether the user has provided a lode.ordering
      if (is.null(lode.ordering)) {
        # order axis indices
        axis_seq <- axis_ind[lode_fn(n = length(axis_ind), i = i)]
        # order lodes according to axes and aesthetics
        aes_dat <- subset(data, x == names(alluv)[axis_ind[i]])[aes_col]
        for (var in names(aes_dat)) aes_dat[[var]] <- arr_fun(aes_dat[[var]])
        lode_seq <- do.call(
          order,
          if (aes.bind) {
            cbind(alluv[wh_def, axis_seq[1], drop = FALSE],
                  aes_dat,
                  alluv[wh_def, axis_seq[-1], drop = FALSE])
          } else {
            cbind(alluv[wh_def, axis_seq, drop = FALSE],
                  aes_dat)
          }
        )
      } else {
        lode_seq <- order(alluv[[axis_ind[i]]][wh_def],
                          lode.ordering[wh_def, i])
      }
      # lode floors and ceilings along axis
      subdata <- subset(data, x == names(alluv)[axis_ind[i]])
      cumweight <- cumsum(subdata$weight[lode_seq])
      ymin_seq <- c(0, cumweight)
      ymax_seq <- c(cumweight, sum(subdata$weight[lode_seq]))
      # lode breaks
      data.frame(x = I(names(alluv)[axis_ind[i]]),
                 ymin = ymin_seq[order(lode_seq)],
                 ymax = ymax_seq[order(lode_seq)])
    }
    lode_positions <- do.call(rbind, lapply(1:length(axis_ind), position_lodes))
    data <- cbind(data, lode_positions[, -1])
    stopifnot(isTRUE(all.equal(data$weight, data$ymax - data$ymin)))
    # add vertical centroids
    data <- transform(data,
                      y = (ymin + ymax) / 2)
    
    # within each alluvium, indices at which contiguous subsets start
    #data <- data[do.call(order, data[, c("x", "alluvium")]), ]
    data <- transform(data,
                      starts = duplicated(data$alluvium) &
                        !duplicated(data[, c("x", "alluvium")]),
                      #axis = as.numeric(as.factor(as.character(x))))
                      axis = cumsum(!duplicated(x)))
    # within each alluvium, group contiguous subsets
    # (data is sorted by 'x' and 'alluvium'; group_by() does not reorder it)
    data <- dplyr::ungroup(dplyr::mutate(dplyr::group_by(data, alluvium),
                                         flow = axis - cumsum(starts)))
    # add 'group' to group contiguous alluvial subsets
    data <- transform(data,
                      group = as.numeric(interaction(alluvium, flow)))
    # arrange data by aesthetics for consistent (reverse) z-ordering
    colour_fill_aes <- intersect(names(data), c("colour", "fill"))
    if (length(colour_fill_aes) > 0) {
      data <- dplyr::arrange_(data, colour_fill_aes)
      data <- transform(data,
                        group = as.numeric(factor(
                          as.character(data$group),
                          levels = as.character(unique(rev(data$group)))
                        )))
    }
    
    data
  }
)

# build alluvial dataset for reference during lode-ordering
alluviate <- function(data, key, value, id) {
  to_alluvia(
    data[, c(key, value, id)],
    key = key, value = value, id = id
  )
}
