#' Alluvial positions
#' 
#' Given a dataset with alluvial structure, \code{stat_alluvium} calculates the 
#' centroids (\code{x} and \code{y}) of the \strong{lodes}, the intersections of
#' the alluvia with the strata, together with their weights (heights; 
#' \code{ymin} and \code{ymax}). It leverages the \code{group} aesthetic for 
#' plotting purposes (for now).
#' @template stat-aesthetics
#' 

#' @import ggplot2
#' @family alluvial stat layers
#' @seealso \code{\link[ggplot2]{layer}} for additional arguments and
#'   \code{\link{geom_alluvium}},
#'   \code{\link{geom_lode}}, and
#'   \code{\link{geom_flow}} for the corresponding geoms.
#' @inheritParams stat_flow
#' @param aggregate.wts Whether to aggregate weights across otherwise equivalent
#'   rows before computing lode and flow positions. Set to \code{TRUE} to group
#'   observations into cohorts.
#'   \strong{Warning}: This is currently an expensive operation.
#' @param lode.guidance The function to prioritize the axis variables for 
#'   ordering the lodes within each stratum. Options are "zigzag", "rightleft",
#'   "leftright", "rightward", and "leftward" (see 
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
                          aggregate.wts = FALSE,
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
      aggregate.wts = FALSE,
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
      data <- data[with(data, order(x, stratum, alluvium)), , drop = FALSE]
      data$x <- contiguate(data$x)
    }
    
    data
  },
  
  compute_panel = function(data, scales,
                           decreasing = NA, reverse = TRUE,
                           aggregate.wts = FALSE,
                           lode.guidance = "zigzag",
                           aes.bind = FALSE,
                           lode.ordering = NULL) {
    
    # aggregate weights over otherwise equivalent alluvia
    if (aggregate.wts) data <- aggregate_along(data, "x", "alluvium", "weight")
    # sort data by 'x' then 'alluvium' (to match 'alluv' downstream)
    data <- data[do.call(order, data[, c("x", "alluvium")]), ]
    # ensure that 'alluvium' values are contiguous starting at 1
    data$alluvium <- contiguate(data$alluvium)
    
    # aesthetics (in prescribed order)
    aesthetics <- intersect(.color_diff_aesthetics, names(data))
    
    # create alluvia-format dataset of alluvium stratum assignments,
    # with strata arranged according to 'decreasing' and 'reverse' parameters
    data$deposit <- if (is.na(decreasing)) {
      if (reverse) -xtfrm(data$stratum) else data$stratum
    } else {
      if (decreasing) -data$weight else data$weight
    }
    data$deposit <- match(data$deposit, sort(unique(data$deposit)))
    alluv <- alluviate(data, "x", "deposit", "alluvium")
    data$deposit <- NULL
    # sort by 'alluvium' (to match 'data')
    alluv <- alluv[order(alluv$alluvium), ]
    # axis indices
    alluv_ind <- seq_along(alluv)[-1]
    
    # if 'lode.ordering' not provided, generate it
    if (is.null(lode.ordering)) {
      # invoke surrounding axes in the order prescribed by 'lode.guidance'
      lode_fun <- get(paste0("lode_", lode.guidance))
      # construct a matrix of orderings
      lode.ordering <- do.call(cbind, lapply(seq_along(alluv_ind), function(i) {
        
        # order surrounding axes according to 'lode.guidance'
        axis_seq <- alluv_ind[lode_fun(n = length(alluv_ind), i = i)]
        # order axis aesthetics ...
        aes_dat <- data[data$x == names(alluv)[alluv_ind[i]],
                        c("alluvium", aesthetics),
                        drop = FALSE]
        # ... in the order prescribed by 'reverse'
        if (reverse) {
          aes_dat <- dplyr::mutate_at(aes_dat, -1, dplyr::funs(dplyr::desc))
        }
        # order on aesthetics and surrounding axes according to 'aes.bind'
        ord_dat <- dplyr::left_join(alluv, aes_dat, by = "alluvium")
        ord_col <- if (aes.bind) {
          c(names(alluv)[axis_seq[1]],
            names(aes_dat)[-1],
            names(alluv)[axis_seq[-1]])
        } else {
          c(names(alluv)[axis_seq],
            names(aes_dat)[-1])
        }
        ord_dat <- ord_dat[, ord_col, drop = FALSE]
        # return the ordering
        do.call(order, ord_dat)
      }))
    }
    # check that array has correct dimensions
    stopifnot(dim(lode.ordering) ==
                c(length(unique(data$alluvium)),
                  length(unique(data$x))))
    
    # gather lode positions into alluvium-axis-order table
    alluv[, -1] <- apply(lode.ordering, 2, order)
    alluv_pos <- tidyr::gather(
      alluv,
      key = "x", value = "position",
      alluv_ind
    )
    rm(alluv) # avoid confusion
    alluv_pos$x <- as.integer(alluv_pos$x)
    # join position variable into 'data'
    data <- dplyr::left_join(data, alluv_pos, by = c("x", "alluvium"))
    
    # calculate lode floors and ceilings from positions by axis
    data <- data[order(data$position), , drop = FALSE]
    data <- dplyr::ungroup(dplyr::mutate(dplyr::group_by(data, x),
                                         ymax = cumsum(weight),
                                         ymin = dplyr::lag(ymax, default = 0)))
    stopifnot(isTRUE(all.equal(data$weight, data$ymax - data$ymin)))
    # add vertical centroids
    data <- transform(data, y = (ymin + ymax) / 2)
    
    # within each alluvium, indices at which contiguous subsets start
    data <- data[with(data, order(x, alluvium)), , drop = FALSE]
    data$starts <- duplicated(data$alluvium) &
      !duplicated(data[, c("x", "alluvium")])
    data$axis <- contiguate(data$x)
    # within each alluvium, group contiguous subsets
    # (data is sorted by 'x' and 'alluvium'; group_by() does not reorder it)
    data <- dplyr::ungroup(dplyr::mutate(dplyr::group_by(data, alluvium),
                                         flow = axis - cumsum(starts)))
    # add 'group' to group contiguous alluvial subsets
    data <- transform(data, group = as.numeric(interaction(alluvium, flow)))
    # arrange data by aesthetics for consistent (reverse) z-ordering
    data <- z_order_aes(data, aesthetics)
    
    data
  }
)
