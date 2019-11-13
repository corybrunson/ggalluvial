#' Alluvial positions
#'
#' Given a dataset with alluvial structure, `stat_alluvium` calculates the
#' centroids (`x` and `y`) of the **lodes**, the intersections of
#' the alluvia with the strata, together with their weights (heights;
#' `ymin` and `ymax`). It leverages the `group` aesthetic for
#' plotting purposes (for now).
#' @template stat-aesthetics
#'

#' @import ggplot2
#' @family alluvial stat layers
#' @seealso [ggplot2::layer()] for additional arguments and [geom_alluvium()],
#'   [geom_lode()], and [geom_flow()] for the corresponding geoms.
#' @inheritParams stat_flow
#' @param aggregate.y Whether to aggregate weights across otherwise equivalent
#'   rows before computing lode and flow positions. Set to `TRUE` to group
#'   observations into cohorts. **Warning**: This is currently an expensive
#'   operation.
#' @param lode.guidance The function to prioritize the axis variables for
#'   ordering the lodes within each stratum, or else a character string
#'   identifying the function. Character options are "zigzag", "frontback",
#'   "backfront", "forward", and "backward" (see [`lode-guidance-functions`]).
#' @param lode.ordering A list (of length the number of axes) of integer vectors
#'   (each of length the number of rows of `data`) or NULL entries (indicating
#'   no imposed ordering), or else a numeric matrix of corresponding dimensions,
#'   giving the preferred ordering of alluvia at each axis. This will be used to
#'   order the lodes within each stratum by sorting the lodes first by stratum
#'   and then by the provided vectors.
#' @example inst/examples/ex-stat-alluvium.r
#' @export
stat_alluvium <- function(mapping = NULL,
                          data = NULL,
                          geom = "alluvium",
                          position = "identity",
                          decreasing = NA,
                          reverse = TRUE,
                          absolute = FALSE,
                          discern = FALSE,
                          aes.bind = FALSE,
                          aggregate.y = FALSE,
                          lode.guidance = "zigzag",
                          lode.ordering = NULL,
                          negate.strata = NULL,
                          min.y = NULL, max.y = NULL,
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
      absolute = absolute,
      discern = discern,
      aggregate.y = aggregate.y,
      lode.guidance = lode.guidance,
      lode.ordering = lode.ordering,
      negate.strata = negate.strata,
      min.y = min.y, max.y = max.y,
      aes.bind = aes.bind,
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
    
    if (! is.null(params$lode.ordering)) {
      if (is.list(params$lode.ordering)) {
        # replace any null entries with uniform `NA` vectors
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
    
    # assign `alluvium` to `stratum` if `stratum` not provided
    if (is.null(data$stratum) & ! is.null(data$alluvium)) {
      data <- transform(data, stratum = alluvium)
    }
    
    # assign uniform weight if not provided
    if (is.null(data$y)) {
      if (is.null(data$weight)) {
        data$y <- rep(1, nrow(data))
      } else {
        defunct_parameter("weight", "y", type = "aesthetic")
        data$y <- data$weight
        data$weight <- NULL
      }
    } else if (any(is.na(data$y))) {
      stop("Data contains missing `y` values.")
    }
    
    type <- get_alluvial_type(data)
    if (type == "none") {
      stop("Data is not in a recognized alluvial form ",
           "(see `help('alluvial-data')` for details).")
    }
    
    if (params$na.rm) {
      data <- na.omit(object = data)
    } else {
      data <- na_keep(data = data, type = type)
    }
    
    # ensure that data is in lode form
    if (type == "alluvia") {
      axis_ind <- get_axes(names(data))
      data <- to_lodes_form(data = data, axes = axis_ind,
                            discern = params$discern)
      # positioning requires numeric `x`
      data <- data[with(data, order(x, stratum, alluvium)), , drop = FALSE]
      data$x <- contiguate(data$x)
    } else {
      if (! is.null(params$discern) && ! (params$discern == FALSE)) {
        warning("Data is already in lodes format, ",
                "so `discern` will be ignored.")
      }
    }
    
    # negate strata
    if (! is.null(params$negate.strata)) {
      if (! all(params$negate.strata %in% unique(data$stratum))) {
        warning("Some values of `negate.strata` are not among strata.")
      }
      wneg <- which(data$stratum %in% params$negate.strata)
      if (length(wneg) > 0) data$y[wneg] <- -data$y[wneg]
    }
    
    data
  },
  
  compute_panel = function(data, scales,
                           decreasing = NA, reverse = TRUE, absolute = FALSE,
                           discern = FALSE,
                           aggregate.y = FALSE,
                           lode.guidance = "zigzag",
                           negate.strata = NULL,
                           min.y = NULL, max.y = NULL,
                           aes.bind = FALSE,
                           lode.ordering = NULL) {
    
    # aesthetics (in prescribed order)
    aesthetics <- intersect(.color_diff_aesthetics, names(data))
    
    # sign variable (sorts positives before negatives)
    data$yneg <- data$y < 0
    
    # aggregate weights over otherwise equivalent alluvia
    if (aggregate.y) data <- aggregate_y_along(data, "x", "alluvium")
    
    # define 'deposit' variable to rank (signed) strata
    # deposits will be stacked positively, then negatively, from zero
    if (is.na(decreasing)) {
      deposits <- unique(data[, c("x", "yneg", "stratum")])
      deposits <- transform(deposits, deposit = order(order(
        x, yneg,
        xtfrm(stratum) * (-1) ^ (reverse + yneg * ! absolute)
      )))
    } else {
      deposits <- stats::aggregate(
        x = data$y,
        by = data[, c("x", "yneg", "stratum"), drop = FALSE],
        FUN = sum
      )
      names(deposits)[ncol(deposits)] <- "y"
      deposits <- transform(deposits, deposit = order(order(
        x, yneg,
        xtfrm(y) * (-1) ^ (decreasing + yneg * ! absolute),
        xtfrm(stratum) * (-1) ^ (reverse + yneg * ! absolute)
      )))
      deposits$y <- NULL
    }
    data <- merge(data, deposits,
                  #by = c("x", "yneg", "stratum"),
                  all.x = TRUE, all.y = FALSE)
    
    # summary data of alluvial deposits
    #alluv_dep <- alluviate(data, "x", "deposit", "alluvium")
    data <- transform(data, depth = deposit * (-1) ^ yneg)
    alluv_dep <- alluviate(data, "x", "depth", "alluvium")
    # axis indices
    alluv_ind <- seq_along(alluv_dep)[-1]
    
    # if `lode.ordering` not provided, generate it
    if (is.null(lode.ordering)) {
      # invoke surrounding axes in the order prescribed by `lode.guidance`
      if (is.character(lode.guidance)) {
        lode.guidance <- get(paste0("lode_", lode.guidance))
      }
      stopifnot(is.function(lode.guidance))
      # construct a matrix of orderings
      lode.ordering <- do.call(cbind, lapply(seq_along(alluv_ind), function(i) {
        
        # -+- align alluvia between positive and negative strata -+-
        # -+- insert 'ypn' variable after adjacent deposits and aesthetics -+-
        # use 'depth' rather than 'deposit' above
        # make `dep_dat` based on `alluv_dep` with absolute depths (deposits)
        # within each index deposit:
        #   reverse order of adjacent deposits with opposite sign
        dep_dat <- alluv_dep
        dep_dat[, alluv_ind] <- abs(dep_dat[, alluv_ind])
        for (d in unique(dep_dat[[alluv_ind[i]]])) {
          for (j in setdiff(alluv_ind, alluv_ind[i])) {
            ypn <- sign(alluv_dep[[j]][dep_dat[[alluv_ind[i]]] == d]) !=
              sign(alluv_dep[[alluv_ind[i]]][dep_dat[[alluv_ind[i]]] == d])
            dep_dat[[j]][dep_dat[[alluv_ind[i]]] == d][ypn] <-
              rev(dep_dat[[j]][dep_dat[[alluv_ind[i]]] == d][ypn])
          }
        }
        
        # order surrounding axes according to `lode.guidance`
        axis_col <- alluv_ind[lode.guidance(n = length(alluv_ind), i = i)[-1]]
        
        # order axis aesthetics ...
        aes_dat <- data[data$x == names(alluv_dep)[alluv_ind[i]],
                        c("alluvium", aesthetics),
                        drop = FALSE]
        # ... in the order prescribed by `reverse`
        for (j in seq(ncol(aes_dat))[-1]) {
          aes_dat[[j]] <- xtfrm(aes_dat[[j]])
          if (reverse) aes_dat[[j]] <- -aes_dat[[j]]
        }
        
        # order on aesthetics and surrounding axes
        ord_dat <- merge(dep_dat, aes_dat, all.x = TRUE, all.y = FALSE)
        # prioritize aesthetics according as `aes.bind`
        ord_col <- if (aes.bind) {
          c(names(aes_dat)[-1], names(dep_dat)[axis_col])
        } else {
          c(names(dep_dat)[axis_col], names(aes_dat)[-1])
        }
        ord_dat <- ord_dat[, ord_col, drop = FALSE]
        
        # return a variable encoding this ordering
        order(do.call(order, ord_dat))
      }))
      
      alluv_dep[, -1] <- lode.ordering
    } else {
      # bind a vector to itself to create a matrix
      if (is.vector(lode.ordering)) {
        lode.ordering <- matrix(lode.ordering,
                                nrow = length(lode.ordering),
                                ncol = length(unique(data$x)))
      }
      # check that array has correct dimensions
      stopifnot(dim(lode.ordering) ==
                  c(length(unique(data$alluvium)),
                    length(unique(data$x))))
      # ensure that data are sorted first by deposit, only then by lode.ordering
      alluv_dep[, -1] <- sapply(seq_along(alluv_ind), function(i) {
        order(order(abs(alluv_dep[, alluv_ind[i]]), lode.ordering[, i]))
      })
    }
    
    # gather lode positions into alluvium-axis-order table
    alluv_pos <- tidyr::gather(
      alluv_dep,
      key = "x", value = "pos",
      alluv_ind
    )
    alluv_pos$x <- as.integer(alluv_pos$x)
    # join 'pos' variable into `data`
    data <- merge(data, alluv_pos, all.x = TRUE, all.y = FALSE)
    
    # sort data by deposit, then position, in preparation for 'y' sums
    data <- data[do.call(order, data[, c("deposit", "pos")]), , drop = FALSE]
    # calculate cumulative weights
    data$ycum <- NA
    for (xx in unique(data$x)) {
      for (yn in c(FALSE, TRUE)) {
        ww <- which(data$x == xx & data$yneg == yn)
        data$ycum[ww] <- cumsum(data$y[ww]) - data$y[ww] / 2
      }
    }
    # calculate y bounds
    data <- transform(data,
                      pos = NULL,
                      ymin = ycum - abs(y) / 2,
                      ymax = ycum + abs(y) / 2,
                      y = ycum)
    
    # within each alluvium, indices at which subsets are contiguous
    data <- data[with(data, order(x, alluvium)), , drop = FALSE]
    data$cont <- duplicated(data$alluvium) &
      ! duplicated(data[, c("x", "alluvium")])
    data$axis <- contiguate(data$x)
    # within each alluvium, group contiguous subsets
    # (data is sorted by `x` and `alluvium`; group_by() does not reorder it)
    data <- dplyr::ungroup(dplyr::mutate(dplyr::group_by(data, alluvium),
                                         flow = axis - cumsum(cont)))
    # add `group` to group contiguous alluvial subsets
    data <- transform(data, group = as.numeric(interaction(alluvium, flow)))
    # remove unused fields
    data$cont <- NULL
    data$axis <- NULL
    data$flow <- NULL
    
    # impose height restrictions
    if (! is.null(min.y)) {
      data <- data[data$ymax - data$ymin >= min.y, , drop = FALSE]
    }
    if (! is.null(max.y)) {
      data <- data[data$ymax - data$ymin <= max.y, , drop = FALSE]
    }
    
    # arrange data by aesthetics for consistent (reverse) z-ordering
    data <- z_order_aes(data, aesthetics)
    
    data
  }
)

# aggregate weights over otherwise equivalent alluvia (omitting missing values)
aggregate_y_along_old <- function(data, key, id) {
  
  # interaction of all variables to aggregate over (without dropping NAs)
  data$binding <- as.numeric(interaction(lapply(
    data[, -match(c(key, id, "y"), names(data)), drop = FALSE],
    addNA, ifany = FALSE
  ), drop = TRUE))
  # convert to alluvia format with 'binding' entries
  alluv_data <- alluviate(data, key, "binding", id)
  # sort by all axes (everything except `id`)
  alluv_data <- alluv_data[do.call(
    order,
    alluv_data[, -match(id, names(alluv_data)), drop = FALSE]
  ), , drop = FALSE]
  
  # define map from original to aggregated `id`s
  alluv_orig <- alluv_data[[id]]
  alluv_agg <- cumsum(! duplicated(interaction(
    alluv_data[, -match(id, names(alluv_data)), drop = FALSE]
  )))
  # transform `id` in `data` accordingly
  data[[id]] <- alluv_agg[match(data[[id]], alluv_orig)]
  
  # aggregate `var` by all other variables
  data_agg <- stats::aggregate(formula = y ~ .,
                               data = data[, c(key, id, "binding", "y")],
                               FUN = sum)
  # merge into `data`, ensuring that no `key`-`id` pairs are duplicated
  data <- unique(merge(
    data_agg,
    data[, -match("y", names(data))],
    all.x = TRUE, all.y = FALSE
  ))
  data$binding <- NULL
  
  data
}

# aggregate weights over otherwise equivalent alluvia (omitting missing values)
aggregate_y_along <- function(data, key, id) {
  
  # interaction of all variables to aggregate over (without dropping NAs)
  data$.binding <- as.numeric(interaction(lapply(
    data[, -match(c(key, id, "y"), names(data)), drop = FALSE],
    addNA, ifany = FALSE
  ), drop = TRUE))
  
  # convert to alluvia format with '.binding' entries
  alluv_data <- alluviate(data, key, ".binding", id)
  
  # redefine 'alluvium' with only as many distinct values as distinct alluvia
  alluv_agg <- stats::aggregate(
    alluv_data[[id]],
    by = alluv_data[, rev(setdiff(names(alluv_data), id))],
    FUN = most
  )
  names(alluv_agg)[ncol(alluv_agg)] <- ".id"
  alluv_data <- merge(alluv_data, alluv_agg, all.x = TRUE, all.y = FALSE)
  data[[id]] <- alluv_data$.id[match(data[[id]], alluv_data[[id]])]
  
  # aggregate 'y' by `key`, `id`, and .binding
  data_agg <- stats::aggregate(
    formula = y ~ .,
    data = data[, c(key, id, ".binding", "y")],
    FUN = sum
  )
  
  # merge into `data`, ensuring that no `key`-`id` pairs are duplicated
  data <- merge(
    data_agg,
    data[, -match("y", names(data))],
    all.x = TRUE, all.y = FALSE
  )
  data$.binding <- NULL
  
  data
}
