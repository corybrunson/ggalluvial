#' Alluvial positions
#'
#' Given a dataset with alluvial structure, `stat_alluvium` calculates the
#' centroids (`x` and `y`) and heights (`ymin` and `ymax`) of the lodes, the
#' intersections of the alluvia with the strata. It leverages the `group`
#' aesthetic for plotting purposes (for now).
#' @template stat-aesthetics
#' @template order-options
#' @template defunct-stat-params
#'

#' @import ggplot2
#' @importFrom rlang .data
#' @family alluvial stat layers
#' @seealso [ggplot2::layer()] for additional arguments and [geom_alluvium()],
#'   [geom_lode()], and [geom_flow()] for the corresponding geoms.
#' @inheritParams stat_flow
#' @param cement.alluvia Logical value indicating whether to aggregate `y`
#'   values over equivalent alluvia before computing lode and flow positions.
#'   Alternatively, a function (or its name) to combine the labels (if any) of
#'   equivalent alluvia, similar to the `distill` parameter of
#'   [to_alluvia_form()]). If set to `TRUE`, defaults to function
#'   `[dplyr::first()]`.
#' @param aggregate.y Deprecated alias for `cement.alluvia`.
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
                          decreasing = ggalluvial_opt("decreasing"),
                          reverse = ggalluvial_opt("reverse"),
                          absolute = ggalluvial_opt("absolute"),
                          discern = FALSE,
                          negate.strata = NULL,
                          aggregate.y = NULL,
                          cement.alluvia = ggalluvial_opt("cement.alluvia"),
                          lode.guidance = ggalluvial_opt("lode.guidance"),
                          lode.ordering = ggalluvial_opt("lode.ordering"),
                          aes.bind = ggalluvial_opt("aes.bind"),
                          infer.label = FALSE,
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
      negate.strata = negate.strata,
      aggregate.y = aggregate.y,
      cement.alluvia = cement.alluvia,
      lode.guidance = lode.guidance,
      lode.ordering = lode.ordering,
      aes.bind = aes.bind,
      infer.label = infer.label,
      min.y = min.y, max.y = max.y,
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
    if (is.null(data$stratum) && ! is.null(data$alluvium)) {
      data$stratum <- data$alluvium
    }
    
    # assign unit amounts if not provided
    if (is.null(data$y)) {
      data$y <- rep(1, nrow(data))
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
                           decreasing = ggalluvial_opt("decreasing"),
                           reverse = ggalluvial_opt("reverse"),
                           absolute = ggalluvial_opt("absolute"),
                           discern = FALSE,
                           negate.strata = NULL,
                           aggregate.y = NULL,
                           cement.alluvia = ggalluvial_opt("cement.alluvia"),
                           lode.guidance = ggalluvial_opt("lode.guidance"),
                           lode.ordering = ggalluvial_opt("lode.ordering"),
                           aes.bind = ggalluvial_opt("aes.bind"),
                           infer.label = FALSE,
                           min.y = NULL, max.y = NULL) {
    
    # introduce label
    if (infer.label) {
      if (is.null(data$label)) {
        data$label <- data$alluvium
      } else {
        warning("Aesthetic `label` is specified, ",
                "so parameter `infer.label` will be ignored.")
      }
    }
    
    # aesthetics (in prescribed order)
    aesthetics <- intersect(.color_diff_aesthetics, names(data))
    # match arguments for `aes.bind`
    if (! is.null(aes.bind)) {
      if (is.logical(aes.bind)) {
        aes.bind.rep <- if (aes.bind) "flow" else "none"
        warning("Logical values of `aes.bind` are deprecated; ",
                "replacing ", aes.bind, " with '", aes.bind.rep, "'.")
        aes.bind <- aes.bind.rep
      }
      aes.bind <- match.arg(aes.bind, c("none", "flows", "alluvia"))
    }
    
    # sign variable (sorts positives before negatives)
    data$yneg <- data$y < 0
    
    # cement (aggregate) `y` over otherwise equivalent alluvia
    if (! is.null(aggregate.y)) {
      deprecate_parameter("aggregate.y", "cement.alluvia")
      cement.alluvia <- aggregate.y
    }
    if (is.logical(cement.alluvia)) {
      label_fun <- first
    } else {
      if (is.character(cement.alluvia)) label_fun <- get(cement.alluvia)
      stopifnot(is.function(cement.alluvia))
      label_fun <- cement.alluvia
      cement.alluvia <- TRUE
    }
    if (cement.alluvia) {
      data <- cement_data(data, "x", "alluvium", label_fun)
    }
    
    # define 'deposit' variable to rank strata vertically
    data <- deposit_data(data, decreasing, reverse, absolute)
    
    # if `lode.ordering` not provided, calculate it (ignoring aesthetics)
    if (is.null(lode.ordering)) {
      # invoke surrounding axes in the order prescribed by `lode.guidance`
      if (is.character(lode.guidance)) {
        lode.guidance <- get(paste0("lode_", lode.guidance))
      }
      stopifnot(is.function(lode.guidance))
      # summary data of alluvial deposits
      alluv_dep <- alluviate(data, "x", "deposit", "alluvium")
      # axis indices
      alluv_x <- setdiff(names(alluv_dep), "alluvium")
      # calculate `lode.ordering` from `lode.guidance`
      lode.ordering <- matrix(NA_integer_,
                              nrow = nrow(alluv_dep), ncol = length(alluv_x))
      dimnames(lode.ordering) <- list(alluv_dep$alluvium, alluv_x)
      for (xx in alluv_x) {
        ord_x <- lode.guidance(length(alluv_x), match(xx, alluv_x))
        # order by aesthetics in order
        lode.ordering[, xx] <- interaction(alluv_dep[, alluv_x[rev(ord_x)]],
                                           drop = TRUE)
      }
      # bind a vector to itself to create a matrix
    } else if (is.vector(lode.ordering)) {
      lode.ordering <- matrix(lode.ordering,
                              nrow = length(lode.ordering),
                              ncol = length(unique(data$x)))
    }
    # check that array has correct dimensions
    stopifnot(dim(lode.ordering) ==
                c(length(unique(data$alluvium)), length(unique(data$x))))
    
    # convert `lode.ordering` into a single sorting variable 'rem_deposit'
    # that orders index lodes by remaining / remote deposits
    lode_ord <- as.data.frame(lode.ordering)
    names(lode_ord) <- sort(unique(data$x))
    lode_ord$alluvium <- if (is.null(rownames(lode.ordering))) {
      if (is.factor(data$alluvium)) {
        levels(data$alluvium)
      } else if (is.numeric(data$alluvium)) {
        sort(unique(data$alluvium))
      } else {
        unique(data$alluvium)
      }
    } else {
      rownames(lode.ordering)
    }
    lode_ord <- tidyr::gather(lode_ord,
                              key = "x", value = "rem_deposit",
                              as.character(sort(unique(data$x))))
    data <- merge(data, lode_ord, by = c("x", "alluvium"),
                  all.x = TRUE, all.y = FALSE)
    
    # identify fissures at aesthetics that vary within strata
    n_lodes <- nrow(unique(data[, c("x", "stratum")]))
    fissure_aes <- aesthetics[which(sapply(aesthetics, function(x) {
      nrow(unique(data[, c("x", "stratum", x)]))
    }) > n_lodes)]
    data$fissure <- if (length(fissure_aes) == 0) {
      1
    } else {
      # order by aesthetics in order
      as.integer(interaction(data[, rev(fissure_aes)], drop = TRUE)) *
        (-1) ^ (data$yneg * absolute + reverse)
    }
    
    # reverse alluvium order among negative observations
    data$fan <- xtfrm(data$alluvium) * (-1) ^ (data$yneg * absolute + reverse)
    
    # sort data in preparation for `y` sums
    sort_fields <- c(
      "x",
      "deposit",
      if (aes.bind == "alluvia") "fissure",
      "rem_deposit",
      if (aes.bind == "flows") "fissure",
      "fan"
    )
    data <- data[do.call(order, data[, sort_fields]), , drop = FALSE]
    # calculate `y` sums
    data$ycum <- NA
    for (xx in unique(data$x)) {
      for (yn in c(FALSE, TRUE)) {
        ww <- which(data$x == xx & data$yneg == yn)
        data$ycum[ww] <- cumulate(data$y[ww])
      }
    }
    # calculate y bounds
    data$deposit <- NULL
    data$rem_deposit <- NULL
    data$fissure <- NULL
    data$ymin <- data$ycum - abs(data$y) / 2
    data$ymax <- data$ycum + abs(data$y) / 2
    data$y <- data$ycum
    data$yneg <- NULL
    data$ycum <- NULL
    
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
    if (! is.null(min.y)) data <- subset(data, ymax - ymin >= min.y)
    if (! is.null(max.y)) data <- subset(data, ymax - ymin <= max.y)
    
    # arrange data by aesthetics for consistent (reverse) z-ordering
    data <- z_order_aes(data, aesthetics)
    
    data
  }
)

# aggregate `y` and `label` over equivalent alluvia (omitting missing values)
cement_data <- function(data, key, id, fun) {
  
  agg_vars <- intersect(c("y", "label"), names(data))
  
  # interaction of all variables to aggregate over (without dropping NAs)
  data$binding <- as.numeric(interaction(lapply(
    data[, -match(c(key, id, agg_vars), names(data)), drop = FALSE],
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
    alluv_data[, -match(id, names(alluv_data)), drop = FALSE], drop = TRUE
  )))
  # transform `id` in `data` accordingly
  data[[id]] <- alluv_agg[match(data[[id]], alluv_orig)]
  
  # aggregate `y` and `label` by all other variables
  by_vars <- c(key, id, "binding")
  data_agg <- dplyr::group_by(data[, c(by_vars, agg_vars)], .dots = by_vars)
  data_agg <- if ("label" %in% agg_vars) {
    dplyr::summarize(data_agg,
                     y = sum(.data$y, na.rm = TRUE), label = fun(.data$label))
  } else {
    #dplyr::summarize(data_agg, y = sum(.data$y, na.rm = TRUE))
    dplyr::summarize_at(data_agg, "y", sum, na.rm = TRUE)
  }
  data_agg <- dplyr::ungroup(data_agg)
  # merge into `data`, ensuring that no `key`-`id` pairs are duplicated
  data <- unique(merge(
    data_agg,
    data[, -match(agg_vars, names(data))],
    all.x = TRUE, all.y = FALSE
  ))
  data$binding <- NULL
  
  data
}

# aggregate weights over otherwise equivalent alluvia (omitting missing values)
cement_data_alt <- function(data, key, id) {
  
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
  
  # aggregate `y` by `key`, `id`, and .binding
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

# build alluvial dataset for reference during lode-ordering
alluviate <- function(data, key, value, id) {
  to_alluvia_form(
    data[, c(key, value, id)],
    key = key, value = value, id = id
  )
}
