#' Alluvial positions
#'
#' Given a dataset with alluvial structure, `stat_alluvium` calculates the
#' centroids (`x` and `y`) and heights (`ymin` and `ymax`) of the lodes, the
#' intersections of the alluvia with the strata. It leverages the `group`
#' aesthetic for plotting purposes (for now).
#' @template stat-aesthetics
#' @template computed-variables
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
#' @param aggregate.y Deprecated alias for `cement.alluvia`.
#' @param lode.guidance The function to prioritize the axis variables for
#'   ordering the lodes within each stratum, or else a character string
#'   identifying the function. Character options are "zigzag", "frontback",
#'   "backfront", "forward", and "backward" (see [`lode-guidance-functions`]).
#' @param lode.ordering **Deprecated in favor of the `order` aesthetic.** A list
#'   (of length the number of axes) of integer vectors (each of length the
#'   number of rows of `data`) or NULL entries (indicating no imposed ordering),
#'   or else a numeric matrix of corresponding dimensions, giving the preferred
#'   ordering of alluvia at each axis. This will be used to order the lodes
#'   within each stratum by sorting the lodes first by stratum, then by the
#'   provided vectors, and lastly by remaining factors (if the vectors contain
#'   duplicate entries and therefore do not completely determine the lode
#'   orderings).
#' @example inst/examples/ex-stat-alluvium.r
#' @export
stat_alluvium <- function(mapping = NULL,
                          data = NULL,
                          geom = "alluvium",
                          position = "identity",
                          decreasing = NULL,
                          reverse = NULL,
                          absolute = NULL,
                          discern = FALSE,
                          negate.strata = NULL,
                          aggregate.y = NULL,
                          cement.alluvia = NULL,
                          lode.guidance = NULL,
                          lode.ordering = NULL,
                          aes.bind = NULL,
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
  
  # `<new-aes> = NULL` prevents "unknown aesthetics" warnings
  default_aes = aes(weight = 1, stratum = NULL, alluvium = NULL),
  
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
    
    # remove null parameter values (see #103)
    params[vapply(params, is.null, NA)] <- NULL
    
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
    } else {
      data <- remove_missing(
        data, na.rm = params$na.rm,
        vars = "y", name = "stat_alluvium",
        finite = TRUE
      )
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
                           decreasing = NULL,
                           reverse = NULL,
                           absolute = NULL,
                           discern = FALSE, distill = "first",
                           negate.strata = NULL,
                           aggregate.y = NULL,
                           cement.alluvia = NULL,
                           lode.guidance = NULL,
                           lode.ordering = NULL,
                           aes.bind = NULL,
                           infer.label = FALSE,
                           min.y = NULL, max.y = NULL) {
    
    # parameter defaults
    if (is.null(decreasing)) decreasing <- ggalluvial_opt("decreasing")
    if (is.null(reverse)) reverse <- ggalluvial_opt("reverse")
    if (is.null(absolute)) absolute <- ggalluvial_opt("absolute")
    if (is.null(cement.alluvia)) cement.alluvia <- ggalluvial_opt("cement.alluvia")
    if (is.null(lode.guidance)) lode.guidance <- ggalluvial_opt("lode.guidance")
    if (is.null(aes.bind)) aes.bind <- ggalluvial_opt("aes.bind")
    
    # introduce label
    if (infer.label) {
      deprecate_parameter("infer.label",
                          msg = "Use `aes(label = after_stat(lode))`.")
      if (is.null(data$label)) {
        data$label <- data$alluvium
      } else {
        warning("Aesthetic `label` is specified, ",
                "so parameter `infer.label` will be ignored.")
      }
    }
    
    # ensure that `lode.ordering` is a matrix with column names
    if (! is.null(lode.ordering)) {
      deprecate_parameter("lode.ordering",
                          msg = "Use the `order` aesthetic instead.")
      if (is.null(data$order)) {
        # bind a vector to itself to create a matrix
        if (is.vector(lode.ordering)) {
          lode.ordering <- matrix(lode.ordering,
                                  nrow = length(lode.ordering),
                                  ncol = length(unique(data$x)))
        }
        # flatten `lode.ordering` into an 'order' column
        data$order <- as.vector(lode.ordering)
      } else {
        warning("Aesthetic `order` is specified, ",
                "so parameter `lode.ordering` will be ignored.")
      }
    }
    
    # differentiation aesthetics (in prescribed order)
    diff_aes <- intersect(c(.color_diff_aesthetics, .text_aesthetics),
                          names(data))
    # match arguments for `aes.bind`
    if (! is.null(aes.bind)) {
      if (is.logical(aes.bind)) {
        aes.bind.rep <- if (aes.bind) "flows" else "none"
        warning("Logical values of `aes.bind` are deprecated; ",
                "replacing ", aes.bind, " with '", aes.bind.rep, "'.")
        aes.bind <- aes.bind.rep
      }
      aes.bind <- match.arg(aes.bind, c("none", "flows", "alluvia"))
    }
    
    # sign variable (sorts positives before negatives)
    data$yneg <- data$y < 0
    # lode variable (before co-opting 'alluvium')
    data$lode <- data$alluvium
    # specify distillation function from `distill`
    distill <- distill_fun(distill)
    
    # initiate variables for `after_stat()`
    weight <- data$weight
    data$weight <- NULL
    if (is.null(weight)) weight <- 1
    data$n <- weight
    data$count <- data$y * weight
    # transform 'order' according to `absolute` and `reverse` params
    if (! is.null(data$order)) data$order <- xtfrm(data$order) *
      (-1) ^ (data$yneg * absolute + reverse)
    
    # cement (aggregate) `y` over otherwise equivalent alluvia
    if (! is.null(aggregate.y)) {
      deprecate_parameter("aggregate.y", "cement.alluvia")
      cement.alluvia <- aggregate.y
    }
    if (cement.alluvia) {
      
      # -+- need to stop depending on 'group' and 'PANEL' -+-
      only_vars <- intersect(c(diff_aes, "group", "PANEL"), names(data))
      bind_vars <- intersect(c("yneg", "stratum", only_vars), names(data))
      sum_vars <- c("y", "n", "count")
      
      # interaction of all variables to aggregate over (without dropping NAs)
      # -+- need to stop depending on 'group' -+-
      data$binding <- as.numeric(interaction(lapply(
        data[, bind_vars, drop = FALSE],
        addNA, ifany = FALSE
      ), drop = TRUE))
      # convert to alluvia format with 'binding' entries
      luv_dat <- alluviate(data, "x", "binding", "alluvium")
      # sort by all axes (everything except 'alluvium')
      luv_dat <- luv_dat[do.call(
        order,
        luv_dat[, setdiff(names(luv_dat), "alluvium"), drop = FALSE]
      ), , drop = FALSE]
      
      # define map from original to aggregated 'alluvium' column
      luv_orig <- luv_dat$alluvium
      luv_agg <- cumsum(! duplicated(interaction(
        luv_dat[, setdiff(names(luv_dat), "alluvium"), drop = FALSE],
        drop = TRUE
      )))
      # transform 'alluvium' in `data` accordingly
      data$alluvium <- luv_agg[match(data$alluvium, luv_orig)]
      
      # aggregate variables over 'x', 'yneg', and 'stratum':
      # sum of computed variables and unique-or-bust values of aesthetics
      by_vars <- c("x", "yneg", "stratum", "alluvium", "binding")
      agg_lode <- stats::aggregate(data[, "lode", drop = FALSE],
                                   data[, by_vars],
                                   distill)
      if (length(only_vars) > 0) {
        agg_only <- stats::aggregate(data[, only_vars, drop = FALSE],
                                     data[, by_vars],
                                     only)
      }
      agg_dat <- stats::aggregate(data[, sum_vars],
                                  data[, by_vars],
                                  sum)
      agg_dat <- merge(agg_dat, agg_lode)
      if (length(only_vars) > 0) {
        agg_dat <- merge(agg_dat, agg_only)
      }
      
      # merge into `data`, ensuring that no `key`-`id` pairs are duplicated
      data <- unique(merge(
        agg_dat,
        data[, setdiff(names(data), sum_vars)],
        all.x = TRUE, all.y = FALSE
      ))
      data$binding <- NULL
      
    }
    
    # define 'deposit' variable to rank strata vertically
    data <- deposit_data(data, decreasing, reverse, absolute)
    
    # ensure that `lode.guidance` is a function
    if (is.character(lode.guidance)) {
      lode.guidance <- get(paste0("lode_", lode.guidance))
    }
    stopifnot(is.function(lode.guidance))
    # invoke surrounding axes in the order prescribed by `lode.guidance`
    lode_ord <- guide_lodes(data, lode.guidance)
    
    # convert `lode_ord` into a single sorting variable 'rem_deposit'
    # that orders index lodes by remaining / remote deposits
    names(lode_ord) <- sort(unique(data$x))
    lode_ord$alluvium <- if (is.null(rownames(lode_ord))) {
      if (is.factor(data$alluvium)) {
        levels(data$alluvium)
      } else if (is.numeric(data$alluvium)) {
        sort(unique(data$alluvium))
      } else {
        unique(data$alluvium)
      }
    } else {
      rownames(lode_ord)
    }
    # match `lode_ord$x` back to `data$x`
    uniq_x <- sort(unique(data$x))
    lode_ord <- tidyr::gather(lode_ord,
                              key = "x", value = "rem_deposit",
                              as.character(uniq_x))
    match_x <- match(lode_ord$x, as.character(uniq_x))
    lode_ord$x <- uniq_x[match_x]
    # merge `lode_ord` back into `data`
    data <- merge(data, lode_ord, by = c("x", "alluvium"),
                  all.x = TRUE, all.y = FALSE)
    
    # identify fissures at aesthetics that vary within strata
    n_lodes <- nrow(unique(data[, c("x", "stratum")]))
    fissure_aes <- diff_aes[which(sapply(diff_aes, function(x) {
      nrow(unique(data[, c("x", "stratum", x)]))
    }) > n_lodes)]
    data$fissure <- if (length(fissure_aes) == 0) {
      1
    } else {
      # order by aesthetics in order
      as.integer(interaction(data[, rev(fissure_aes)], drop = TRUE)) *
        (-1) ^ (data$yneg * absolute + reverse)
    }
    
    # calculate variables for `after_stat()`
    x_sums <- tapply(abs(data$count), data$x, sum, na.rm = TRUE)
    data$prop <- data$count / x_sums[match(as.character(data$x), names(x_sums))]
    
    # reverse alluvium order
    data$fan <- xtfrm(data$alluvium) * (-1) ^ reverse
    
    # sort data in preparation for `y` sums
    sort_fields <- c(
      "x",
      "deposit",
      if (! is.null(data$order)) "order",
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
    data$rem_deposit <- NULL
    data$order <- NULL
    data$fissure <- NULL
    data$fan <- NULL
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
    # add 'group' to group contiguous alluvial subsets
    data <- transform(data, group = as.numeric(interaction(alluvium, flow)))
    # remove unused fields
    data$cont <- NULL
    data$axis <- NULL
    data$flow <- NULL
    
    # impose height restrictions
    if (! is.null(min.y)) data <- subset(data, ymax - ymin >= min.y)
    if (! is.null(max.y)) data <- subset(data, ymax - ymin <= max.y)
    
    # arrange data by aesthetics for consistent (reverse) z-ordering
    data <- z_order_aes(data, diff_aes)
    
    data
  }
)

# apply lode guidance function to produce ordering matrix
guide_lodes <- function(data, guidance_fun) {
  
  # summary data of alluvial deposits
  alluv_dep <- alluviate(data, "x", "deposit", "alluvium")
  # axis indices
  alluv_x <- setdiff(names(alluv_dep), "alluvium")
  
  # initialize ordering matrix
  ord_mat <- matrix(NA_integer_,
                    nrow = nrow(alluv_dep), ncol = length(alluv_x))
  dimnames(ord_mat) <- list(alluv_dep$alluvium, alluv_x)
  
  # calculate orderings from `guidance_fun`
  for (xx in alluv_x) {
    ii <- match(xx, alluv_x)
    ord_x <- guidance_fun(length(alluv_x), match(xx, alluv_x))
    # order by prescribed ordering and by aesthetics in order
    ord_mat[, xx] <- interaction(alluv_dep[, alluv_x[rev(ord_x)]],
                                   drop = TRUE)
  }
  
  # check that array has correct dimensions
  stopifnot(dim(ord_mat) ==
              c(length(unique(data$alluvium)), length(unique(data$x))))
  
  # return ordering matrix as a data aframe
  as.data.frame(ord_mat)
}

# build alluvial dataset for reference during lode-ordering
alluviate <- function(data, key, value, id) {
  to_alluvia_form(
    data[, c(key, value, id), drop = FALSE],
    key = key, value = value, id = id
  )
}
