#' Flow positions
#'
#' Given a dataset with alluvial structure, `stat_flow` calculates the centroids
#' (`x` and `y`) and heights (`ymin` and `ymax`) of the flows between each pair
#' of adjacent axes.
#' @template stat-aesthetics
#' @template computed-variables
#' @template order-options
#' @template defunct-stat-params
#'

#' @import ggplot2
#' @family alluvial stat layers
#' @seealso [ggplot2::layer()] for additional arguments and
#'   [geom_alluvium()] and
#'   [geom_flow()] for the corresponding geoms.
#' @inheritParams stat_stratum
#' @param aes.bind At what grouping level, if any, to prioritize differentiation
#'   aesthetics when ordering the lodes within each stratum. Defaults to
#'   `"none"` (no aesthetic binding) with intermediate option `"flows"` to bind
#'   aesthetics after stratifying by axes linked to the index axis (the one
#'   adjacent axis in `stat_flow()`; all remaining axes in `stat_alluvium()`)
#'   and strongest option `"alluvia"` to bind aesthetics after stratifying by
#'   the index axis but before stratifying by linked axes (only available for
#'   `stat_alluvium()`). Stratification by any axis is done with respect to the
#'   strata at that axis, after separating positive and negative strata,
#'   consistent with the values of `decreasing`, `reverse`, and `absolute`.
#'   Thus, if `"none"`, then lode orderings will not depend on aesthetic
#'   variables. All aesthetic variables are used, in the order in which they are
#'   specified in `aes()`.
#' @example inst/examples/ex-stat-flow.r
#' @export
stat_flow <- function(mapping = NULL,
                      data = NULL,
                      geom = "flow",
                      position = "identity",
                      decreasing = NULL,
                      reverse = NULL,
                      absolute = NULL,
                      discern = FALSE,
                      negate.strata = NULL,
                      aes.bind = NULL,
                      infer.label = FALSE,
                      min.y = NULL, max.y = NULL,
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
      reverse = reverse,
      absolute = absolute,
      discern = discern,
      negate.strata = negate.strata,
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
StatFlow <- ggproto(
  "StatFlow", Stat,
  
  required_aes = c("x"),
  optional_aes = c("order"),
  
  # `<new-aes> = NULL` prevents "unknown aesthetics" warnings
  default_aes = aes(weight = 1, stratum = NULL, alluvium = NULL, order = NULL),
  
  setup_params = function(data, params) {
    
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
        vars = "y", name = "stat_flow",
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
  
  compute_panel = function(self, data, scales,
                           decreasing = NULL,
                           reverse = NULL,
                           absolute = NULL,
                           discern = FALSE, distill = "first",
                           negate.strata = NULL,
                           aes.bind = NULL,
                           infer.label = FALSE,
                           min.y = NULL, max.y = NULL) {
    
    # parameter defaults
    if (is.null(decreasing)) decreasing <- ggalluvial_opt("decreasing")
    if (is.null(reverse)) reverse <- ggalluvial_opt("reverse")
    if (is.null(absolute)) absolute <- ggalluvial_opt("absolute")
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
    
    # differentiation and text aesthetics (in prescribed order)
    diff_aes <- intersect(c(.color_diff_aesthetics, .text_aesthetics),
                          names(data))
    # match arguments for `aes.bind`
    if (! is.null(aes.bind)) {
      if (is.logical(aes.bind)) {
        aes.bind.rep <- if (aes.bind) "flow" else "none"
        warning("Logical values of `aes.bind` are deprecated; ",
                "replacing ", aes.bind, " with '", aes.bind.rep, "'.")
        aes.bind <- aes.bind.rep
      }
      aes.bind <- match.arg(aes.bind, c("none", "flows", "alluvia"))
      if (aes.bind == "alluvia") {
        warning("`aes.bind = 'alluvia'` only available for `geom_alluvium()`; ",
                "changing to 'flows'.")
        aes.bind <- "flows"
      }
    }
    
    # sign variable (sorts positives before negatives)
    data$yneg <- data$y < 0
    # lode variable (before co-opting 'alluvium')
    data$lode <- data$alluvium
    # specify distillation function from `distill`
    distill <- distill_fun(distill)
    # transform 'order' according to `absolute` and `reverse` params
    if (! is.null(data$order)) data$order <- xtfrm(data$order) *
      (-1) ^ (data$yneg * absolute + reverse)
    
    # define 'deposit' variable to rank strata vertically
    data <- deposit_data(data, decreasing, reverse, absolute)
    
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
    
    # stack positions of flows to strata, using 'alluvium' to link them
    # (does not assume that 'x' is continuous or regularly-spaced)
    ran_x <- range(data$x)
    uniq_x <- sort(unique(data$x))
    # ensure that 'alluvium' ranges simply from 1 to max
    data$alluvium <- contiguate(data$alluvium)
    alluvium_max <- max(data$alluvium)
    data <- rbind(
      transform(data[data$x != ran_x[2], , drop = FALSE],
                alluvium = alluvium +
                  alluvium_max *
                  (match(as.character(x), as.character(uniq_x)) - 1),
                link = match(as.character(x), as.character(uniq_x)),
                flow = factor("from", levels = c("from", "to"))),
      transform(data[data$x != ran_x[1], , drop = FALSE],
                alluvium = alluvium +
                  alluvium_max *
                  (match(as.character(x), as.character(uniq_x)) - 2),
                link = match(as.character(x), as.character(uniq_x)) - 1,
                flow = factor("to", levels = c("from", "to")))
    )
    
    # flag flows between common pairs of strata and of aesthetics
    # (induces NAs for one-sided flows)
    lnk_vars <- intersect(c("deposit", "order", "fissure"), names(data))
    adj_vars <- paste0("adj_", lnk_vars)
    # interactions of link:from:to
    for (i in seq(lnk_vars)) {
      data <- match_flows(data, lnk_vars[[i]], adj_vars[[i]])
      #data[[adj_vars[i]]] <- xtfrm(data[[adj_vars[i]]])
    }
    # designate these flow pairings the alluvia
    data$alluvium <- as.integer(interaction(data[, adj_vars], drop = TRUE))
    
    # initiate variables for `after_stat()`
    weight <- data$weight
    data$weight <- NULL
    if (is.null(weight)) weight <- 1
    data$n <- weight
    data$count <- data$y * weight
    
    # aggregate variables over 'alluvium', 'x', 'yneg', and 'stratum':
    # sum of computed variables and unique-or-bust values of aesthetics
    by_vars <- intersect(c("alluvium", "x", "yneg", "stratum",
                           "deposit", "order", "fissure", "link", "flow",
                           "adj_deposit", "adj_order", "adj_fissure"),
                         names(data))
    only_vars <- c(diff_aes)
    sum_vars <- c("y", "n", "count")
    data <- dplyr::group_by(data, dplyr::across(by_vars))
    # keep `NA`s in order to correctly position flows:
    # `distill()`, `only()`, and `sum(na.rm = TRUE)`
    agg_lode <- dplyr::summarize_at(data, "lode", distill)
    if (length(only_vars) > 0) {
      agg_only <- dplyr::summarize_at(data, only_vars, only)
    }
    data <- dplyr::summarize_at(data, sum_vars, sum, na.rm = TRUE)
    data <- dplyr::ungroup(data)
    # merges forget tibble classes
    data <- merge(data, agg_lode)
    if (length(only_vars) > 0) {
      data <- merge(data, agg_only)
    }
    
    # redefine 'group' to be used to control grobs in the geom step
    data$group <- data$alluvium
    
    # calculate variables for `after_stat()`
    x_sums <- tapply(abs(data$count), data$x, sum, na.rm = TRUE)
    data$prop <- data$count / x_sums[match(as.character(data$x), names(x_sums))]
    
    # sort data in preparation for `y` sums
    sort_fields <- c(
      "link", "x",
      "deposit",
      if (! is.null(data$order)) "order",
      #if (aes.bind != "none") "fissure",
      if (aes.bind == "flows") "adj_fissure",
      "adj_deposit",
      "alluvium", "flow"
    )
    data <- data[do.call(order, data[, sort_fields]), , drop = FALSE]
    # calculate `y` sums
    data$ycum <- NA
    for (ll in unique(data$link)) {
      for (ss in unique(data$flow)) {
        for (yn in c(FALSE, TRUE)) {
          ww <- which(data$link == ll & data$flow == ss & data$yneg == yn)
          data$ycum[ww] <- cumulate(data$y[ww])
        }
      }
    }
    # calculate y bounds
    data$fissure <- NULL
    data$adj_deposit <- NULL
    data$adj_fissure <- NULL
    data$link <- NULL
    data$ymin <- data$ycum - abs(data$y) / 2
    data$ymax <- data$ycum + abs(data$y) / 2
    data$y <- data$ycum
    data$yneg <- NULL
    data$ycum <- NULL
    
    # impose height restrictions
    if (! is.null(min.y)) data <- subset(data, ymax - ymin >= min.y)
    if (! is.null(max.y)) data <- subset(data, ymax - ymin <= max.y)
    
    # arrange data by aesthetics for consistent (reverse) z-ordering
    data <- z_order_aes(data, diff_aes)
    
    data
  }
)

match_flows <- function(data, var, var_col) {
  adj <- tidyr::spread(data[, c("alluvium", "link", "flow", var)],
                       key = "flow", value = var)
  adj[[var_col]] <- interaction(adj$link, adj$from, adj$to, drop = TRUE)
  merge(data,
        adj[, c("alluvium", var_col)],
        by = "alluvium", all.x = TRUE, all.y = FALSE)
}
