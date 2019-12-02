#' Flow positions
#'
#' Given a dataset with alluvial structure, `stat_flow` calculates the centroids
#' (`x` and `y`) and heights (`ymin` and `ymax`) of the flows between each pair
#' of adjacent axes.
#' @template stat-aesthetics
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
                      decreasing = ggalluvial_opt("decreasing"),
                      reverse = ggalluvial_opt("reverse"),
                      absolute = ggalluvial_opt("absolute"),
                      discern = FALSE,
                      negate.strata = NULL,
                      aes.bind = ggalluvial_opt("aes.bind"),
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
  
  compute_panel = function(self, data, scales,
                           decreasing = ggalluvial_opt("decreasing"),
                           reverse = ggalluvial_opt("reverse"),
                           absolute = ggalluvial_opt("absolute"),
                           discern = FALSE,
                           negate.strata = NULL,
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
      if (aes.bind == "alluvia") {
        warning("`aes.bind = 'alluvia'` only available for `geom_alluvium()`; ",
                "changing to 'flows'.")
      }
    }
    
    # sign variable (sorts positives before negatives)
    data$yneg <- data$y < 0
    
    # define 'deposit' variable to rank strata vertically
    data <- deposit_data(data, decreasing, reverse, absolute)
    
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
    
    # stack contacts of flows to strata, using 'alluvium' to link them
    # -+- why is 'x' necessarily continuous? -+-
    x_ran <- range(data$x)
    data$alluvium <- contiguate(data$alluvium)
    alluvium_max <- max(data$alluvium)
    data <- rbind(
      transform(data[data$x != x_ran[2], , drop = FALSE],
                alluvium = alluvium + alluvium_max * (as.numeric(x) - 1),
                link = as.numeric(x),
                contact = I("back")),
      transform(data[data$x != x_ran[1], , drop = FALSE],
                alluvium = alluvium + alluvium_max * (as.numeric(x) - 2),
                link = as.numeric(x) - 1,
                contact = I("front"))
    )
    data$contact <- factor(data$contact, levels = c("back", "front"))
    
    # flag flows between common pairs of strata and of aesthetics
    # (induces NAs for one-sided flows)
    vars <- c("deposit", "fissure")
    adj_vars <- paste0("adj_", vars)
    # interactions of link:back:front
    for (i in seq(vars)) {
      data <- match_contacts(data, vars[i], adj_vars[i])
      #data[[adj_vars[i]]] <- xtfrm(data[[adj_vars[i]]])
    }
    # designate these flow pairings the alluvia
    data$alluvium <- as.integer(interaction(data[, adj_vars], drop = TRUE))
    
    # sum `y` and, if numeric, `label` over `x`, `yneg`, and `stratum`
    sum_vars <- c("y", if (is.numeric(data$label)) "label")
    # exclude `group` because it will be redefined below
    data$group <- NULL
    by_vars <- setdiff(names(data), c("group", sum_vars))
    # keep NAs in order to correctly position flows
    data <- dplyr::summarize_at(dplyr::group_by(data, .dots = by_vars),
                                sum_vars, sum, na.rm = TRUE)
    # redefine `group` to be used to control grobs in the geom step
    data$group <- data$alluvium
    
    # sort data in preparation for `y` sums
    sort_fields <- c(
      "link", "x",
      "deposit",
      if (aes.bind == "flows") "adj_fissure",
      "adj_deposit",
      "alluvium", "contact"
    )
    data <- data[do.call(order, data[, sort_fields]), , drop = FALSE]
    # calculate `y` sums
    data$ycum <- NA
    for (ll in unique(data$link)) {
      for (ss in unique(data$contact)) {
        for (yn in c(FALSE, TRUE)) {
          ww <- which(data$link == ll & data$contact == ss & data$yneg == yn)
          data$ycum[ww] <- cumulate(data$y[ww])
        }
      }
    }
    # calculate y bounds
    data$deposit <- NULL
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
    data <- z_order_aes(data, aesthetics)
    
    data
  }
)

match_contacts <- function(data, var, var_col) {
  adj <- tidyr::spread_(data[, c("alluvium", "link", "contact", var)],
                        key = "contact", value = var)
  adj[[var_col]] <- interaction(adj$link, adj$back, adj$front, drop = TRUE)
  merge(data,
        adj[, c("alluvium", var_col)],
        by = "alluvium", all.x = TRUE, all.y = FALSE)
}
