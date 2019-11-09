#' Flow positions
#'
#' Given a dataset with alluvial structure, `stat_flow` calculates the centroids
#' (`x` and `y`) and weights (heights; `ymin` and `ymax`) of alluvial flows
#' between each pair of adjacent axes.
#' @template stat-aesthetics
#'

#' @import ggplot2
#' @family alluvial stat layers
#' @seealso [ggplot2::layer()] for additional arguments and
#'   [geom_alluvium()] and
#'   [geom_flow()] for the corresponding geoms.
#' @inheritParams stat_stratum
#' @param aes.bind Whether to prioritize aesthetics before axes (other than the
#'   index axis) when ordering the lodes within each stratum. Defaults to
#'   `FALSE`.
#' @example inst/examples/ex-stat-flow.r
#' @export
stat_flow <- function(mapping = NULL,
                      data = NULL,
                      geom = "flow",
                      position = "identity",
                      decreasing = NA,
                      reverse = TRUE,
                      absolute = FALSE,
                      discern = FALSE,
                      aes.bind = FALSE,
                      negate.strata = NULL,
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
      aes.bind = aes.bind,
      negate.strata = negate.strata,
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
  
  compute_panel = function(self, data, scales,
                           decreasing = NA, reverse = TRUE, absolute = FALSE,
                           discern = FALSE,
                           aes.bind = FALSE,
                           negate.strata = NULL,
                           min.y = NULL, max.y = NULL) {
    
    # aesthetics (in prescribed order)
    aesthetics <- intersect(.color_diff_aesthetics, names(data))
    
    # sign variable
    # (sorts positives before negatives)
    # (will have no effect if all values are non-negative)
    data$yneg <- data$y < 0
    
    # define 'deposit' variable to rank (signed) strata
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
                  by = c("x", "yneg", "stratum"),
                  all.x = TRUE, all.y = FALSE)
    
    # identify aesthetics that vary within strata (at "fissures")
    n_lodes <- nrow(unique(data[, c("x", "stratum")]))
    fissure_aes <- aesthetics[which(sapply(aesthetics, function(x) {
      nrow(unique(data[, c("x", "stratum", x)]))
    }) > n_lodes)]
    data$fissure <- if (length(fissure_aes) == 0) {
      1
    } else {
      interaction(data[, rev(fissure_aes)], drop = TRUE)
    }
    
    # stack contacts of flows to strata, using `alluvium` to link them
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
    flow_vars <- paste0("flow_", vars)
    # interactions of link:back:front
    for (i in seq(vars)) {
      data <- match_contacts(data, vars[i], flow_vars[i])
      #data[[flow_var]] <- xtfrm(data[[flow_var]])
    }
    # designate these flow pairings the alluvia
    data$alluvium <- as.integer(interaction(data[, flow_vars], drop = TRUE))
    # flag flows between positive and negative strata
    ypn <- stats::aggregate(data$yneg,
                            by = data[, "alluvium", drop = FALSE],
                            FUN = "sum")
    names(ypn)[length(ypn)] <- "yposneg"
    ypn$yposneg <- ypn$yposneg == 1
    data <- merge(data, ypn, by = "alluvium", all.x = TRUE, all.y = FALSE)
    # reverse orders of these flow flags
    for (fv in flow_vars) {
      data[[fv]] <- as.integer(data[[fv]]) * (-1) ^ data$yposneg
    }
    
    # aggregate alluvial segments within flows,
    # totalling `weight` and, if numeric, `label`
    sum_cols <- c("y", if (is.numeric(data$label)) "label")
    group_cols <- setdiff(names(data), c("group", sum_cols))
    data <- dplyr::summarize_at(dplyr::group_by(data, .dots = group_cols),
                                sum_cols, sum, na.rm = TRUE)
    data <- transform(data,
                      group = alluvium)
    
    # sort data in preparation for `y` sums
    sort_fields <- c(
      "link", "x",
      "deposit",
      if (aes.bind) {
        c("flow_fissure", "flow_deposit")
      } else {
        c("flow_deposit", "flow_fissure")
      },
      "alluvium", "contact"
    )
    data <- data[do.call(order, data[, sort_fields]), , drop = FALSE]
    # calculate cumulative weights
    data$ycum <- NA
    for (ll in unique(data$link)) {
      for (ss in unique(data$contact)) {
        for (yn in c(FALSE, TRUE)) {
          ww <- which(data$link == ll & data$contact == ss & data$yneg == yn)
          data$ycum[ww] <- cumsum(data$y[ww]) - data$y[ww] / 2
        }
      }
    }
    # calculate y bounds
    data <- transform(data,
                      deposit = NULL,
                      fissure = NULL,
                      flow_deposit = NULL,
                      flow_fissure = NULL,
                      link = NULL,
                      ymin = ycum - abs(y) / 2,
                      ymax = ycum + abs(y) / 2,
                      y = ycum)
    data$yneg <- NULL
    data$ycum <- NULL
    
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

match_contacts <- function(data, var, var_col) {
  adj <- tidyr::spread_(data[, c("alluvium", "link", "contact", var)],
                        key = "contact", value = var)
  adj[[var_col]] <- interaction(adj$link, adj$back, adj$front, drop = TRUE)
  merge(data,
        adj[, c("alluvium", var_col)],
        by = "alluvium", all.x = TRUE, all.y = FALSE)
}
