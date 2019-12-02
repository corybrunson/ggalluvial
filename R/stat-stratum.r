#' Stratum positions
#'
#' Given a dataset with alluvial structure, `stat_stratum` calculates the
#' centroids (`x` and `y`) and heights (`ymin` and `ymax`) of the strata at each
#' axis.
#' @template stat-aesthetics
#' @template order-options
#' @template defunct-stat-params
#'

#' @import ggplot2
#' @family alluvial stat layers
#' @seealso [ggplot2::layer()] for additional arguments and
#'   [geom_stratum()] for the corresponding geom.
#' @inheritParams ggplot2::layer
#' @template layer-params
#' @param geom The geometric object to use display the data;
#'    override the default.
#' @param decreasing Logical; whether to arrange the strata at each axis
#'   in the order of the variable values (`NA`, the default),
#'   in ascending order of totals (largest on top, `FALSE`), or
#'   in descending order of totals (largest on bottom, `TRUE`).
#' @param reverse Logical; if `decreasing` is `NA`,
#'   whether to arrange the strata at each axis
#'   in the reverse order of the variable values,
#'   so that they match the order of the values in the legend.
#'   Ignored if `decreasing` is not `NA`.
#'   Defaults to `TRUE`.
#' @param absolute Logical; if some cases or strata are negative,
#'   whether to arrange them (respecting `decreasing` and `reverse`)
#'   using negative or absolute values of `y`.
#' @param discern Passed to [to_lodes_form()] if `data` is in
#'   alluvia format.
#' @param negate.strata A vector of values of the `stratum` aesthetic to be
#'   treated as negative (will ignore missing values with a warning).
#' @param infer.label Logical; whether to assign the `stratum` or `alluvium`
#'   variable to the `label` aesthetic. Defaults to `FALSE`, and requires that
#'   no `label` aesthetic is assigned. This parameter is intended only for uses
#'   in which the data are in alluva form and are therefore converted to lode
#'   form before the statistical transformation.
#' @param label.strata Deprecated; alias for `infer.label`.
#' @param min.y,max.y Numeric; bounds on the heights of the strata to be
#'   rendered. Use these bounds to exclude strata outside a certain range, for
#'   example when labeling strata using [ggplot2::geom_text()].
#' @param min.height,max.height Deprecated aliases for `min.y` and `max.y`.
#' @example inst/examples/ex-stat-stratum.r
#' @export
stat_stratum <- function(mapping = NULL,
                         data = NULL,
                         geom = "stratum",
                         position = "identity",
                         decreasing = ggalluvial_opt("decreasing"),
                         reverse = ggalluvial_opt("reverse"),
                         absolute = ggalluvial_opt("absolute"),
                         discern = FALSE,
                         negate.strata = NULL,
                         infer.label = FALSE, label.strata = NULL,
                         min.y = NULL, max.y = NULL,
                         min.height = NULL, max.height = NULL,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE,
                         ...) {
  layer(
    stat = StatStratum,
    mapping = mapping,
    data = data,
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
      infer.label = infer.label, label.strata = label.strata,
      min.y = min.y, max.y = max.y,
      min.height = min.height, max.height = max.height,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggalluvial-ggproto
#' @usage NULL
#' @export
StatStratum <- ggproto(
  "StatStratum", Stat,
  
  required_aes = c("x"),
  
  setup_data = function(data, params) {
    
    # if `alluvium` not provided, assign each row its own, grouped by `x`
    if (is.null(data$alluvium) && ! is.null(data$x)) {
      data$alluvium <- NA
      for (xx in unique(data$x)) {
        ww <- which(data$x == xx)
        data$alluvium[ww] <- 1:length(ww)
      }
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
    
    # nullify `group` and `alluvium` fields (to avoid confusion with geoms)
    data$group <- NULL
    data$alluvium <- NULL
    
    data
  },
  
  compute_panel = function(self, data, scales,
                           decreasing = ggalluvial_opt("decreasing"),
                           reverse = ggalluvial_opt("reverse"),
                           absolute = ggalluvial_opt("absolute"),
                           discern = FALSE,
                           negate.strata = NULL,
                           infer.label = FALSE, label.strata = NULL,
                           min.y = NULL, max.y = NULL,
                           min.height = NULL, max.height = NULL) {
    
    # introduce label
    if (! is.null(label.strata)) {
      deprecate_parameter("label.strata", "infer.label")
      infer.label <- label.strata
    }
    if (infer.label) {
      if (is.null(data$label)) {
        data$label <- data$stratum
      } else {
        warning("Aesthetic `label` is specified, ",
                "so parameter `infer.label` will be ignored.")
      }
    }
    
    # aesthetics (in prescribed order)
    aesthetics <- intersect(.color_diff_aesthetics, names(data))
    
    # sign variable (sorts positives before negatives)
    data$yneg <- data$y < 0
    
    # aggregate variables over `x`, `yneg`, and `stratum`:
    # take sums of `y` and, if numeric, of `label`
    # require others hold constant (or else be lost)
    agg_dat <- unique(data[, c("x", "yneg", "stratum")])
    for (var in setdiff(names(data), c("x", "yneg", "stratum"))) {
      agg_var <- stats::aggregate(
        data[[var]],
        by = data[, c("x", "yneg", "stratum")],
        FUN = if (var %in% c(aesthetics, "group", "PANEL")) {
          only
        } else {
          agg_fun(data[[var]])
        }
      )
      names(agg_var)[ncol(agg_var)] <- var
      agg_dat <- merge(agg_dat, agg_var,
                       by = c("x", "yneg", "stratum"), all = TRUE)
    }
    data <- agg_dat
    
    # remove empty lodes (including labels)
    data <- subset(data, y != 0)
    
    # define 'deposit' variable to rank strata vertically
    data <- deposit_data(data, decreasing, reverse, absolute)
    
    # sort data in preparation for `y` sums
    data <- data[with(data, order(deposit)), , drop = FALSE]
    
    # calculate `y` sums
    data$ycum <- NA
    for (xx in unique(data$x)) {
      for (yn in c(FALSE, TRUE)) {
        ww <- which(data$x == xx & data$yneg == yn)
        data$ycum[ww] <- cumulate(data$y[ww])
      }
    }
    # calculate y bounds
    data$ymin <- data$ycum - abs(data$y) / 2
    data$ymax <- data$ycum + abs(data$y) / 2
    data$y <- data$ycum
    data$yneg <- NULL
    data$ycum <- NULL
    
    # impose height restrictions
    if (! is.null(min.height)) {
      deprecate_parameter("min.height", "min.y")
      min.y <- min.height
    }
    if (! is.null(max.height)) {
      deprecate_parameter("max.height", "max.y")
      max.y <- max.height
    }
    if (! is.null(min.y)) data <- subset(data, ymax - ymin >= min.y)
    if (! is.null(max.y)) data <- subset(data, ymax - ymin <= max.y)
    
    data
  }
)

# select aggregation function based on variable type
agg_fun <- function(x) {
  if (is.character(x) | is.factor(x)) {
    function(y) as.character(only(y))
  } else if (is.numeric(x)) {
    sum
  } else {
    function(y) NA
  }
}

# single unique value, or else NA
only <- function(x) {
  uniq <- unique(x)
  if (length(uniq) == 1) {
    uniq
  } else {
    NA
  }
}
