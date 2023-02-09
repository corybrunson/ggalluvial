#' Stratum positions
#'
#' Given a dataset with alluvial structure, `stat_stratum` calculates the
#' centroids (`x` and `y`) and heights (`ymin` and `ymax`) of the strata at each
#' axis.
#' @template stat-aesthetics
#' @template computed-variables
#' @template order-options
#' @template defunct-stat-params
#'

#' @import ggplot2
#' @family alluvial stat layers
#' @seealso [ggplot2::layer()] for additional arguments and [geom_stratum()] for
#'   the corresponding geom.
#' @inheritParams ggplot2::layer
#' @template layer-params
#' @param geom The geometric object to use display the data; override the
#'   default.
#' @param decreasing Logical; whether to arrange the strata at each axis in the
#'   order of the variable values (`NA`, the default), in ascending order of
#'   totals (largest on top, `FALSE`), or in descending order of totals (largest
#'   on bottom, `TRUE`).
#' @param reverse Logical; if `decreasing` is `NA`, whether to arrange the
#'   strata at each axis in the reverse order of the variable values, so that
#'   they match the order of the values in the legend. Ignored if `decreasing`
#'   is not `NA`. Defaults to `TRUE`.
#' @param absolute Logical; if some cases or strata are negative, whether to
#'   arrange them (respecting `decreasing` and `reverse`) using negative or
#'   absolute values of `y`.
#' @param discern Passed to [to_lodes_form()] if `data` is in alluvia format.
#' @param distill A function (or its name) to be used to distill alluvium values
#'   to a single lode label, accessible via
#'   [`ggplot2::after_stat()`][ggplot2::aes_eval] (similar to its behavior in
#'   [to_alluvia_form()]). It recognizes three character values: `"first"` (the
#'   default) and `"last"` [as defined][dplyr::nth()] in **dplyr**; and `"most"`
#'   (which returns the first modal value).
#' @param negate.strata A vector of values of the `stratum` aesthetic to be
#'   treated as negative (will ignore missing values with a warning).
#' @param infer.label Logical; whether to assign the `stratum` or `alluvium`
#'   variable to the `label` aesthetic. Defaults to `FALSE`, and requires that
#'   no `label` aesthetic is assigned. This parameter is intended for use only
#'   with data in alluva form, which are converted to lode form before the
#'   statistical transformation. Deprecated; use
#'   [`ggplot2::after_stat()`][ggplot2::aes_eval] instead.
#' @param label.strata Defunct; alias for `infer.label`.
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
                         decreasing = NULL,
                         reverse = NULL,
                         absolute = NULL,
                         discern = FALSE, distill = "first",
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
      discern = discern, distill = distill,
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
  
  # `<new-aes> = NULL` prevents "unknown aesthetics" warnings
  default_aes = aes(weight = 1, stratum = NULL, alluvium = NULL),
  
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
    } else {
      data <- remove_missing(
        data, na.rm = params$na.rm,
        vars = "y", name = "stat_stratum",
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
    
    # nullify `group` and `alluvium` fields (to avoid confusion with geoms)
    data$group <- NULL
    #data$alluvium <- NULL
    
    data
  },
  
  compute_panel = function(self, data, scales,
                           decreasing = NULL,
                           reverse = NULL,
                           absolute = NULL,
                           discern = FALSE, distill = "first",
                           negate.strata = NULL,
                           infer.label = FALSE, label.strata = NULL,
                           min.y = NULL, max.y = NULL,
                           min.height = NULL, max.height = NULL) {
    
    # parameter defaults
    if (is.null(decreasing)) decreasing <- ggalluvial_opt("decreasing")
    if (is.null(reverse)) reverse <- ggalluvial_opt("reverse")
    if (is.null(absolute)) absolute <- ggalluvial_opt("absolute")
    
    # introduce label
    if (! is.null(label.strata)) {
      defunct_parameter("label.strata",
                        msg = "use `aes(label = after_stat(stratum))`.")
      infer.label <- label.strata
    }
    if (infer.label) {
      deprecate_parameter("infer.label",
                          msg = "Use `aes(label = after_stat(stratum))`.")
      if (is.null(data$label)) {
        data$label <- data$stratum
      } else {
        warning("Aesthetic `label` is specified, ",
                "so parameter `infer.label` will be ignored.")
      }
    }
    
    # differentiation aesthetics (in prescribed order)
    diff_aes <- intersect(c(.color_diff_aesthetics, .text_aesthetics),
                          names(data))
    
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
    
    # aggregate variables over 'x', 'yneg', and 'stratum':
    # sum of computed variables and unique-or-bust values of aesthetics
    by_vars <- c("x", "yneg", "stratum")
    only_vars <- c(diff_aes)
    sum_vars <- c("y", "n", "count")
    if (! is.null(data$lode)) {
      agg_lode <- stats::aggregate(data[, "lode", drop = FALSE],
                                   data[, by_vars],
                                   distill)
    }
    if (length(only_vars) > 0) {
      agg_only <- stats::aggregate(data[, only_vars, drop = FALSE],
                                   data[, by_vars],
                                   only)
    }
    data <- stats::aggregate(data[, sum_vars],
                             data[, by_vars],
                             sum)
    if (! is.null(data$lode)) {
      data <- merge(data, agg_lode)
    }
    if (length(only_vars) > 0) {
      data <- merge(data, agg_only)
    }
    
    # remove empty lodes (including labels)
    data <- subset(data, y != 0)
    
    # define 'deposit' variable to rank strata vertically
    data <- deposit_data(data, decreasing, reverse, absolute)
    
    # calculate variables for `after_stat()`
    x_sums <- tapply(abs(data$count), data$x, sum, na.rm = TRUE)
    data$prop <- data$count / x_sums[match(as.character(data$x), names(x_sums))]
    
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

# single unique value, or else NA
only <- function(x) {
  uniq <- unique(x)
  if (length(uniq) == 1L) {
    uniq
  } else {
    switch(
      class(x),
      integer = NA_integer_,
      numeric = NA_real_,
      character = NA_character_,
      factor = factor(NA_character_, levels = levels(x))
    )
  }
}
