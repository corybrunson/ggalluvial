# Draft helper function: reorder the strata of lodes-form data according to an
# uncrossing order obtained from {wompwomp}.
#
# The function reshapes `data` to alluvia form, runs
# `wompwomp::sort_to_uncross()` to obtain an uncrossing order at each axis, and
# re-levels the stratum variable accordingly. What wompwomp returns is a
# per-axis ordering; `order` specifies how these axis-specific orderings are
# translated into a single global ordering of the `stratum` variable:
#
#   * the lode guidance options `zigzag`, `zagzig`, `forward`, `backward`,
#     `frontback`, and `backfront` (with directional aliases `rightward`,
#     `leftward`, `rightleft`, and `leftright`): concatenate the per-axis
#     orderings in the corresponding lode guidance pattern, starting from the
#     axis at position `start`, by default a value appropriate to the role of
#     the anchoring axis in the chosen lode guidance pattern: the floored
#     middle axis (perhaps the axis least likely to be crossed) for
#     `zagzig` and `frontback`, a sometimes-different floored middle axis,
#     the ceilinged middle axis, for `zigzag` and `backfront`, the first axis
#     for `forward`, and the last axis for `backward`. The default, `forward`
#     from the *first* axis, scans the axes left-to-right in increasing
#     order.
#   * `mean_rank`: score each stratum by its mean position (rank) across the
#                  axes at which it appears, then order by that score.
#
# The concatenating options (e.g. `forward` with `start = 1`) are equivalent in
# output; `mean_rank` can disagree whenever a stratum appears at more than one
# axis (as in the `majors` data: `forward` begins "Ceramic | Painting | ...",
# but `mean_rank` begins "Painting | Ceramic | ..."). All options agree when
# every stratum lives at a single axis (as in the `vaccinations` and Titanic
# examples).
#
# Shared design conventions and caveats:
#   * Only the stratum column is modified, so the returned frame has the same
#     columns, in the same order, and the same rows as `data`.
#   * The stratum column is `factor()`ed if not already a factor, or re-leveled
#     if it is, using the combined level vector.
#   * A weight column passed to `weight` is summed within each alluvium and
#     supplied to wompwomp as its `wt` argument; if `weight` is `NULL`, the
#     alluvia are treated as unit-weight entities.
#   * wompwomp captures `wt` by NSE: an unweighted call must *omit* `wt`
#     (an explicit `wt = NULL` errors) and a weighted call must pass the
#     literal string "wt" (a variable that evaluates to a string errors).
#   * to_alluvia_form() captures `key`/`value`/`id` by NSE (rlang::enquo), which
#     accepts both bare column names supplied by the caller and string values
#     stored in variables (here, computed from `names(data)`).
#   * The default method ("tsp") is a randomized heuristic: repeated calls can
#     return different orderings unless a seed is set (or results are cached).

#' Relevel strata to uncross alluvia
#'
#' Reshape `data` to alluvia form, run [wompwomp::sort_to_uncross()] to obtain
#' an uncrossing order at each axis, and re-level the stratum variable
#' accordingly.
#'
#' @inheritParams alluvial-data
#' @param method A valid `method` argument to [wompwomp::sort_to_uncross()],
#'   e.g. `"tsp"` (default).
#' @param order How to combine the per-axis orders returned by wompwomp into a
#'   single global ordering of the stratum variable: either a lode guidance
#'   pattern ([lode-guidance-functions], used with `start`) or the positional
#'   `"mean_rank"`.
#' @param start Position (1-based) of the axis at which concatenating lode
#'   guidance orders begin; if `NULL` (default), a value appropriate to the
#'   role of the anchoring axis in the chosen lode guidance pattern: the
#'   first axis for `"forward"`, the last axis for `"backward"`, the floored
#'   middle axis (e.g. 2 when there are 4 axes) for `"zagzig"` and
#'   `"frontback"`, and the ceilinged middle axis (e.g. 3 when there are 4
#'   axes) for `"zigzag"` and `"backfront"`.
#'
#' @return `data` with the stratum variable factored or re-leveled, and all
#'   columns in their original order.
uncross_strata <- function(data, method = "tsp",
                          order = c("forward", "zigzag", "zagzig",
                                    "backward", "frontback", "backfront",
                                    "rightward", "leftward",
                                    "rightleft", "leftright",
                                    "mean_rank"),
                          start = NULL,
                          key = "x", value = "stratum",
                          id = "alluvium", weight = NULL) {
  # capture the structural columns by NSE, as in to_lodes_form() and friends
  key_var <- tidyselect::vars_pull(names(data), !! rlang::enquo(key))
  value_var <- tidyselect::vars_pull(names(data), !! rlang::enquo(value))
  id_var <- tidyselect::vars_pull(names(data), !! rlang::enquo(id))
  if (is.null(rlang::enexpr(weight))) {
    weight_var <- NULL
  } else {
    weight_var <- tidyselect::vars_select(names(data), !! rlang::enquo(weight))
  }
  order <- match.arg(order)
  stopifnot(is_lodes_form(data, key_var, value_var, id_var, silent = TRUE))

  # wide (alluvia) form, one row per alluvium (only the structural columns)
  wide <- to_alluvia_form(data[c(id_var, key_var, value_var)],
                          key = key_var, value = value_var, id = id_var)
  axes_vars <- levels(factor(data[[key_var]]))

  if (! is.null(weight_var)) {
    wt_by_id <- stats::aggregate(data[[weight_var]], list(data[[id_var]]), sum)
    names(wt_by_id) <- c(id_var, "wt")
    wide <- merge(wide, wt_by_id, by = id_var, all.x = TRUE)
    womp_wt <- "wt"
  } else {
    womp_wt <- NULL
  }

  # wompwomp ordering of each axis
  sorted <- if (is.null(womp_wt)) {
    suppressWarnings(wompwomp::sort_to_uncross(
      wide, cols = axes_vars, method = method
    ))
  } else {
    suppressWarnings(wompwomp::sort_to_uncross(
      wide, cols = axes_vars, wt = "wt", method = method
    ))
  }

  # per-axis stratum orders, in wompwomp's recommended order
  axis_levels <- lapply(sorted[axes_vars], levels)

  if (order == "mean_rank") {
    # score each stratum by its mean position (rank) across the axes at which
    # it appears; order by that score, breaking ties by first appearance
    all_strata <- unique(unlist(axis_levels))
    mean_ranks <- vapply(
      all_strata,
      function(s) mean(vapply(axis_levels, function(lv) match(s, lv),
                              integer(1))),
      numeric(1)
    )
    strata <- all_strata[order(mean_ranks)]
  } else {
    # concatenate the per-axis orders following the lode guidance function
    guidance_fun <- switch(
      order,
      zigzag = lode_zigzag, zagzig = lode_zagzig,
      forward = lode_forward, backward = lode_backward,
      frontback = lode_frontback, backfront = lode_backfront,
      rightward = lode_rightward, leftward = lode_leftward,
      rightleft = lode_rightleft, leftright = lode_leftright
    )
    n_axes <- length(axes_vars)
    if (is.null(start)) {
      # choose a default `start` befitting the chosen lode guidance function
      mid_fl <- floor((n_axes + 1) / 2)
      mid_ceil <- ceiling((n_axes + 1) / 2)
      start <- switch(
        order,
        forward = 1, rightward = 1,
        backward = n_axes, leftward = n_axes,
        frontback = mid_fl, rightleft = mid_fl, zagzig = mid_fl,
        backfront = mid_ceil, leftright = mid_ceil, zigzag = mid_ceil
      )
    }
    stopifnot(start >= 1, start <= n_axes)
    axis_order <- guidance_fun(n_axes, start)
    strata <- unique(unlist(axis_levels[axis_order]))
  }

  # re-factor (or re-level) `stratum`, leaving other columns untouched
  data[[value_var]] <- factor(data[[value_var]], levels = strata)
  data
}
