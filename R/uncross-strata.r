#' Reorder strata to minimize alluvial crossings
#'
#' These functions call [wompwomp::sort_to_uncross()] to obtain an uncrossing
#' order at each axis, then translate these axis-wise orderings into the levels
#' of the corresponding factor variables.
#'
#' Alluvial plots encode the values of categorical variables as strata, placed
#' at successive axes. Large numbers of crossings of their connecting alluvia
#' can compromise the clarity of the plot. **wompwomp** provides several methods
#' to minimize these crossings, and these functions are convenience wrappers for
#' data in formats recognized by **ggalluvial**.
#'
#' `uncross_strata_alluvia()` takes `data` in alluvia format (one row per
#' alluvium, with the axis variables as columns), runs
#' [wompwomp::sort_to_uncross()] to obtain a category order at each axis, and
#' returns the axis variables as factors with levels in these orders.
#'
#' `uncross_strata_lodes()` takes `data` in lodes format (one row per lode, with
#' `key` (axis), `value` (stratum), and `id` (alluvium) fields), reshapes it to
#' alluvia form, calls `uncross_strata_alluvia()`, then reshapes it back to
#' lodes form. The last step uses a guidance function (`stratum.guidance`) to
#' combine the axis-wise factor level orders into a single level order:
#' 

#' * the lode guidance options (`zigzag`, `zagzig`, `forward` (alias
#'   `rightward`), `backward` (alias `leftward`), `frontback` (alias
#'   `rightleft`), and `backfront` (alias `leftright`): concatenate the per-axis
#'   orderings in the corresponding lode guidance pattern, starting from the
#'   axis at position `start`. When not explicitly passed, the default `start`
#'   position depends on the guidance pattern: the middle axis (rounded down)
#'   for `zagzig` and `frontback`, the middle axis (rounded up) for `zigzag` and
#'   `backfront`, the first axis for `forward`, and the last axis for
#'   `backward`. See [lode-guidance-functions] for details on the guidance
#'   functions.
#' * `mean_rank`: score each stratum by its mean position (rank) across the
#'   axes at which it appears, then order by that score.

#' @name uncross-strata
#' @inheritParams alluvial-data
#' @param method A valid `method` argument to [wompwomp::sort_to_uncross()];
#'   defaults to `"neighbornet"`.
#' @param missing_value Character, passed to
#'   [wompwomp::sort_to_uncross_options()]; the value to replace missing entries
#'   in the sorted data. `NA` is not yet allowed.
#' @return The input data with either all axis variables or the one stratum
#'   variable factored or re-leveled.
#' @example inst/examples/ex-uncross-strata.r
#' @family alluvial data manipulation
#' @export
uncross_strata_alluvia <- function(data, ..., axes = NULL,
                                   weight = NULL, method = "neighbornet",
                                   missing_value = "Missing") {
  if (! is.null(enexpr(axes))) {
    # FIXME: Using an external vector in selections was deprecated.
    axes <- unname(vars_select(names(data), !! enquo(axes)))
  } else {
    quos <- quos(...)
    if (is_empty(quos)) {
      axes <- names(data)
    } else {
      axes <- unname(vars_select(names(data), !!! quos))
    }
  }
  stopifnot(is_alluvia_form(data, axes = axes, silent = TRUE))

  rlang::check_installed(
    "wompwomp",
    reason = "to optimize ribbon crossings.",
    version = "1.0.0"
  )
  if (missing_value %in% names(data)) {
    stop(
      "String '", missing_value, "' is already a `data` column name;",
      " pass a different value to the `missing_value` argument."
    )
  }
  stu_opts <- wompwomp::sort_to_uncross_options(
    missing_value = missing_value
  )
  # wompwomp ordering of each axis
  data_sort <- if (is.null(enexpr(weight))) {
    suppressWarnings(wompwomp::sort_to_uncross(
      data, cols = axes, method = method, options = stu_opts
    ))
  } else {
    weight_var <- vars_pull(names(data), !! enquo(weight))
    suppressWarnings(do.call(
      wompwomp::sort_to_uncross,
      list(data, cols = axes, wt = weight_var,
           method = method, options = stu_opts)
    ))
  }
  
  # merge non-axis/weight variables back in
  merge_by <- c(axes, if (! is.null(enexpr(weight))) weight_var)
  # FIXME: If original has missing entries then sorted has filler values;
  # also sorted has been collapsed.
  merge(data_sort, data, by = merge_by)
}

#' @rdname uncross-strata
#' @export
uncross_strata_lodes <- function(data, method = "neighbornet",
                                 stratum.guidance = c("forward", "zigzag",
                                                      "zagzig",
                                                      "backward", "frontback",
                                                      "backfront", "rightward",
                                                      "leftward", "rightleft",
                                                      "leftright", "mean_rank"),
                                 start = NULL,
                                 key = "x", value = "stratum",
                                 id = "alluvium", weight = NULL,
                                 missing_value = "Missing") {
  
  key_var <- vars_pull(names(data), !! enquo(key))
  value_var <- vars_pull(names(data), !! enquo(value))
  id_var <- vars_pull(names(data), !! enquo(id))
  if (is.null(enexpr(weight))) {
    weight_var <- NULL
  } else {
    weight_var <- vars_select(names(data), !! enquo(weight))
  }
  stratum.guidance <- match.arg(stratum.guidance)
  stopifnot(is_lodes_form(data, key_var, value_var, id_var, silent = TRUE))

  # convert to alluvia form and store axis variables
  data_alluvia <- to_alluvia_form(data[c(id_var, key_var, value_var)],
                          key = key_var, value = value_var, id = id_var)
  axes_vars <- levels(factor(data[[key_var]]))

  if (! is.null(weight_var)) {
    wt_by_id <- stats::aggregate(data[[weight_var]], list(data[[id_var]]), sum)
    names(wt_by_id) <- c(id_var, "wt")
    data_alluvia <- merge(data_alluvia, wt_by_id, by = id_var, all.x = TRUE)
    womp_wt <- "wt"
  } else {
    womp_wt <- NULL
  }

  # uncross the axis orders (alluvia form in, alluvia form out)
  data_sort <- if (is.null(womp_wt)) {
    uncross_strata_alluvia(data_alluvia, axes = axes_vars,
                           method = method, missing_value = missing_value)
  } else {
    uncross_strata_alluvia(data_alluvia, axes = axes_vars, weight = "wt",
                           method = method, missing_value = missing_value)
  }

  # re-level axis variables of `data_alluvia` (which retains `id`) by the
  # wompwomp per-axis orderings, then use `to_lodes_form()` to combine them into
  # the levels of the stratum variable (use `do.call()` so that the string
  # key/value/id values are injected literally, rather than being miscaptured by
  # the NSE arguments)
  data_sort_alluvia <- data_alluvia
  for (axis in axes_vars) {
    data_sort_alluvia[[axis]] <-
      factor(data_alluvia[[axis]], levels = levels(data_sort[[axis]]))
  }
  data_sort_lodes <- do.call(
    to_lodes_form,
    list(data_sort_alluvia,
         axes = axes_vars,
         key = key_var, value = value_var, id = id_var,
         stratum.guidance = stratum.guidance, start = start)
  )
  strata <- levels(data_sort_lodes[[value_var]])

  # re-factor (or re-level) the `stratum` variable
  data[[value_var]] <- factor(data[[value_var]], levels = strata)
  data
}
