#' Check for alluvial structure and convert between alluvial formats
#'
#' These functions check a data frame for either of two alluvial formats
#' (`is_*_form()`) and convert a data frame from one format to the other
#' (`to_*_form()`).
#'
#' Alluvial plots consist of multiple horizontally-distributed columns (axes)
#' representing factor variables, vertical divisions (strata) of these axes
#' representing these variables' values; and splines (alluvial flows) connecting
#' vertical subdivisions (lodes) within strata of adjacent axes representing
#' subsets or amounts of observations that take the corresponding values of the
#' corresponding variables. Two functions check a data frame for alluvial
#' structure:
#' 
#' - One row per **lode**, wherein each row encodes a subset or amount of
#'   observations having a specific profile of axis values, a `key` field
#'   encodes the axis, a `value` field encodes the value within each axis, and a
#'   `id` column identifies multiple lodes corresponding to the same subset or
#'   amount of observations. `is_lodes_form` tests for this structure.
#' - One row per **alluvium**, wherein each row encodes a subset or amount of
#'   observations having a specific profile of axis values and a set `axes` of
#'   fields encodes its values at each axis variable. `is_alluvia_form` tests
#'   for this structure.
#'
#' `to_lodes_form` takes a data frame with several designated variables to
#' be used as axes in an alluvial plot, and reshapes the data frame so that
#' the axis variable names constitute a new factor variable and their values
#' comprise another. Other variables' values will be repeated, and a
#' row-grouping variable can be introduced. This function invokes
#' [tidyr::gather()].
#'
#' `to_alluvia_form` takes a data frame with axis and axis value variables
#' to be used in an alluvial plot, and reshape the data frame so that the
#' axes constitute separate variables whose values are given by the value
#' variable. This function invokes [tidyr::spread()].
#'

#' @name alluvial-data
#' @importFrom rlang enquo enquos enexpr enexprs quos is_empty quo_name
#'   is_character is_integerish is_quosures have_name
#' @importFrom tidyselect vars_pull vars_select eval_select
#' @family alluvial data manipulation
#' @param data A data frame.
#' @param logical Defunct. Whether to return a logical value or a character
#'   string indicating the type of alluvial structure ("none", "lodes", or
#'   "alluvia").
#' @param silent Whether to print messages.
#' @param key,value,id In `to_lodes_form`, handled as in
#'   [tidyr::gather()] and used to name the new axis (key), stratum
#'   (value), and alluvium (identifying) variables. In `to_alluvia_form`,
#'   handled as in [tidyr::spread()] and used to identify the fields
#'   of `data` to be used as the axis (key), stratum (value), and alluvium
#'   (identifying) variables.
#' @param axes In `*_alluvia_form`, handled as in
#'   [dplyr::select()] and used to identify the field(s) of
#'   `data` to be used as axes.
#' @param ... Used in `is_alluvia_form` and `to_lodes_form` as in
#'   [dplyr::select()] to determine axis variables, as an alternative
#'   to `axes`. Ignored when `axes` is provided.
#' @param weight Optional field of `data`, handled using
#'   [`rlang::enquo()`][rlang::nse-defuse], to be used as heights or depths of
#'   the alluvia or lodes.
#' @param site Optional vector of fields of `data`, handled using
#'   [`rlang::enquos()`][rlang::nse-defuse], to be used to group rows before
#'   testing for duplicate and missing id-axis pairings. Variables intended for
#'   faceting should be passed to `site`.
#' @param diffuse Fields of `data`, handled using
#'   [tidyselect::vars_select()], to merge into the reshapen data by
#'   `id`. They must be a subset of the axis variables. Alternatively, a
#'   logical value indicating whether to merge all (`TRUE`) or none
#'   (`FALSE`) of the axis variables.
#' @param distill A logical value indicating whether to include variables, other
#'   than those passed to `key` and `value`, that vary within values
#'   of `id`. Alternatively, a function (or its name) to be used to distill
#'   each such variable to a single value. In addition to existing functions,
#'   `distill` accepts the character values `"first"` (used if
#'   `distill` is `TRUE`), `"last"`, and `"most"` (which
#'   returns the first modal value).
#' @param discern Logical value indicating whether to suffix values of the
#'   variables used as axes that appear at more than one variable in order to
#'   distinguish their factor levels. This forces the levels of the combined
#'   factor variable `value` to be in the order of the axes.
#' @param stratum.guidance In `to_lodes_form`, how to combine the factor levels
#'   of the `axes` variables into those of the `value` variable: `NULL`
#'   (default) concatenates the per-axis levels in the order of the axes; a
#'   lode guidance pattern ([lode-guidance-functions]) concatenates the
#'   per-axis levels in the corresponding order, starting from `start`; and
#'   `"mean_rank"` orders the strata by their mean position (rank) across the
#'   axes at which they appear.
#' @param start In `to_lodes_form`, position (1-based) of the axis at which
#'   lode guidance orders begin, used only when `stratum.guidance` is a lode
#'   guidance pattern. If `NULL` (default), a value appropriate to the role of
#'   the anchoring axis in the chosen lode guidance pattern: the first axis for
#'   `"forward"`, the last axis for `"backward"`, the floored middle axis
#'   (e.g. 2 when there are 4 axes) for `"zagzig"` and `"frontback"`, and the
#'   ceilinged middle axis (e.g. 3 when there are 4 axes) for `"zigzag"` and
#'   `"backfront"`.
#' @example inst/examples/ex-alluvial-data.r

#' @rdname alluvial-data
#' @export
is_lodes_form <- function(data,
                          key, value, id,
                          weight = NULL, site = NULL,
                          logical = TRUE, silent = FALSE) {
  if (! isTRUE(logical)) defunct_parameter("logical")
  
  key_var <- vars_pull(names(data), !! enquo(key))
  value_var <- vars_pull(names(data), !! enquo(value))
  id_var <- vars_pull(names(data), !! enquo(id))
  
  # test id-axis pairings within each site (see issue #65)
  if (! is.null(enexprs(site))) {
    site_vars <- vars_select(names(data), !!! enquos(site))
    data[[id_var]] <- interaction(data[c(id_var, site_vars)], drop = FALSE)
  }
  
  if (any(duplicated(cbind(data[c(key_var, id_var)])))) {
    if (! silent) message("Duplicated id-axis pairings",
                          if (! is.null(enexprs(site))) "." else
                            "; should `site` have been specified?")
    return(if (logical) FALSE else "none")
  }
  
  n_pairs <-
    dplyr::n_distinct(data[key_var]) * dplyr::n_distinct(data[id_var])
  if (nrow(data) < n_pairs) {
    if (! silent) warning("Missing id-axis pairings (at some sites).")
  }
  
  # if `weight` is not `NULL`, use NSE to identify `weight_var`
  if (! is.null(enexpr(weight))) {
    weight_var <- vars_select(names(data), !! enquo(weight))
    if (! is.numeric(data[[weight_var]])) {
      if (! silent) message("Lode weights are non-numeric.")
      return(if (logical) FALSE else "none")
    }
  }
  
  if (logical) TRUE else "lodes"
}

#' @rdname alluvial-data
#' @export
is_alluvia_form <- function(data,
                            ..., axes = NULL,
                            weight = NULL,
                            logical = TRUE, silent = FALSE) {
  if (! isTRUE(logical)) defunct_parameter("logical")
  
  if (is.null(enexpr(weight))) {
    weight_var <- NULL
  } else {
    weight_var <- vars_select(names(data), !! enquo(weight))
    if (! is.numeric(data[[weight_var]])) {
      if (! silent) message("Alluvium weights are non-numeric.")
      return(if (logical) FALSE else "none")
    }
  }
  
  if (! is.null(enexpr(axes))) {
    axes <- data_at_vars(data, axes)
  } else {
    quos <- quos(...)
    if (is_empty(quos)) {
      axes <- setdiff(names(data), c(weight_var))
    } else {
      axes <- unname(vars_select(names(data), !!! quos))
    }
  }
  
  n_alluvia <- nrow(dplyr::distinct(data[axes]))
  n_combns <- do.call(prod, lapply(data[axes], dplyr::n_distinct))
  if (n_alluvia < n_combns) {
    if (! silent) message("Missing alluvia for some stratum combinations.")
  }
  
  if (logical) TRUE else "alluvia"
}

#' @rdname alluvial-data
#' @export
to_lodes_form <- function(data,
                          ..., axes = NULL,
                          key = "x", value = "stratum", id = "alluvium",
                          diffuse = FALSE, discern = FALSE,
                          stratum.guidance = NULL, start = NULL) {
  
  key_var <- quo_name(enexpr(key))
  value_var <- quo_name(enexpr(value))
  id_var <- quo_name(enexpr(id))
  
  if (! is.null(enexpr(axes))) {
    axes <- data_at_vars(data, axes)
  } else {
    quos <- quos(...)
    if (is_empty(quos)) {
      axes <- names(data)
    } else {
      axes <- unname(vars_select(names(data), !!! quos))
    }
  }
  
  stopifnot(is_alluvia_form(data, axes, silent = TRUE))
  
  if (! is.data.frame(data)) data <- as.data.frame(data)
  
  if (is.logical(enexpr(diffuse))) {
    diffuse <- if (diffuse) axes else NULL
  } else {
    diffuse <- unname(vars_select(names(data), !! enquo(diffuse)))
    if (! all(diffuse %in% axes)) {
      stop("All `diffuse` variables must be `axes` variables.")
    }
  }
  
  # combine factor levels
  list_levels <- lapply(lapply(data[axes], as.factor), levels)
  cat_levels <- unname(unlist(list_levels))
  if (any(duplicated(cat_levels)) & is.null(discern)) {
    warning("Some strata appear at multiple axes.")
  }
  if (isTRUE(discern)) {
    # suffix (or "discern") values that appear at several axes
    data <- discern_data(data, axes)
    # reconstitute the per-axis levels from the discerned data
    list_levels <- lapply(lapply(data[axes], as.factor), levels)
  }
  if (is.null(stratum.guidance)) {
    # concatenate the per-axis levels in the order of the axes (default)
    strata <-
      if (isTRUE(discern)) make.unique(cat_levels) else unique(cat_levels)
  } else {
    stratum.guidance <- match.arg(stratum.guidance, c("forward", "zigzag",
                                "zagzig", "backward",
                                "frontback", "backfront", "rightward",
                                "leftward", "rightleft", "leftright",
                                "mean_rank"))
    if (stratum.guidance == "mean_rank") {
      # score each stratum by its mean position (rank) across the axes at
      # which it appears; order by that score, breaking ties by appearance
      all_strata <- unique(unlist(list_levels))
      mean_ranks <- vapply(
        all_strata,
        function(s) mean(vapply(list_levels, function(lv) match(s, lv),
                                integer(1))),
        numeric(1)
      )
      strata <- all_strata[order(mean_ranks)]
    } else {
      # concatenate the per-axis levels following the lode guidance function
      guidance_fun <- switch(
        stratum.guidance,
        zigzag = lode_zigzag, zagzig = lode_zagzig,
        forward = lode_forward, backward = lode_backward,
        frontback = lode_frontback, backfront = lode_backfront,
        rightward = lode_rightward, leftward = lode_leftward,
        rightleft = lode_rightleft, leftright = lode_leftright
      )
      n_axes <- length(axes)
      if (is.null(start)) {
        # choose a default `start` befitting the chosen lode guidance function
        mid_fl <- floor((n_axes + 1) / 2)
        mid_ceil <- ceiling((n_axes + 1) / 2)
        start <- switch(
          stratum.guidance,
          forward = 1, rightward = 1,
          backward = n_axes, leftward = n_axes,
          frontback = mid_fl, rightleft = mid_fl, zagzig = mid_fl,
          backfront = mid_ceil, leftright = mid_ceil, zigzag = mid_ceil
        )
      }
      stopifnot(start >= 1, start <= n_axes)
      axis_order <- guidance_fun(n_axes, start)
      strata <- unique(unlist(list_levels[axis_order]))
    }
  }
  
  # format data in preparation for `gather()`
  data[[id_var]] <- 1:nrow(data)
  if (! is.null(diffuse)) {
    diffuse_data <- data[, c(id_var, diffuse), drop = FALSE]
  }
  for (i in axes) data[[i]] <- as.character(data[[i]])
  
  # `gather()` by `axes`
  res <- tidyr::gather(data,
                       key = !! key_var, value = !! value_var,
                       axes,
                       factor_key = TRUE)
  res[[value_var]] <- factor(res[[value_var]], levels = strata)
  # recombine with `diffuse_data`
  if (! is.null(diffuse)) {
    res <- merge(diffuse_data, res, by = id_var, all.x = FALSE, all.y = TRUE)
  }
  
  res
}

#' @rdname alluvial-data
#' @export
to_alluvia_form <- function(data,
                            key, value, id,
                            distill = FALSE) {
  
  key_var <- vars_pull(names(data), !! enquo(key))
  value_var <- vars_pull(names(data), !! enquo(value))
  id_var <- vars_pull(names(data), !! enquo(id))
  
  stopifnot(is_lodes_form(data, key_var, value_var, id_var, silent = TRUE))
  
  # handle any variables that vary within `id`s
  uniq_id <- length(unique(data[[id_var]]))
  uniq_data <- unique(data[setdiff(names(data), c(key_var, value_var))])
  if (! uniq_id == nrow(uniq_data)) {
    distill_vars <- names(which(sapply(
      setdiff(names(uniq_data), id_var),
      function(x) nrow(unique(uniq_data[c(id_var, x)]))
    ) > uniq_id))
    if (is.logical(distill)) {
      if (isTRUE(distill)) {
        distill <- "first"
      } else {
        warning("The following variables vary within `id`s ",
                "and will be dropped: ",
                paste(distill_vars, collapse = ", "))
        distill <- NULL
      }
    # } else if (is.character(distill)) {
    #   distill <- get(distill)
    } else {
      distill <- distill_fun(distill)
    }
    if (! is.null(distill)) {
      stopifnot(is.function(distill))
      message("Distilled variables: ",
              paste(distill_vars, collapse = ", "))
      distill_data <- stats::aggregate(
        data[distill_vars],
        data[id_var],
        distill
      )
      if (length(distill_vars) == 1) names(distill_data)[-1] <- distill_vars
    }
    data <- data[setdiff(names(data), distill_vars)]
  } else {
    distill <- NULL
  }
  
  # `spread()` by designated `key` and `value`
  res <- tidyr::spread(data, key = !! key_var, value = !! value_var)
  # recombine with `distill_data`
  if (! is.null(distill)) {
    res <- merge(distill_data, res, by = id_var, all.x = FALSE, all.y = TRUE)
  }
  
  res
}

# require different character strings to represent strata at different axes
discern_data <- function(data, axes, sep = ".") {
  # strata at each axis in order
  list_levels <- lapply(lapply(data[axes], as.factor), levels)
  # concatenated vector of strata at all axes
  cat_levels <- unlist(list_levels)
  # vector of uniquified strata across all axes
  new_levels <- make.unique(unname(cat_levels))
  # cumulative number of strata before each axis
  i_levels <- cumsum(c(0, sapply(list_levels, length)))
  # characterized, uniquified strata at each axis
  for (i in seq_along(axes)) {
    axis_levels <- as.numeric(as.factor(data[[axes[i]]]))
    level_inds <- (i_levels[i] + 1):i_levels[i + 1]
    data[[axes[i]]] <- new_levels[level_inds][axis_levels]
  }
  data
}

# mimic the behavior of `tbl_at_vars()` in `select_at()`
data_at_vars <- function(data, vars) {
  data_vars <- names(data)
  if (is_character(vars)) {
    vars
  } else if (is_integerish(vars)) {
    data_vars[vars]
  } else if (is_quosures(vars)) {
    # create a named list to pass to `tidyselect::eval_select()`
    data_names <- rlang::set_names(seq_along(data_vars), data_vars)
    out <- eval_select(rlang::expr(c(!!! vars)), data_names)
    out <- names(out)
    if (! any(have_name(vars))) {
      names(out) <- NULL
    }
    out
  } else {
    stop("Either a character or numeric vector ",
         "or a `vars()` object ",
         "is required.")
  }
}
