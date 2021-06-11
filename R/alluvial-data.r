#' Check for alluvial structure and convert between alluvial formats
#'
#' Alluvial plots consist of multiple horizontally-distributed columns (axes)
#' representing factor variables, vertical divisions (strata) of these axes
#' representing these variables' values; and splines (alluvial flows) connecting
#' vertical subdivisions (lodes) within strata of adjacent axes representing
#' subsets or amounts of observations that take the corresponding values of the
#' corresponding variables. This function checks a data frame for either of two
#' types of alluvial structure:
#'
#' - One row per **lode**, wherein each row encodes a subset or amount of
#' observations having a specific profile of axis values, a `key` field encodes
#' the axis, a `value` field encodes the value within each axis, and a `id`
#' column identifies multiple lodes corresponding to the same subset or amount
#' of observations. `is_lodes_form` tests for this structure. - One row per
#' **alluvium**, wherein each row encodes a subset or amount of observations
#' having a specific profile of axis values and a set `axes` of fields encodes
#' its values at each axis variable. `is_alluvia_form` tests for this structure.
#'
#' `to_lodes_form` takes a data frame with several designated variables to be
#' used as axes in an alluvial plot, and reshapes the data frame so that the
#' axis variable names constitute a new factor variable and their values
#' comprise another. Other variables' values will be repeated, and a
#' row-grouping variable can be introduced. This function invokes
#' [tidyr::pivot_longer()].
#'
#' `to_alluvia_form` takes a data frame with axis and axis value variables to be
#' used in an alluvial plot, and reshape the data frame so that the axes
#' constitute separate variables whose values are given by the value variable.
#' This function invokes [tidyr::pivot_wider()].
#'
#' The check and conversion functions both use **rlang** operators to [capture
#' and defuse][rlang::nse-defuse] expressions. See the examples for different
#' options.
#' 

#' @name alluvial-data
#' @importFrom rlang enquo enquos enexpr enexprs quos is_empty quo_name
#'   is_character is_integerish is_quosures have_name
#' @importFrom tidyselect vars_pull vars_select all_of
#' @family alluvial data manipulation
#' @param data A data frame.
#' @param silent Whether to print messages.
#' @param key,value,id In `to_lodes_form`, captured, defused, and used to name
#'   the new axis (`key`), stratum (`value`), and alluvium (`id`entifying)
#'   variables. In `is_lodes_form` and `to_alluvia_form`, captured, defused, and
#'   used to identify the fields of `data` to be used as the axis (key), stratum
#'   (value), and alluvium (identifying) variables.
#' @param axes In `*_alluvia_form`, captured, defused, and used to identify the
#'   field(s) of `data` to be used as axes.
#' @param y Optional field(s) of `data`, captured and defused, to be used as
#'   heights or depths of the alluvia or lodes.
#' @param y_to A string specifying the name of the column to create from the
#'   column(s) identified by `y`. If needed for multiple columns but not
#'   provided, defaults to 'y'.
#' @param site Optional vector of fields of `data`, captured and defused, to be
#'   used to group rows before testing for duplicate and missing id-axis
#'   pairings. Variables intended for faceting should be passed to `site`.
#' @param diffuse Fields of `data`, handled using [tidyselect::vars_select()],
#'   to merge into the reshapen data by `id`. They must be a subset of the axis
#'   variables. Alternatively, a logical value indicating whether to merge all
#'   (`TRUE`) or none (`FALSE`) of the axis variables.
#' @param distill A logical value indicating whether to include variables, other
#'   than those passed to `key` and `value`, that vary within values of `id`.
#'   Alternatively, a function (or its name) to be used to distill each such
#'   variable to a single value. In addition to existing functions, `distill`
#'   accepts the character values `"first"` (used if `distill` is `TRUE`),
#'   `"last"`, and `"most"` (which returns the first modal value).
#' @param discern Logical value indicating whether to suffix values of the
#'   variables used as axes that appear at more than one variable in order to
#'   distinguish their factor levels. This forces the levels of the combined
#'   factor variable `value` to be in the order of the axes.
#' @example inst/examples/ex-alluvial-data.r

#' @rdname alluvial-data
#' @export
is_lodes_form <- function(data,
                          key, value, id,
                          y = NULL, site = NULL,
                          silent = FALSE) {

  key_var <- vars_pull(names(data), !! enquo(key))
  value_var <- vars_pull(names(data), !! enquo(value))
  id_var <- vars_pull(names(data), !! enquo(id))
  if (key_var %in% c(value_var, id_var)) {
    stop("`key` must be distinct from `id` and `value` variables.")
  }

  # test id-axis pairings within each site (see issue #65)
  if (! is.null(enexprs(site))) {
    site_vars <- vars_select(names(data), !!! enquos(site))
    data[[id_var]] <- interaction(data[c(id_var, site_vars)], drop = FALSE)
  }

  if (any(duplicated(cbind(data[c(key_var, id_var)])))) {
    if (! silent) message("Duplicated id-axis pairings",
                          if (! is.null(enexprs(site))) "." else
                            "; should `site` have been specified?")
    return(FALSE)
  }

  n_pairs <-
    dplyr::n_distinct(data[key_var]) * dplyr::n_distinct(data[id_var])
  if (nrow(data) < n_pairs) {
    if (! silent) warning("Missing id-axis pairings (at some sites).")
  }

  # if `y` is not `NULL`, use non-standard evaluation to identify `y_var`
  if (! is.null(enexpr(y))) {
    y_var <- vars_select(unique(names(data)), !! enquo(y))
    if (length(y_var) > 0) {
      if (length(y_var) > 1) stop("`y` must be a single variable.")
      if (y_var %in% c(key_var, value_var, id_var)) {
        stop("`y` must be distinct from id-key-value variables.")
      }
      if (! is.numeric(data[[y_var]])) {
        if (! silent) message("Lode `y` (height) values are non-numeric.")
        return(FALSE)
      }
    }
  }

  TRUE
}

#' @rdname alluvial-data
#' @export
is_alluvia_form <- function(data,
                            axes = NULL, y = NULL,
                            silent = FALSE) {

  # note: specifying `y` is optional
  if (is.null(enexpr(y))) {
    y_var <- NULL
  } else {
    y_var <- vars_select(unique(names(data)), !! enquo(y))
    if (! all(sapply(data[y_var], is.numeric))) {
      if (! silent) message("Some lode `y` (height) values are non-numeric.")
      return(FALSE)
    }
  }

  if (! is.null(enexpr(axes))) {
    axes <- data_at_vars(data, enquos(axes))
  } else {
    axes <- setdiff(names(data), c(y_var))
  }
  if (length(y_var) > 1 && length(y_var) != length(axes)) {
    if (! silent) message("The number of `y` (height) columns, if > 1, ",
                          "must equal the number of axes.")
    return(FALSE)
  }

  n_alluvia <- nrow(dplyr::distinct(data[axes]))
  n_combns <- do.call(prod, lapply(data[axes], dplyr::n_distinct))
  if (n_alluvia < n_combns) {
    if (! silent) message("Some stratum combinations are missing alluvia.")
  }

  TRUE
}

#' @rdname alluvial-data
#' @export
to_lodes_form <- function(data,
                          axes = NULL, y = NULL,
                          key = "x", value = "stratum", id = "alluvium",
                          y_to = NULL,
                          diffuse = FALSE, discern = FALSE) {

  # note: specifying `y` is optional
  if (is.null(rlang::enexpr(y))) {
    y_var <- NULL
  } else {
    y_var <- unname(vars_select(unique(names(data)), !! enquo(y)))
    if (! all(sapply(data[y_var], is.numeric))) {
      message("Some lode `y` (height) values are non-numeric.")
      return(FALSE)
    }
  }

  if (! is.null(enexpr(axes))) {
    axes <- data_at_vars(data, enquos(axes))
  } else {
    axes <- setdiff(names(data), c(y_var))
  }
  if (length(y_var) > 1 && length(y_var) != length(axes)) {
    message("The number of `y` (height) columns, if > 1, ",
            "must equal the number of `axes`.")
    return(FALSE)
  }

  stopifnot(is_alluvia_form(data,
                            axes = all_of(axes), y = all_of(y_var),
                            silent = TRUE))

  key_var <- rlang::quo_name(rlang::enexpr(key))
  value_var <- rlang::quo_name(rlang::enexpr(value))
  id_var <- rlang::quo_name(rlang::enexpr(id))

  if (! is.data.frame(data)) data <- as.data.frame(data)

  if (is.logical(enexpr(diffuse))) {
    diffuse <- if (diffuse) axes else NULL
  } else {
    diffuse <- unname(vars_select(unique(names(data)), !! enquo(diffuse)))
    if (! all(diffuse %in% c(axes, y_var))) {
      stop("All `diffuse` variables must be `axes` or `y` variables.")
    }
  }

  # combine factor levels
  cat_levels <- unname(unlist(lapply(lapply(data[axes], as.factor), levels)))
  if (any(duplicated(cat_levels)) & is.null(discern)) {
    #warning("Some strata appear at multiple axes.")
  }
  if (isTRUE(discern)) {
    data <- discern_data(data, axes)
    # uniquify strata separately from `discern_data` as a validation step
    strata <- make.unique(unname(cat_levels))
  } else {
    strata <- unique(unname(cat_levels))
  }

  # format data in preparation for `pivot_longer()`
  data[[id_var]] <- seq(nrow(data))
  if (! is.null(diffuse)) {
    diffuse_data <- data[, c(id_var, diffuse), drop = FALSE]
  }
  for (i in axes) data[[i]] <- as.character(data[[i]])

  key_ptype <- list(factor())
  names(key_ptype) <- key_var
  # `pivot_longer()` by `axes` (and possibly `y`)
  res <- if (length(y_var) == 0 ) {
    tidyr::pivot_longer(data,
                        cols = all_of(axes),
                        names_to = all_of(key_var),
                        names_ptypes = key_ptype,
                        values_to = all_of(value_var))
  } else if (length(y_var) == 1) {
    if (! is.null(y_to)) names(data)[match(y_var, names(data))] <- y_to
    tidyr::pivot_longer(data,
                        cols = all_of(axes),
                        names_to = all_of(key_var),
                        names_ptypes = key_ptype,
                        values_to = all_of(value_var))
  } else {
    if (is.null(y_to)) {
      message("Pivoting multiple `y` columns, y_to = 'y'.")
      y_to <- "y"
    }
    axes_to <- paste(value_var, axes, sep = "___")
    y_var_to <- paste(y_to, axes, sep = "___")
    names(data)[match(axes, names(data))] <- axes_to
    names(data)[match(y_var, names(data))] <- y_var_to
    tidyr::pivot_longer(data,
                        cols = c(all_of(axes_to), all_of(y_var_to)),
                        names_to = c(".value", key_var),
                        names_sep = "___")
  }
  res[[value_var]] <- factor(res[[value_var]], levels = strata)

  # merge in `diffuse_data`
  if (is.null(diffuse)) {
    res <- as.data.frame(res)
  } else {
    res <- merge(diffuse_data, res, by = id_var, all.x = FALSE, all.y = TRUE)
  }

  res
}

#' @rdname alluvial-data
#' @export
to_alluvia_form <- function(data,
                            key, value, id,
                            y = NULL,
                            distill = FALSE) {

  key_var <- vars_pull(names(data), !! enquo(key))
  value_var <- vars_pull(names(data), !! enquo(value))
  id_var <- vars_pull(names(data), !! enquo(id))
  if (key_var %in% c(value_var, id_var)) {
    stop("`key` must be distinct from `id` and `value` variables.")
  }
  if (value_var == id_var) {
    message("Duplicating column '", value_var, "' used as `id` and `value`.")
    value_var <- paste0(value_var, "___")
    data[[value_var]] <- data[[id_var]]
  }

  y_var <- if (is.null(rlang::enexpr(y))) NULL else {
    vars_pull(names(data), !! enquo(y))
  }
  stopifnot(is_lodes_form(data,
                          key = key_var, value = value_var, id = id_var,
                          y = y_var,
                          silent = TRUE))

  # whether any variables vary within `id`s
  uniq_id <- dplyr::n_distinct(data[[id_var]])
  uniq_data <- unique(data[setdiff(names(data), c(key_var, value_var, y_var))])
  if (! uniq_id == nrow(uniq_data)) {
    # which variables vary within `id`s
    distill_vars <- names(which(sapply(
      setdiff(names(uniq_data), c(id_var, y_var)),
      function(x) nrow(unique(uniq_data[c(id_var, x)]))
    ) > uniq_id))
    # how these variables will be handled
    if (is.logical(distill)) {
      if (distill) {
        distill <- most
      } else {
        warning("The following variables vary within `id`s ",
                "and will be dropped: ",
                paste(distill_vars, collapse = ", "))
        distill <- NULL
      }
    } else if (is.character(distill)) {
      distill <- get(distill)
    }
    # distill these variables
    if (! is.null(distill)) {
      stopifnot(is.function(distill))
      message("Distilled variables: ",
              paste(distill_vars, collapse = ", "))
      distill_data <- stats::aggregate(
        data[distill_vars],
        data[id_var],
        distill
      )
      #if (length(distill_vars) == 1) names(distill_data)[-1] <- distill_vars
    }
  } else {
    distill <- NULL
    distill_vars <- NULL
  }

  const_vars <- setdiff(names(data),
                        c(key_var, value_var, id_var, y_var, distill_vars))
  if (! is.null(distill) && ! is.null(y_var)) distill_data[[y_var]] <- NULL
  # `pivot_wider()` by designated `key` and `value` (and possibly `y`)
  y_const <- nrow(unique(data[c(id_var, y_var)])) == uniq_id
  id_vars <- c(id_var, const_vars, if (y_const) y_var)
  values_vars <- c(value_var, if (! y_const) y_var)
  res <- tidyr::pivot_wider(data,
                            id_cols = all_of(id_vars),
                            names_from = all_of(key_var),
                            names_sep = "_",
                            values_from = all_of(values_vars))
  if (! y_const && value_var != id_var) {
    axes <- match(paste(value_var, unique(data[[key_var]]), sep = "_"),
                  names(res))
    names(res)[axes] <- as.character(unique(data[[key_var]]))
  }

  # merge in `distill_data`
  if (is.null(distill)) {
    res <- as.data.frame(res)
  } else {
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
    out <- vars_select(data_vars, !!! vars)
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
