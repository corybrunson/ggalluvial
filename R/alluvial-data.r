#' @title Test for alluvial structure and convert between alluvial formats
#'
#' @description
#'
#' The functions `is_*_form()` determine whether a data frame is alluvial---that
#' it has either the id--key--value structure of lodes format or the tidy
#' properties of alluvia format. The functions `to_*_form()` convert data from
#' one format to the other.
#' 

#' @details
#'
#' Alluvial plots consist of multiple horizontally-distributed columns (axes)
#' representing categorical variables, vertical divisions (strata) of these axes
#' representing these variables' values; and ribbons (alluvial flows) connecting
#' vertical subdivisions (lodes) within strata of adjacent axes, representing
#' subsets or amounts of observations that take the corresponding values of the
#' corresponding variables. **ggalluvial** can generate an alluvial plot from
#' data in either of two recognized formats:
#' 

#' - **Lodes format** encodes _one row per measurement_ or _one row per lode_,
#' using an `id`--`key`--`value` structure: Each row contains the recorded
#' `value` of one measurement (the `key`) for one subject or cohort (the `id`).
#' Visually, the lode is the unique intersection of one alluvium (the `id`) and
#' one axis (the `key`) and belongs to a unique stratum (the `value`).
#' Additional columns may contain lode-level variables, including magnitudes and
#' weights.

#' - **Alluvia format** encodes _one row per subject_ or _one row per alluvium_,
#' using a [tidy](https://tidyr.tidyverse.org/) structure: Each subject (or
#' cohort) has a row, each measurement has a column, and each measured value is
#' an entry. Additional columns may contain subject-level variables, including
#' magnitudes and weights; but lode-level magnitudes and weights may also be
#' encoded as column ranges having the same cardinality as those of the
#' measurements.

#'
#' The functions `is_*_form()` test for these structures. Both formats may
#' include missing values, by omission (lodes format) or as `NA`s (both). See
#' the primary vignette for several illustrations: `vignette("ggalluvial)`.
#'
#' The two formats are related by a specialized pair of pivot operations.
#' `to_lodes_form()` tests a data frame for alluvia format and then converts it
#' to lodes format; `to_alluvia_form()` does the reverse. Some information may
#' be lost under either operation, and the parameters `diffuse`, `discern`, and
#' `distill` give the user some control over how additional variables are
#' handled.
#'

#' @name alluvial-data
#' @import tidyselect
#' @family alluvial data operations
#' @param data A data frame.
#' @param silent Whether to print explanatory messages.
#' @param alluvia_from,axes_from,strata_from Columns---one each---containing the
#'   alluvium (identifier), axis (key), and stratum (value) of each row,
#'   analogous to `id_cols,names_from,values_from` in [tidyr::pivot_wider()].
#' @param axes Columns containing the stratum (entry) of each alluvium (row) at
#'   each axis (column), analogous to `cols` in [tidyr::pivot_longer()].
#' @param alluvia_to,axes_to,strata_to Strings---one each---specifying the names
#'   of the columns to contain alluvia (identifiers), axes (keys), and strata
#'   (values).
#' @param id,key,value Deprecated aliases for `alluvia_*,axes_*,strata_*`. While
#'   they still accept unquoted names, note that their `*_to` replacements only
#'   accept strings.
#' @param y Optional column containing alluvium heights (subject magnitudes).
#' @param y_to A string specifying the name of the column to create from the
#'   column(s) identified by `y`. If needed for multiple columns but not
#'   provided, defaults to 'y'.
#' @param diffuse Fields of `data` to merge into the lengthened data, joining by
#'   `alluvia_to`. They must be among the variables passed to `axes`.
#'   Alternatively, a logical value indicating whether to merge all (`TRUE`) or
#'   none (`FALSE`) of these variables.
#' @param distill A logical value indicating whether variables excluded from
#'   `axes_from` and `strata_from` that vary within values of `alluvia_from`
#'   should be included in the result. Alternatively, a function (or its name)
#'   to be used to distill each such variable to a single value. In addition to
#'   existing functions, `distill` accepts the character values `"first"` (used
#'   if `distill` is `TRUE`), `"last"`, and `"most"` (which returns the first
#'   modal value).
#' @param discern Logical value indicating whether to suffix values of the
#'   variables passed to `axes` that appear at more than one axis, in order to
#'   distinguish their factor levels. This forces the levels of the combined
#'   factor variable `strata_to` to be in the order of the axes.
#' @example inst/examples/ex-alluvial-data.r

#' @rdname alluvial-data
#' @export
is_lodes_form <- function(data,
                          alluvia_from, axes_from, strata_from,
                          key, value, id,
                          y = NULL,
                          silent = FALSE) {
  
  if (! missing(id)) {
    deprecate_parameter("id", "alluvia_from")
    if (missing(alluvia_from)) {
      alluvia_from <- rlang::quo_name(rlang::enexpr(id))
    }
  }
  if (! missing(key)) {
    deprecate_parameter("key", "axes_from")
    if (missing(axes_from)) {
      axes_from <- rlang::quo_name(rlang::enexpr(key))
    }
  }
  if (! missing(value)) {
    deprecate_parameter("value", "strata_from")
    if (missing(strata_from)) {
      strata_from <- rlang::quo_name(rlang::enexpr(value))
    }
  }
  
  alluv_var <- vars_pull(names(data), !! enquo(alluvia_from))
  axis_var <- vars_pull(names(data), !! enquo(axes_from))
  strat_var <- vars_pull(names(data), !! enquo(strata_from))
  if (axis_var %in% c(strat_var, alluv_var)) {
    stop("`axes_from` must be distinct from `alluvia_from` and `strata_from`.")
  }
  
  if (any(duplicated(cbind(data[c(alluv_var, axis_var)])))) {
    if (! silent) warning("Some id-key (alluvium-axis) pairs are duplicated.")
  }
  
  n_pairs <-
    dplyr::n_distinct(data[axis_var]) * dplyr::n_distinct(data[alluv_var])
  if (nrow(data) < n_pairs) {
    if (! silent) warning("Some id-key (alluvium-axis) pairs are missing.")
  }
  
  # if `y` is not `NULL`, use non-standard evaluation to identify `y_var`
  if (! is.null(rlang::enexpr(y))) {
    y_var <- vars_select(unique(names(data)), !! enquo(y))
    if (length(y_var) > 0) {
      if (length(y_var) > 1) stop("`y` must be a single variable.")
      if (y_var %in% c(axis_var, strat_var, alluv_var)) {
        stop("`y` must be distinct from id-key-value variables.")
      }
      if (! is.numeric(data[[y_var]])) {
        if (! silent) message("Lode y (height) values are non-numeric.")
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
  if (is.null(rlang::enexpr(y))) {
    y_var <- NULL
  } else {
    y_var <- vars_select(unique(names(data)), !! enquo(y))
    if (! all(sapply(data[y_var], is.numeric))) {
      if (! silent) message("Some lode y (height) values are non-numeric.")
      return(FALSE)
    }
  }
  
  if (is.null(rlang::enexpr(axes))) {
    axes <- setdiff(names(data), y_var)
  } else {
    axes <- enquo(axes)
    axes <- unname(vars_select(unique(names(data)), !! enquo(axes)))
  }
  if (length(y_var) > 1 && length(y_var) != length(axes)) {
    if (! silent) message("The number of y (height) columns, if > 1, ",
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
                          alluvia_to = "alluvium",
                          axes_to = "x",
                          strata_to = "stratum",
                          key = NULL, value = NULL, id = NULL,
                          y_to = NULL,
                          diffuse = FALSE, discern = FALSE) {
  
  if (! is.null(id)) {
    deprecate_parameter("id", "alluvia_to")
    alluvia_to <- rlang::quo_name(rlang::enexpr(id))
  }
  if (! is.null(key)) {
    deprecate_parameter("key", "axes_to")
    axes_to <- rlang::quo_name(rlang::enexpr(key))
  }
  if (! is.null(value)) {
    deprecate_parameter("value", "strata_to")
    strata_to <- rlang::quo_name(rlang::enexpr(value))
  }
  
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
  
  if (is.null(rlang::enexpr(axes))) {
    axes <- setdiff(names(data), y_var)
  } else {
    axes <- unname(vars_select(unique(names(data)), !! enquo(axes)))
  }
  if (length(y_var) > 1 && length(y_var) != length(axes)) {
    message("The number of `y` (height) columns, if > 1, ",
            "must equal the number of `axes`.")
    return(FALSE)
  }
  
  stopifnot(is_alluvia_form(data,
                            axes = all_of(axes), y = all_of(y_var),
                            silent = TRUE))
  
  if (! is.data.frame(data)) data <- as.data.frame(data)
  
  if (is.logical(rlang::enexpr(diffuse))) {
    diffuse <- if (diffuse) axes else NULL
  } else {
    diffuse <- unname(vars_select(unique(names(data)), !! enquo(diffuse)))
    if (! all(diffuse %in% c(axes, y_var))) {
      stop("All `diffuse` variables must be `axes` or `y` variables.")
    }
  }
  
  # combine factor levels
  cat_levels <- unname(unlist(lapply(lapply(data[axes], as.factor), levels)))
  if (any(duplicated(cat_levels)) && is.null(discern)) {
    message("Some strata appear at multiple axes.")
  }
  if (isTRUE(discern)) {
    data <- discern_data(data, axes)
    # uniquify strata separately from `discern_data` as a validation step
    strata <- make.unique(unname(cat_levels))
  } else {
    strata <- unique(unname(cat_levels))
  }
  
  # format data in preparation for `pivot_longer()`
  data[[alluvia_to]] <- seq(nrow(data))
  if (! is.null(diffuse)) {
    diffuse_data <- data[, c(alluvia_to, diffuse), drop = FALSE]
  }
  for (i in axes) data[[i]] <- as.character(data[[i]])
  
  key_ptype <- list(factor())
  names(key_ptype) <- axes_to
  # `pivot_longer()` by `axes` (and possibly `y`)
  res <- if (length(y_var) == 0 ) {
    tidyr::pivot_longer(data,
                        cols = all_of(axes),
                        names_to = all_of(axes_to),
                        names_ptypes = key_ptype,
                        values_to = all_of(strata_to))
  } else if (length(y_var) == 1) {
    if (! is.null(y_to)) names(data)[match(y_var, names(data))] <- y_to
    tidyr::pivot_longer(data,
                        cols = all_of(axes),
                        names_to = all_of(axes_to),
                        names_ptypes = key_ptype,
                        values_to = all_of(strata_to))
  } else {
    if (is.null(y_to)) {
      message("Pivoting multiple `y` columns, y_to = 'y'.")
      y_to <- "y"
    }
    axes_var_to <- paste(strata_to, axes, sep = "___")
    y_var_to <- paste(y_to, axes, sep = "___")
    names(data)[match(axes, names(data))] <- axes_var_to
    names(data)[match(y_var, names(data))] <- y_var_to
    tidyr::pivot_longer(data,
                        cols = c(all_of(axes_var_to), all_of(y_var_to)),
                        names_to = c(".value", axes_to),
                        names_sep = "___")
  }
  res[[strata_to]] <- factor(res[[strata_to]], levels = strata)
  
  # merge in `diffuse_data`
  if (is.null(diffuse)) {
    res <- as.data.frame(res)
  } else {
    res <- merge(diffuse_data, res, by = alluvia_to,
                 all.x = FALSE, all.y = TRUE)
  }
  
  res
}

#' @rdname alluvial-data
#' @export
to_alluvia_form <- function(data,
                            alluvia_from, axes_from, strata_from,
                            key, value, id,
                            y = NULL,
                            distill = FALSE) {
  
  if (! missing(id)) {
    deprecate_parameter("id", "alluvia_from")
    if (missing(alluvia_from)) {
      alluvia_from <- rlang::quo_name(rlang::enexpr(id))
    }
  }
  if (! missing(key)) {
    deprecate_parameter("key", "axes_from")
    if (missing(axes_from)) {
      axes_from <- rlang::quo_name(rlang::enexpr(key))
    }
  }
  if (! missing(value)) {
    deprecate_parameter("value", "strata_from")
    if (missing(strata_from)) {
      strata_from <- rlang::quo_name(rlang::enexpr(value))
    }
  }
  
  alluv_var <- vars_pull(names(data), !! enquo(alluvia_from))
  axis_var <- vars_pull(names(data), !! enquo(axes_from))
  strat_var <- vars_pull(names(data), !! enquo(strata_from))
  if (axis_var %in% c(strat_var, alluv_var)) {
    stop("`axes_from` must be distinct from `alluvia_from` and `strata_from`.")
  }
  if (strat_var == alluv_var) {
    message("Duplicating column '", strat_var,
            "' passed to `alluvia_from` and `strata_from`.")
    # use nonsense prefix (not suffix, to avoid conflict with `sep = "_"`)
    strat_var <- paste0("___", strat_var)
    data[[strat_var]] <- data[[alluv_var]]
  }
  
  y_var <- if (is.null(rlang::enexpr(y))) NULL else {
    vars_pull(names(data), !! enquo(y))
  }
  stopifnot(is_lodes_form(data,
                          alluvia_from = alluv_var,
                          axes_from = axis_var,
                          strata_from = strat_var,
                          y = y_var,
                          silent = TRUE))
  
  # whether any variables vary within `id`s
  uniq_id <- dplyr::n_distinct(data[[alluv_var]])
  uniq_data <- unique(data[setdiff(names(data), c(axis_var, strat_var, y_var))])
  if (! uniq_id == nrow(uniq_data)) {
    # which variables vary within `id`s
    distill_vars <- names(which(sapply(
      setdiff(names(uniq_data), c(alluv_var, y_var)),
      function(x) nrow(unique(uniq_data[c(alluv_var, x)]))
    ) > uniq_id))
    # how these variables will be handled
    if (is.logical(distill)) {
      if (distill) {
        message("Variables that vary within alluvia ",
                "will be distilled using `most()`.")
        distill <- most
      } else {
        warning("The following variables vary within alluvia ",
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
        data[alluv_var],
        distill
      )
      #if (length(distill_vars) == 1) names(distill_data)[-1] <- distill_vars
    }
  } else {
    distill <- NULL
    distill_vars <- NULL
  }
  
  const_vars <- setdiff(names(data),
                        c(axis_var, strat_var, alluv_var, y_var, distill_vars))
  if (! is.null(distill) && ! is.null(y_var)) distill_data[[y_var]] <- NULL
  # `pivot_wider()` by `axes_from` and `strata_from` (and possibly `y`)
  y_const <- nrow(unique(data[c(alluv_var, y_var)])) == uniq_id
  ids_vars <- c(alluv_var, const_vars, if (y_const) y_var)
  values_vars <- c(strat_var, if (! y_const) y_var)
  res <- tidyr::pivot_wider(data,
                            id_cols = all_of(ids_vars),
                            names_from = all_of(axis_var),
                            names_sep = "_",
                            values_from = all_of(values_vars))
  if (! y_const && strat_var != alluv_var) {
    axes <- match(paste(strat_var, unique(data[[axis_var]]), sep = "_"),
                  names(res))
    names(res)[axes] <- as.character(unique(data[[axis_var]]))
  }
  
  # merge in `distill_data`
  if (is.null(distill)) {
    res <- as.data.frame(res)
  } else {
    res <- merge(distill_data, res, by = alluv_var, all.x = FALSE, all.y = TRUE)
  }
  
  res
}

# distilling functions
first <- dplyr::first
last <- dplyr::last
most <- function(x) {
  x[which(factor(x) == names(which.max(table(factor(x)))))[1]]
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
