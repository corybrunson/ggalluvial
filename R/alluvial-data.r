#' @title Test for alluvial structure and convert between alluvial formats
#'
#' @description
#'
#' The functions `is_*_form()` determine whether a data frame is alluvial---that
#' it has either the id--key--value structure of lodes format or the tidy
#' structure of alluvia format. The functions `to_*_form()` convert data from
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
#' using an id--key--value structure: Each row contains the recorded value of
#' one measurement (the key) for one subject or cohort (the id). Visually, the
#' lode is the unique intersection of one alluvium (the id) and one axis (the
#' key) and belongs to a unique stratum (the value). Additional columns may
#' contain lode-level variables, including magnitudes and weights.

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
#' @param alluvia_from,axes_from,strata_from Three arguments describing which
#'   columns contain the alluvium (identifier), axis (key), and stratum (value)
#'   of each row. These are analogous (and eventually passed) to
#'   `id_cols,names_from,values_from` in [tidyr::pivot_wider()] and, as there,
#'   handled using [`rlang::enquo()`][rlang::nse-defuse].
#' @param axes An argument describing which columns contain the stratum (entry)
#'   of each alluvium (row) at each axis (column). This is analogous (and
#'   eventually passed) to `cols` in [tidyr::pivot_longer()] and, as there,
#'   handled using [`rlang::enquo()`][rlang::nse-defuse].
#' @param alluvia_to,axes_to,strata_to Three strings specifying the names of the
#'   new columns to create for the alluvia (identifiers), axes (keys), and
#'   strata (values). The latter two are analogous (and eventually passed) to
#'   `names_to,values_to` in [tidyr::pivot_longer()].
#' @param axes_prefix Passed to `names_prefix` in [tidyr::pivot_longer()] or
#'   [tidyr::pivot_wider()].
#' @param axes_sep Passed to `names_sep` in [tidyr::pivot_wider()].
#' @param strata_drop_na Passed to `values_drop_na` in [tidyr::pivot_longer()].
#' @param strata_fill Passed to `values_fill` in [tidyr::pivot_wider()].
#' @param key,value,id Deprecated aliases for `axes_*,strata_*,alluvia_*`,
#'   respectively. While they still accept unquoted names, note that their
#'   `*_to` replacements only accept strings. If values are passed to both, then
#'   these take precedence.
#' @param y,weight Optional column(s) containing alluvium heights (subject
#'   magnitudes) and weights (used in computed variables).
#' @param y_to,weight_to Single strings specifying the names of the columns to
#'   create from the column(s) identified by `y` or `weight`. If needed (for
#'   multiple columns passed to `y` or to `weight`) but not provided, default
#'   to `'y'` and `'weight'`, respectively.
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
                          y = NULL, weight = NULL,
                          key, value, id,
                          silent = FALSE) {
  
  # if old parameters are used, override new parameters
  if (! missing(id) || ! missing(key) || ! missing(value)) {
    deprecate_parameter(c("id", "key", "value"),
                        c("alluvia_from", "axes_from", "strata_from"),
                        msg = "Deprecated parameters will be used this time.")
  }
  # use tidy selection (when parameters are not null)
  alluvia_from <- if (missing(id)) {
    names(tidyselect::eval_select(enquo(alluvia_from), data))
  } else {
    names(tidyselect::eval_select(enquo(id), data))
  }
  axes_from <- if (missing(key)) {
    names(tidyselect::eval_select(enquo(axes_from), data))
  } else {
    names(tidyselect::eval_select(enquo(key), data))
  }
  strata_from <- if (missing(value)) {
    names(tidyselect::eval_select(enquo(strata_from), data))
  } else {
    names(tidyselect::eval_select(enquo(value), data))
  }
  y <- names(tidyselect::eval_select(enquo(y), data))
  if (length(y) == 0L) y <- NULL
  weight <- names(tidyselect::eval_select(enquo(weight), data))
  if (length(weight) == 0L) weight <- NULL
  
  if (axes_from %in% c(strata_from, alluvia_from)) {
    stop("`axes_from` must be distinct from `alluvia_from` and `strata_from`.")
  }
  
  if (any(duplicated(cbind(data[c(alluvia_from, axes_from)])))) {
    if (! silent) warning("Some id-key (alluvium-axis) pairs are duplicated.")
    return(FALSE)
  }
  
  n_pairs <-
    dplyr::n_distinct(data[axes_from]) * dplyr::n_distinct(data[alluvia_from])
  if (nrow(data) < n_pairs) {
    if (! silent) warning("Some id-key (alluvium-axis) pairs are missing.")
  }
  
  # if `y` or `weight` is not `NULL`, use non-standard evaluation to identify it
  if (! is.null(y) || ! is.null(weight)) {
    if (length(y) > 0L || length(weight) > 0L) {
      if (length(y) > 1L || length(weight) > 1L)
        stop("`y` and `weight`, if provided, must be single variables.")
      if (any(c(y, weight)  %in% c(axes_from, strata_from, alluvia_from)))
        stop("`y` or `weight` must be distinct from id-key-value variables.")
      if ( (! is.null(y) && ! is.numeric(data[[y]])) ||
           (! is.null(weight) && ! is.numeric(data[[weight]])) ) {
        if (! silent) message("Lode `y` or `weight` values are non-numeric.")
        return(FALSE)
      }
    }
  }
  
  TRUE
}

#' @rdname alluvial-data
#' @export
is_alluvia_form <- function(data,
                            axes = NULL, y = NULL, weight = NULL,
                            silent = FALSE) {
  
  # use tidy selection (when parameters are not null)
  y <- names(tidyselect::eval_select(enquo(y), data))
  if (length(y) == 0L) y <- NULL
  weight <- names(tidyselect::eval_select(enquo(weight), data))
  if (length(weight) == 0L) weight <- NULL
  axes <- names(tidyselect::eval_select(enquo(axes), data))
  if (length(axes) == 0L) axes <- setdiff(names(data), c(y, weight))
  
  # note: specifying `y` is optional
  if (! is.null(y)) {
    if (! all(sapply(data[y], is.numeric))) {
      if (! silent) message("Some lode `y` values are non-numeric.")
      return(FALSE)
    }
  }
  if (! is.null(weight)) {
    if (! all(sapply(data[weight], is.numeric))) {
      if (! silent) message("Some lode `weight` values are non-numeric.")
      return(FALSE)
    }
  }
  
  if ((length(y) > 1 && length(y) != length(axes)) ||
      (length(weight) > 1 && length(weight) != length(axes))) {
    if (! silent)
      message("`y` and `weight` must have length either 1 or `length(axes)`.")
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
                          axes = NULL, y = NULL, weight = NULL,
                          alluvia_to = "alluvium",
                          axes_to = "x", axes_prefix = NULL,
                          strata_to = "stratum", strata_drop_na = TRUE,
                          y_to = "y", weight_to = "weight",
                          key = NULL, value = NULL, id = NULL,
                          diffuse = FALSE, discern = FALSE) {
  
  # use tidy selection (when parameters are not null)
  y <- names(tidyselect::eval_select(enquo(y), data))
  if (length(y) == 0L) y <- NULL
  weight <- names(tidyselect::eval_select(enquo(weight), data))
  if (length(weight) == 0L) weight <- NULL
  axes <- names(tidyselect::eval_select(enquo(axes), data))
  if (length(axes) == 0L) axes <- setdiff(names(data), c(y, weight))
  diffuse <- if (is.logical(enexpr(diffuse))) {
    if (diffuse) axes else NULL
  } else {
    names(tidyselect::eval_select(enquo(diffuse), data))
  }
  if (length(diffuse) == 0L) diffuse <- NULL
  if (! all(diffuse %in% c(axes, y, weight)))
    stop("All `diffuse` variables must be among `axes`, `y`, or `weight`.")
  
  # if old parameters are used, override new parameters
  if (! is.null(id) || ! is.null(key) || ! is.null(value)) {
    deprecate_parameter(c("id", "key", "value"),
                        c("alluvia_to", "axes_to", "strata_to"),
                        msg = "Deprecated parameters will be used this time.")
  }
  alluvia_to <- if (is.null(id)) alluvia_to else quo_name(enexpr(id))
  axes_to <- if (is.null(key)) axes_to else quo_name(enexpr(key))
  strata_to <- if (is.null(value)) strata_to else quo_name(enexpr(value))
  if (! is.null(data[[alluvia_to]]))
    stop("Column '", alluvia_to, "' already exists.")
  
  stopifnot(is_alluvia_form(data,
                            axes = all_of(axes),
                            y = all_of(y), weight = all_of(weight),
                            silent = TRUE))
  
  if (! is.data.frame(data)) data <- as.data.frame(data)
  
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
  
  # prepare to pivot_longer()` by `axes` (and possibly `y` or `weight`)
  key_ptype <- list(factor())
  names(key_ptype) <- axes_to
  multi_pivot <- length(y) > 1L || length(weight) > 1L
  if (multi_pivot) {
    axes_suffix <- if (is.null(axes_prefix)) axes else
      gsub(paste0("^", axes_prefix), "", axes)
    axes_var_to <- paste(strata_to, axes_suffix, sep = "___")
    names(data)[match(axes, names(data))] <- axes_var_to
  }
  if (length(y) == 1L) {
    names(data)[match(y, names(data))] <- y_to
  } else if (length(y) > 1L) {
    y_var_to <- paste(y_to, axes_suffix, sep = "___")
    names(data)[match(y, names(data))] <- y_var_to
  }
  if (length(weight) == 1L) {
    names(data)[match(weight, names(data))] <- weight_to
  } else if (length(weight) > 1L) {
    weight_var_to <- paste(weight_to, axes_suffix, sep = "___")
    names(data)[match(weight, names(data))] <- weight_var_to
  }
  pivot_cols <- c(if (! multi_pivot) axes else axes_var_to,
                  if (length(y) > 1L) y_var_to,
                  if (length(weight) > 1L) weight_var_to)
  pivot_names_to <- c(if (multi_pivot) ".value", axes_to)
  pivot_names_sep <- if (multi_pivot) "___"
  pivot_values_to <- if (! multi_pivot) strata_to
  # pivot!
  res <- tidyr::pivot_longer(data,
                             cols = all_of(pivot_cols),
                             names_to = pivot_names_to,
                             names_prefix = axes_prefix,
                             names_sep = pivot_names_sep,
                             names_ptypes = key_ptype,
                             values_to = strata_to,
                             values_drop_na = strata_drop_na)
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
                            alluvia_from = "alluvium",
                            axes_from = "axis",
                            axes_prefix = "", axes_sep = "_",
                            strata_from = "stratum",
                            strata_fill = NULL,
                            y = NULL, weight = NULL,
                            key, value, id,
                            distill = FALSE) {
  
  # if old parameters are used, override new parameters
  if (! missing(id) || ! missing(key) || ! missing(value)) {
    deprecate_parameter(c("id", "key", "value"),
                        c("alluvia_from", "axes_from", "strata_from"),
                        msg = "Deprecated parameters will be used this time.")
  }
  # recover names from tidy selection (when parameters are not null)
  alluvia_from <- if (missing(id)) {
    names(tidyselect::eval_select(enquo(alluvia_from), data))
  } else {
    names(tidyselect::eval_select(enquo(id), data))
  }
  axes_from <- if (missing(key)) {
    names(tidyselect::eval_select(enquo(axes_from), data))
  } else {
    names(tidyselect::eval_select(enquo(key), data))
  }
  strata_from <- if (missing(value)) {
    names(tidyselect::eval_select(enquo(strata_from), data))
  } else {
    names(tidyselect::eval_select(enquo(value), data))
  }
  y <- names(tidyselect::eval_select(enquo(y), data))
  if (length(y) == 0L) y <- NULL
  weight <- names(tidyselect::eval_select(enquo(weight), data))
  if (length(weight) == 0L) weight <- NULL
  
  if (strata_from == alluvia_from) {
    message("Duplicating column '", strata_from,
            "' passed to `alluvia_from` and `strata_from`.")
    # use nonsense prefix (not suffix, to avoid conflict with `sep = "_"`)
    strata_from <- paste0("___", strata_from)
    data[[strata_from]] <- data[[alluvia_from]]
  }
  
  stopifnot(is_lodes_form(data,
                          alluvia_from = alluvia_from,
                          axes_from = axes_from,
                          strata_from = strata_from,
                          y = y, weight = weight,
                          silent = TRUE))
  
  # whether any variables vary within `id`s
  uniq_id <- dplyr::n_distinct(data[[alluvia_from]])
  uniq_data <- unique(data[setdiff(names(data),
                                   c(axes_from, strata_from, y, weight))])
  if (! uniq_id == nrow(uniq_data)) {
    # which variables vary within `id`s
    distill_vars <- names(which(sapply(
      setdiff(names(uniq_data), c(alluvia_from, y, weight)),
      function(x) nrow(unique(uniq_data[c(alluvia_from, x)]))
    ) > uniq_id))
    # how these variables will be handled
    if (is.logical(distill)) {
      if (distill) {
        message("Variables that vary within alluvia ",
                "will be distilled using `most()`.")
        distill <- most
      } else {
        message("The following variables vary within alluvia ",
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
        data[alluvia_from],
        distill
      )
      #if (length(distill_vars) == 1) names(distill_data)[-1] <- distill_vars
    }
  } else {
    distill <- NULL
    distill_vars <- NULL
  }
  
  const_vars <- setdiff(names(data),
                        c(axes_from, strata_from, alluvia_from,
                          y, weight, distill_vars))
  if (! is.null(distill)) {
    if (! is.null(y)) distill_data[[y]] <- NULL
    if (! is.null(weight)) distill_data[[weight]] <- NULL
  }
  # `pivot_wider()` by `axes_from` and `strata_from` (and possibly `y`)
  y_const <- nrow(unique(data[c(alluvia_from, y)])) == uniq_id
  weight_const <- nrow(unique(data[c(alluvia_from, weight)])) == uniq_id
  ids_vars <- c(alluvia_from, const_vars,
                if (y_const) y, if (weight_const) weight)
  values_vars <- c(strata_from, if (! y_const) y, if (! weight_const) weight)
  res <- tidyr::pivot_wider(data,
                            id_cols = all_of(ids_vars),
                            names_from = all_of(axes_from),
                            names_prefix = axes_prefix,
                            names_sep = axes_sep,
                            values_from = all_of(values_vars),
                            values_fill = strata_fill)
  if ((! y_const || ! weight_const) && strata_from != alluvia_from) {
    axes <- match(paste0(strata_from, axes_sep, axes_prefix,
                         unique(data[[axes_from]])),
                  names(res))
    names(res)[axes] <- paste0(axes_prefix,
                               as.character(unique(data[[axes_from]])))
  }
  
  # merge in `distill_data`
  if (is.null(distill)) {
    res <- as.data.frame(res)
  } else {
    res <- merge(distill_data, res, by = alluvia_from,
                 all.x = FALSE, all.y = TRUE)
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
