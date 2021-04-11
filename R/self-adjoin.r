#' Adjoin a dataset to itself
#'
#' This function binds a dataset to itself along adjacent pairs of a `key`
#' variable. It is invoked by [geom_flow()] to convert data in lodes
#' form to something similar to alluvia form.
#'

#' `self_adjoin` invokes [`dplyr::mutate-joins`] functions in order to convert
#' a dataset with measures along a discrete `key` variable into a dataset
#' consisting of column bindings of these measures (by any `by` variables) along
#' adjacent values of `key`.
#' @name self-adjoin
#' @importFrom rlang enquo
#' @importFrom tidyselect vars_pull
#' @family alluvial data manipulation
#' @param data A data frame in lodes form (repeated measures data; see
#'   [`alluvial-data`]).
#' @param key Column of `data` indicating sequential collection; handled as in
#'   [tidyr::spread()].
#' @param by Character vector of variables to self-adjoin by; passed to
#'   [`dplyr::mutate-joins`] functions.
#' @param link Character vector of variables to adjoin. Will be replaced by
#'   pairs of variables suffixed by `suffix`.
#' @param keep.x,keep.y Character vector of variables to associate with the
#'   first (respectively, second) copy of `data` after adjoining. These
#'   variables can overlap with each other but cannot overlap with `by` or
#'   `link`.
#' @param suffix Suffixes to add to the adjoined `link` variables; passed to
#'   [`dplyr::mutate-joins`] functions.
#' @example inst/examples/ex-self-adjoin.r
#' @export
self_adjoin <- function(
  data, key, by = NULL,
  link = NULL,
  keep.x = NULL, keep.y = NULL,
  suffix = c(".x", ".y")
) {
  key_var <- vars_pull(names(data), !! enquo(key))
  
  # ensure that `key` is coercible to numeric
  #key_num <- data[[key_var]]
  #if (is.character(key_num)) key_num <- as.factor(key_num)
  #key_num <- as.numeric(key_num)
  
  # identify unique values of `key` in order
  uniq_key <- sort(unique(data[[key_var]]))
  key_num <- match(data[[key_var]], uniq_key)
  
  # select datasets `x` and `y`
  x <- transform(data, step = key_num)[, c("step", by, link, keep.x)]
  y <- transform(data, step = key_num - 1)[, c("step", by, link, keep.y)]
  
  # return inner join of `x` and `y`
  adj <- dplyr::inner_join(
    x, y,
    by = c("step", by),
    suffix = suffix
  )
  adj$step <- uniq_key[adj$step]
  adj
}
