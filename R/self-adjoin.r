#' Adjoin a dataset to itself
#'
#' This function binds a dataset to itself along adjacent pairs of a \code{key}
#' variable. It is invoked by \code{\link{geom_flow}} to convert data in lodes
#' form to something similar to alluvia form.
#'

#' \code{self_adjoin} invokes \code{\link[dplyr]{join}} functions in order to
#' convert a dataset with measures along a discrete \code{key} variable into a
#' dataset consisting of column bindings of these measures (by any \code{by}
#' variables) along adjacent values of \code{key}.
#' @name self-adjoin
#' @import tidyselect
#' @family alluvial data manipulation
#' @param data A data frame in lodes form (repeated measures data; see
#'   \code{\link{alluvial-data}}).
#' @param key Column of \code{data} indicating sequential collection; handled as
#'   in \code{\link[tidyr]{spread}}.
#' @param by Character vector of variables to self-adjoin by; passed to
#'   \code{\link[dplyr]{join}} functions.
#' @param link Character vector of variables to adjoin. Will be replaced by
#'   pairs of variables suffixed by \code{suffix}.
#' @param keep.x,keep.y Character vector of variables to associate with the
#'   first (respectively, second) copy of \code{data} after adjoining. These
#'   variables can overlap with each other but cannot overlap with \code{by} or
#'   \code{link}.
#' @param suffix Suffixes to add to the adjoined \code{link} variables; passed
#'   to \code{\link[dplyr]{join}} functions.
#' @example inst/examples/ex-self-adjoin.r
#' @export
self_adjoin <- function(
  data, key, by = NULL,
  link = NULL,
  keep.x = NULL, keep.y = NULL,
  suffix = c(".x", ".y")
) {
  # ensure that `key` is coercible to numeric
  key_var <- tidyselect::vars_pull(names(data), !!rlang::enquo(key))
  key_num <- data[[key_var]]
  if (is.character(key_num)) key_num <- as.factor(key_num)
  key_num <- as.numeric(key_num)

  # self-(inner )join `link` variables by `key` and `by`
  adj <- dplyr::inner_join(
    transform(data, step = key_num)[, c("step", by, link)],
    transform(data, step = key_num - 1)[, c("step", by, link)],
    by = c("step", by),
    suffix = suffix
  )

  # bind `keep.*` variables
  if (!is.null(keep.x)) {
    adj <- dplyr::left_join(
      adj,
      transform(data, step = key_num)[, c("step", by, keep.x)],
      by = c("step", by)
    )
  }
  if (!is.null(keep.y)) {
    adj <- dplyr::left_join(
      adj,
      transform(data, step = key_num - 1)[, c("step", by, keep.y)],
      by = c("step", by)
    )
  }

  adj
}
