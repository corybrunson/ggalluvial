
deprecate_parameter <- function(old, new = NA, type = "parameter", msg = NULL) {
  .Deprecated(msg = paste0(
    "The ", type, " `", old, "` is deprecated.",
    if (is.null(new)) {
      "\nPass unparameterized arguments instead."
    } else if (! is.na(new)) {
      paste0("\nPass arguments to `", new, "` instead.")
    } else if (! is.null(msg)) {
      paste0("\n", msg)
    } else {
      ""
    }
  ))
}

defunct_parameter <- function(old, new = NA, type = "parameter", msg = NULL) {
  .Defunct(msg = paste0(
    "The ", type, " `", old, "` is defunct.",
    if (is.null(new)) {
      "\nPass unparameterized arguments instead."
    } else if (! is.na(new)) {
      paste0("\nPass arguments to `", new, "` instead.")
    } else if (! is.null(msg)) {
      paste0("\n", msg)
    } else {
      ""
    }
  ))
}

release_questions <- function() {
  c(
    "Have previous CRAN NOTEs been addressed?"
  )
}

#' Deprecated functions
#'
#' These functions are deprecated in the current version and may be removed in a
#' future version.
#'
#' Use `is_*_form` instead of `is_alluvial` and `is_alluvial_*`.
#' Use `to_*_form` instead of `to_*`.
#'
#' @name ggalluvial-deprecated
#' @keywords internal
NULL

#' @rdname ggalluvial-deprecated
#' @export
is_alluvial <- function(data, ..., silent = FALSE) {
  .Deprecated(msg = paste0(
    "The function `is_alluvial()` is deprecated; ",
    "use `is_lodes_form()` or `is_alluvia_form()`."
  ))
  
  # determine method based on arguments given
  dots <- lazyeval::lazy_dots(...)
  if (! is.null(dots$key) | ! is.null(dots$value) | ! is.null(dots$id)) {
    if (! is.null(dots$axes)) {
      stop("Arguments to `key`, `value`, and `id` are mutually exclusive ",
           "with an argument to `axes`.")
    }
    is_lodes_form(data = data, ..., silent = silent)
  } else {
    is_alluvia_form(data = data, ..., silent = silent)
  }
}

#' @rdname ggalluvial-deprecated
#' @export
is_alluvial_lodes <- function(...) {
  .Deprecated("is_lodes_form")
  is_lodes_form(...)
}

#' @rdname ggalluvial-deprecated
#' @export
is_alluvial_alluvia <- function(...) {
  .Deprecated("is_alluvia_form")
  is_alluvia_form(...)
}

#' @rdname ggalluvial-deprecated
#' @export
to_lodes <- function(...) {
  .Deprecated("to_lodes_form")
  to_lodes_form(...)
}

#' @rdname ggalluvial-deprecated
#' @export
to_alluvia <- function(...) {
  .Deprecated("to_alluvia_form")
  to_alluvia_form(...)
}
