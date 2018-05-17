#' Deprecated functions
#' 
#' These functions are deprecated in the current version and may be removed in a
#' future version.
#' 
#' @name ggalluvial-deprecated
#' @keywords internal
NULL

deprecate_parameter <- function(old, new = NA) {
  .Deprecated(msg = paste0(
    "The parameter `", old, "` is deprecated.",
    if (is.null(new)) {
      "\nPass unparameterized arguments instead."
    } else if (!is.na(new)) {
      paste0("\nPass arguments to `", new, "` instead.")
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
