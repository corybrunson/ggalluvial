#' @keywords internal
#'

#' @section Acknowledgments:
#'

#' Many users identified problems and suggested improvements via email and the
#' GitHub issue tracker.
#'
#' Development benefitted from the use of equipment and the support of
#' colleagues at [UConn Health](https://health.uconn.edu/) and at [UF
#' Health](https://ufhealth.org/).
#' 
"_PACKAGE"

#' @importFrom rlang "%||%"

# stratum and lode ordering options are documented in the `stat_*()` topics
# curve options are documented in the `geom_*()` topics
op.ggalluvial <- list(
  # stratum and lode ordering
  ggalluvial.decreasing = NA,
  ggalluvial.reverse = TRUE,
  ggalluvial.absolute = TRUE,
  ggalluvial.cement.alluvia = FALSE,
  ggalluvial.lode.guidance = "zigzag",
  ggalluvial.aes.bind = "none",
  # curves
  ggalluvial.curve_type = "xspline",
  ggalluvial.curve_range = NA_real_,
  ggalluvial.segments = 48L
)

ggalluvial_opt <- function(x) {
  x_ggalluvial <- paste0("ggalluvial.", x)
  res <- getOption(x_ggalluvial)
  if (! is.null(res)) {
    return(res)
  }
  
  op.ggalluvial[[x_ggalluvial]]
}
