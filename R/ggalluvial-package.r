#' @keywords internal
#'

#' @section Acknowledgments:
#'

#' Many users identified problems and suggested improvements via email and the
#' GitHub issue tracker.
#'
#' Development benefitted from the use of equipment and the support of
#' colleagues at [UConn Health](https://health.uconn.edu/).
#' 
"_PACKAGE"

# stratum and lode ordering options are documented in the `stat_*()` topics
op.ggalluvial <- list(
  ggalluvial.decreasing = NA,
  ggalluvial.reverse = TRUE,
  ggalluvial.absolute = TRUE,
  ggalluvial.cement.alluvia = FALSE,
  ggalluvial.lode.guidance = "zigzag",
  ggalluvial.lode.ordering = NULL,
  ggalluvial.aes.bind = "none"
)

ggalluvial_opt <- function(x) {
  x_ggalluvial <- paste0("ggalluvial.", x)
  res <- getOption(x_ggalluvial)
  if (! is.null(res)) {
    return(res)
  }
  
  op.ggalluvial[[x_ggalluvial]]
}
