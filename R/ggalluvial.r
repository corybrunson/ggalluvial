#' Quick alluvial diagram
#' 
#' Produces an alluvial diagram with axis strata and labels.
#' 
#' @seealso \code{\link{geom_alluvium}} and \code{\link{geom_stratum}}
#' @export
#' @param ... arguments passed to \code{ggplot} and inherited by
#'   \code{geom_alluvium} and \code{geom_stratum}.

ggalluvial <- function(...) {
    ggplot(...) + geom_alluvium() + geom_stratum() + geom_text(stat = "stratum")
}
