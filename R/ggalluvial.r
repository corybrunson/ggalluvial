#' Quick alluvial diagram
#' 
#' Produces an alluvial diagram with axis strata and labels.
#' 
#' @seealso \code{\link{alluvium}} and \code{\link{stratum}}
#' @usage NULL
#' @export
#' @param ... arguments passed to \code{ggplot} and inherited by
#'   \code{geom_alluvium} and \code{geom_stratum}.
ggalluvial <- function(...) {
    input_list <- list(...)
    aes_input <- input_list[[which(sapply(input_list, class) == "uneval")]]
    axis_input <- aes_input[grep("^axis[0-9\\.]$", names(aes_input))]
    axis_breaks <- as.numeric(gsub("^axis", "", names(axis_input)))
    axis_labels <- unname(as.character(axis_input))
    ggplot(...) +
        geom_alluvium() +
        geom_stratum() +
        geom_text(stat = "stratum") +
        scale_x_continuous(breaks = axis_breaks, labels = axis_labels)
}
