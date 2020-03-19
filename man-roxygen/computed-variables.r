#' @section Computed variables:
#' These can be used with [ggplot2::after_stat()] to [control aesthetic
#' evaluation](https://ggplot2.tidyverse.org/reference/aes_eval.html).
#' \describe{
#'   \item{n}{number of cases in lode}
#'   \item{count}{cumulative weight of lode}
#'   \item{prop}{weighted proportion of lode}
#' }
#' Each variable is grouped by `x` and weighted by `weight`.
