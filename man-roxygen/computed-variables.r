#' @section Computed variables:
#' These can be used with [ggplot2::after_stat()] to [control aesthetic
#' evaluation](https://ggplot2.tidyverse.org/reference/aes_eval.html).
#' \describe{
#'   \item{n}{number of cases in lode}
#'   \item{count}{cumulative weight of lode}
#'   \item{prop}{weighted proportion of lode}
#'   \item{stratum}{value of variable used to define strata}
#'   \item{lode}{lode label distilled from alluvia
#'               (`stat_alluvium()` and `stat_flow()` only)}
#'   \item{position}{`"front"` or `"back"` of each flow
#'                   (`stat_flow()` only)}
#' }
#' The numerical variables are grouped by `x` and weighted by `weight`. The
#' character variable `lode` is obtained from `alluvium` according to `distill`.
