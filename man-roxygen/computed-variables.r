#' @section Computed variables: These can be used with
#'   [`ggplot2::after_stat()`][ggplot2::aes_eval] to [control aesthetic
#'   evaluation](https://ggplot2.tidyverse.org/reference/aes_eval.html).
#' \describe{
#'   \item{`n`}{number of cases in lode}
#'   \item{`count`}{cumulative weight of lode}
#'   \item{`prop`}{weighted proportion of lode}
#'   \item{`stratum`}{value of variable used to define strata}
#'   \item{`deposit`}{order in which (signed) strata are deposited}
#'   \item{`lode`}{lode label distilled from alluvia
#'                 (`stat_alluvium()` and `stat_flow()` only)}
#'   \item{`flow`}{direction of flow `"to"` or `"from"` from its axis
#'                 (`stat_flow()` only)}
#' }

#' The numerical variables `n`, `count`, and `prop` are calculated after the
#' data are grouped by `x` and weighted by `weight` (in addition to `y`).

#' The integer variable `deposit` is used internally to sort the data before
#' calculating heights. The character variable `lode` is obtained from
#' `alluvium` according to `distill`.
