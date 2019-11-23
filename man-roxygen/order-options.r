#' @section Package options:
#' `stat_stratum`, `stat_alluvium`, and `stat_flow` order strata and lodes
#' according to the values of several parameters, which must be held fixed
#' across every layer in an alluvial plot. These package-specific options set
#' global values for these parameters that will be defaulted to when not
#' manually set:
#' 
#' - `ggalluvial.decreasing` (each `stat_*`): defaults to `NA`.
#' - `ggalluvial.reverse` (each `stat_*`): defaults to `TRUE`.
#' - `ggalluvial.absolute` (each `stat_*`): defaults to `TRUE`.
#' - `ggalluvial.cement.alluvia` (`stat_alluvium`): defaults to `FALSE`.
#' - `ggalluvial.lode.guidance` (`stat_alluvium`): defaults to `"zigzag"`.
#' - `ggalluvial.lode.ordering` (`stat_alluvium`): defaults to `NULL`.
#' - `ggalluvial.aes.bind` (`stat_alluvium` and `stat_flow`): defaults to
#'   `"none"`.
#' 
#' See [base::options()] for how to use options.
#' 
