#' @section Aesthetics:
#' `stat_alluvium`, `stat_flow`, and `stat_stratum` require one
#' of two sets of aesthetics:
#' \itemize{
#'   \item **`x`** and at least one of
#'         **`alluvium`** and **`stratum`**
#'   \item any number of **`axis[0-9]*`**
#'         (`axis1`, `axis2`, etc.)
#' }
#' Use `x`, `alluvium`, and/or `stratum` for data in lodes format
#' and `axis[0-9]*` for data in alluvia format
#' (see [`alluvial-data`]).
#' Arguments to parameters inconsistent with the format will be ignored.
#' Additionally, each `stat_*` layer accepts the following optional
#' aesthetics:
#' \itemize{
#'   \item `y`
#'   \item `group`
#' }
#' `y` controls the heights of the alluvia
#' and may be aggregated across equivalent observations.
#' `group` is used internally; arguments are ignored.
#' Finally, `stat_stratum` accepts the following optional aesthetic:
#' \itemize{
#'   \item `label`
#' }
#' `label` is used to label the strata and must take a unique value across
#' the observations within each stratum.
#' These and any other aesthetics are aggregated as follows:
#' Numeric aesthetics, including `y`, are summed.
#' Character and factor aesthetics, including `label`,
#' are assigned to strata provided they take unique values across the
#' observations within each stratum (otherwise `NA` is assigned).
#'
