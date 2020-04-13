#' @section Aesthetics:
#' `stat_alluvium`, `stat_flow`, and `stat_stratum` require one
#' of two sets of aesthetics:
#'
#' - **`x`** and at least one of **`alluvium`** and **`stratum`**
#' - any number of **`axis[0-9]*`** (`axis1`, `axis2`, etc.)
#'
#' Use `x`, `alluvium`, and/or `stratum` for data in lodes format
#' and `axis[0-9]*` for data in alluvia format (see [`alluvial-data`]).
#' Arguments to parameters inconsistent with the format will be ignored.
#' Additionally, each `stat_*()` accepts the following optional
#' aesthetics:
#'
#' - `y`
#' - `weight`
#' - `group`
#' - `label`
#'
#' `y` controls the heights of the alluvia,
#' and may be aggregated across equivalent observations.
#' `weight` applies to the computed variables (see that section below)
#' but does not affect the positional aesthetics.
#' `group` is used internally; arguments are ignored.
#' `label` is used to label the strata or lodes and must take a unique value
#' across the observations within each stratum or lode.
#' Often the same variable will be passed to `label` as to the corresponding
#' alluvial aesthetic (`stratum` or `alluvium`).
#' 
#' These and any other aesthetics are aggregated as follows:
#' Numeric aesthetics, including `y`, are summed.
#' Character and factor aesthetics, including `label`,
#' are assigned to strata or lodes provided they take unique values across the
#' observations within each (and are otherwise assigned `NA`).
#'
