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
#' - `order`
#' - `group`
#' - `label`
#'

#' `y` controls the heights of the alluvia,
#' and may be aggregated across equivalent observations.

#' `weight` applies to the computed variables (see that section below)
#' but does not affect the positional aesthetics.

#' `order`, recognized by `stat_alluvium()` and `stat_flow()`, is used to
#' arrange the lodes within each stratum. It tolerates duplicates and takes
#' precedence over the differentiation aesthetics (when `aes.bind` is not
#' `"none"`) and lode guidance with respect to the remaining axes. (It replaces
#' the deprecated parameter `lode.ordering`.)

#' `group` is used internally; arguments are ignored.

#' `label` is used to label the strata or lodes and must take a unique value
#' across the observations within each stratum or lode.

#' 
#' These and any other aesthetics are aggregated as follows:
#' Numeric aesthetics, including `y`, are summed.
#' Character and factor aesthetics, including `label`,
#' are assigned to strata or lodes provided they take unique values across the
#' observations within each (and are otherwise assigned `NA`).
#'
