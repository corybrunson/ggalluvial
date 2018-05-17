#' @section Aesthetics:
#' \code{stat_alluvium}, \code{stat_flow}, and \code{stat_stratum} require one
#' of two sets of aesthetics:
#' \itemize{
#'   \item \strong{\code{x}} and at least one of
#'         \strong{\code{alluvium}} and \strong{\code{stratum}}
#'   \item any number of \strong{\code{axis[0-9]*}}
#'         (\code{axis1}, \code{axis2}, etc.)
#' }
#' Use \code{x}, \code{alluvium}, and/or \code{stratum} for data in lodes format
#' and \code{axis[0-9]*} for data in alluvia format
#' (see \code{\link{alluvial-data}}).
#' Arguments to parameters inconsistent with the format will be ignored.
#' Additionally, each \code{stat_*} layer accepts the following optional
#' aesthetics:
#' \itemize{
#'   \item \code{weight}
#'   \item \code{group}
#' }
#' \code{weight} controls the vertical dimensions of the alluvia
#' and are aggregated across equivalent observations.
#' \code{group} is used internally; arguments are ignored.
#' Finally, \code{stat_stratum} accepts the following optional aesthetic:
#' \itemize{
#'   \item \code{label}
#' }
#' \code{label} is used to label the strata and must take a unique value across
#' the observations within each stratum.
#' These and any other aesthetics are aggregated as follows:
#' Numeric aesthetics, including \code{weight}, are summed.
#' Character and factor aesthetics, including \code{label},
#' are assigned to strata provided they take unique values across the
#' observations within each stratum (otherwise \code{NA} is assigned).
#' 
