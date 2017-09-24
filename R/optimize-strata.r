#' Optimize strata
#' 
#' This function attempts to order the strata at each axis so as to maximize the
#' readability of the diagram, by minimizing an objective function related to
#' the number and sizes of the overlaps between flows when they cross. The
#' planned objective functions include the following:
#' \itemize{
#'   \item \code{"count"}: the number of crossings
#'   \item \code{"weight"}: the total weight of the crossings
#'   (the weight of each crossing is the product of the weights of the flows)
#'   \item \code{"area"}: the total area of the crossings
#'   (weights have less area when the flows intersect at greater angles)
#' }
#' @param data Data frame.
#' @param ... Additional parameters used to determine method and passed
#'   thereto. All or none of \code{key}, \code{value}, and \code{id}, or else
#'   optionally \code{axes}, and (in either case) optionally \code{weight}.
#' @param objective Character, the objective function to minimize; matched to
#'   \code{"count"}, \code{"weight"}, or \code{"area"}.
#' @param niter Positive integer; the number of iterations to perform from
#'   randomly sampled seed permutation sets.
#' @export
optimize_strata <- function(data, ..., objective = "count", niter = 10) {
  
  type <- is_alluvial(data, ..., logical = FALSE)
  if (type == "none") {
    stop("Data is not in a recognized alluvial form ",
         "(see `help(is_alluvial)` for details).")
  }
  
  # ensure that data is in lode form
  if (type == "alluvia") {
    data <- to_lodes(data = data,
                     key = "x", value = "stratum", id = "alluvium",
                     axes = 1:ncol(data))
    # positioning requires numeric 'x'
    data$x <- as.numeric(as.factor(data$x))
  }
  
  # governing parameters
  obs <- sort(unique(data$x))
  ncats <- sapply(obs, function(x) {
    length(unique(data[data$x == x, ]$stratum))
  })
  
  # iterate optimization procedure
  res <- list(
    perms = lapply(ncats, seq_along),
    obj = Inf
  )
  for (i in 1:length(obs)) {
    init <- lapply(ncats, sample)
    new_res <- optimize_strata_alluvia(data, objective, init)
    if (new_res$obj < res$obj) {
      res <- new_res
    }
  }
  res$perms
}

optimize_strata_alluvia <- function(data, objective, init) {
  
  # objective function
  objective_fun <- get(paste0("objective_", match.arg(objective, c(
    "count", "weight", "area"
  ))))
  
  
  
  list(perms = perms, obj = obj)
}

objective_count <- function() {
  
}

objective_weight <- function() {
  
}

objective_area <- function() {
  
}
