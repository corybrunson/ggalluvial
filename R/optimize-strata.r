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
#'   optionally \code{axes}.
#' @param weight Optional numeric or character; the fields of \code{data}
#'   corresponding to alluvium or lode weights (heights when plotted). The
#'   default value \code{NULL} assigns every alluvium equal weight.
#' @param objective Character, the objective function to minimize; matched to
#'   \code{"count"}, \code{"weight"}, or \code{"area"}.
#' @param exhaustive Logical; whether to exhaust all permutations of all axes,
#'   versus to perform heuristic optimization from random initial permutations.
#' @param niter Positive integer; the number of iterations to perform from
#'   randomly sampled seed permutation sets.
#' @export
optimize_strata <- function(
  data, ..., weight = NULL, objective = "count", exhaustive = TRUE, niter = 10
) {
  
  if (is.numeric(weight)) weight <- names(data)[weight]
  
  type <- is_alluvial(data, ..., weight = weight, logical = FALSE)
  if (type == "none") {
    stop("Data is not in a recognized alluvial form ",
         "(see `help(is_alluvial)` for details).")
  }
  
  # ensure that data is in lode form
  if (type == "alluvia") {
    
    axes <- if (missing(axes)) {
      setdiff(names(data), weight)
    }
    
    data <- to_lodes(data = data,
                     key = "x", value = "stratum", id = "alluvium",
                     axes = axes)
    # positioning requires numeric 'x'
    data$x <- as.numeric(as.factor(data$x))
  }
  
  # governing parameters
  xs <- sort(unique(data$x))
  n_cats <- sapply(xs, function(x) {
    length(unique(data[data$x == x, ]$stratum))
  })
  
  # convert stratum values to numeric by axis
  #data$stratum <- as.numeric(as.factor(data$stratum))
  #for (i in seq_along(xs)) {
  #  data$stratum[data$x == xs[i]] <- match(
  #    data$stratum[data$x == xs[i]],
  #    sort(unique(data$stratum[data$x == xs[i]]))
  #  )
  #}
  
  # objective function
  objective_fun <- get(paste0("objective_", match.arg(objective, c(
    "count", "weight", "area"
  ))))
  perms <- lapply(n_cats, seq_len)
  obj <- objective_fun(data, perms)
  
  if (exhaustive) {
    rev_perms <- lapply(lapply(n_cats, seq_len), rev)
    repeat {
      if (identical(new_perms, rev_perms) break
          new_perms <- increment_permutations(perms)
          new_obj <- objective_fun(data, new_perms)
          if (new_obj < obj) {
            perms <- new_perms
            obj <- new_obj
          }
    }
  } else {
    for (i in seq_along(niter)) {
      init <- lapply(n_cats, sample)
      res <- optimize_strata_alluvia(data, objective_fun, init)
      if (res$obj < obj) {
        perms <- res$perms
        obj <- res$obj
      }
    }
  }
  
  perms
}

increment_permutations <- function(perms) {
  
}

optimize_strata_alluvia <- function(data, objective_fun, init) {
  
  # test every adjacent transposition for a lower objective function value
  repeat {
    step <- FALSE
    perms <- init
    obj <- objective_fun(data, perms)
    for (i in seq_along(init)) {
      for (j in seq_along(init[[i]])[-1]) {
        new_perms <- init
        new_perms[[i]][c(j - 1, j)] <- new_perms[[i]][c(j, j - 1)]
        new_obj <- objective_fun(data, new_perms)
        if (new_obj < obj) {
          perms <- new_perms
          obj <- new_obj
          step <- TRUE
        }
      }
    }
    if (step == FALSE) break
  }
  
  list(perms = perms, obj = obj)
}

objective_fun <- function(data, perms) {
  xs <- sort(unique(data$x))
  for (i in seq_along(xs)) {
    data$stratum[data$x == xs[i]] <- perms[[i]][data$stratum[data$x == xs[i]]]
  }
  obj <- 0
  for (i in seq_along(xs)[-1]) {
    d_left <- data[data$x == xs[i - 1], c("alluvium", "stratum")]
    p_left <- d_left$alluvium[order(d_left$stratum)]
    d_right <- data[data$x == xs[i], c("alluvium", "stratum")]
    p_right <- d_right$alluvium[order(d_right$stratum)]
    
  }
}

objective_count <- function(data, perms) {
  
}

objective_weight <- function(data, perms) {
  
}

objective_area <- function(data, perms) {
  
}
