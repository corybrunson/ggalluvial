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
#' @param data Data frame in lode form (see \code{\link{to_lodes}}).
#' @param key,value,id Numeric or character; the fields of \code{data}
#'   corresponding to the axis (variable), stratum (value), and alluvium
#'   (identifying) variables.
#' @param weight Optional numeric or character; the fields of \code{data}
#'   corresponding to alluvium or lode weights (heights when plotted). The
#'   default value \code{NULL} assigns every alluvium equal weight.
#' @param free.strata Logical; whether to allow the same strata to be ordered
#'   differently at different axes.
#' @param objective Character, the objective function to minimize; matched to
#'   \code{"count"}, \code{"weight"}, or \code{"area"}.
#' @param method Character; whether to exhaust all permutations of all axes
#'   (\code{"exhaustive"}) versus to perform heuristic optimization from random
#'   initial permutations (\code{"heuristic"}).
#' @param niter Positive integer; if \code{method == "heuristic"}, the number of
#'   iterations to perform from randomly sampled seed permutation sets.
#' @param reverse Logical; if \code{decreasing} is \code{NA},
#'   whether to arrange the strata at each axis
#'   in the reverse order of the variable values,
#'   so that they match the order of the values in the legend.
#'   Ignored if \code{decreasing} is not \code{NA}.
#'   Defaults to \code{TRUE}.
#' @export
optimize_strata <- function(
  data,
  key, value, id,
  weight = NULL,
  free.strata = FALSE,
  objective = "count", method = "exhaustive", niter = 6,
  reverse = TRUE
) {
  
  if (!is_alluvial_lodes(data = data, key = key, value = value, id = id,
                         weight = weight, logical = TRUE)) {
    stop("Data must be in 'lodes' format.")
  }
  
  # governing parameters
  xs <- sort(unique(data[[key]]))
  n_cats <- sapply(xs, function(x) {
    length(unique(data[data[[key]] == x, ][[value]]))
  })
  
  # convert stratum values to numeric by axis
  #data[[value]] <- contiguate(data[[value]])
  #for (i in seq_along(xs)) {
  #  data[[value]][data[[key]] == xs[i]] <- match(
  #    data[[value]][data[[key]] == xs[i]],
  #    sort(unique(data[[value]][data[[key]] == xs[i]]))
  #  )
  #}
  
  # objective function
  objective_fun <- get(paste0("objective_", match.arg(objective, c(
    "count", "weight", "area"
  ))))
  perms <- lapply(n_cats, seq_len)
  obj <- objective_fun(data, key, value, id, perms)
  
  method <- match.arg(method, c("exhaustive", "heuristic"))
  if (method == "exhaustive") {
    rev_perms <- lapply(lapply(n_cats, seq_len), rev)
    repeat {
      if (identical(new_perms, rev_perms)) break
      new_perms <- increment_permutations(perms)
      new_obj <- objective_fun(data, key, value, id, new_perms)
      if (new_obj < obj) {
        perms <- new_perms
        obj <- new_obj
      }
    }
  } else {
    for (i in seq_along(niter)) {
      init <- lapply(n_cats, sample)
      res <- optimize_strata_alluvia(data, key, value, id, objective_fun, init)
      if (res$obj < obj) {
        perms <- res$perms
        obj <- res$obj
      }
    }
  }
  
  # replace with reversal if closer to original
  perm_lens <- sapply(perms, permutation_length)
  rev_perms <- lapply(perms, rev)
  rev_perm_lens <- sapply(rev_perms, permutation_length)
  # CHECK THIS DURING TESTING
  if ((reverse & rev_perm_lens > perm_lens) |
      (!reverse & perm_lens > rev_perm_lens)) {
    perms <- rev_perms
  }
  
  perms
}

increment_permutations <- function(perms) {
  
}

optimize_strata_alluvia <- function(data, key, value, id, objective_fun, init) {
  
  # test every adjacent transposition for a lower objective function value
  repeat {
    step <- FALSE
    perms <- init
    obj <- objective_fun(data, key, value, id, perms)
    for (i in seq_along(init)) {
      for (j in seq_along(init[[i]])[-1]) {
        new_perms <- init
        new_perms[[i]][c(j - 1, j)] <- new_perms[[i]][c(j, j - 1)]
        new_obj <- objective_fun(data, key, value, id, new_perms)
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

# RESUME HERE
# DELETE AFTER ADAPTING TO SPECIFIC OBJECTIVE FUNCTIONS
objective_fun <- function(data, key, value, id, perms) {
  xs <- sort(unique(data[[key]]))
  for (i in seq_along(xs)) {
    this_axis <- which(data[[key]] == xs[i])
    these_values <- sort(unique(data[[value]][this_axis]))
    data[[value]][this_axis] <-
      these_values[perms[[i]][as.numeric(droplevels(data[[value]][this_axis]))]]
  }
  obj <- 0
  for (i in seq_along(xs)[-1]) {
    d_left <- data[data[[key]] == xs[i - 1], c(id, value)]
    p_left <- d_left[[id]][order(d_left[[value]])]
    d_right <- data[data[[key]] == xs[i], c(id, value)]
    p_right <- d_right[[id]][order(d_right[[value]])]
    
  }
}

objective_count <- function(data, key, value, id, perms) {
  
}

objective_weight <- function(data, key, value, id, perms) {
  
}

objective_area <- function(data, key, value, id, perms) {
  
}

permutation_length <- function(perm) {
  pairs <- combn(perm, 2)
  sum(pairs[1, ] > pairs[2, ])
}
