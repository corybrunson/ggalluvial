#' Optimize strata
#' 
#' The function \code{optimize_strata()} attempts to order the strata at each 
#' axis so as to maximize the readability of the diagram, by minimizing an 
#' objective function related to the overlaps of crossing flows. The objective
#' functions include the following:
#' 

#' \itemize{
#'   \item \code{"count"}: the number of crossings
#'   \item \code{"weight"}: the total weight of the crossings
#'   (the weight of each crossing is the product of the weights of the flows)
#' }
#' 

#' The function \code{permute_strata()} then applies the stratum permutations to
#' a data frame (which must have the correct number of strata at each axis).
#' 

#' @name minimize-overlaps
#' @param data Data frame in lode form (see \code{\link{to_lodes}}).
#' @param key,value,id Numeric or character; the fields of \code{data}
#'   corresponding to the axis (variable), stratum (value), and alluvium
#'   (identifying) variables.
#' @param weight Optional numeric or character; the fields of \code{data}
#'   corresponding to alluvium or lode weights (heights when plotted). The
#'   default value \code{NULL} assigns every alluvium equal weight.
#' @param objective Character, the objective function to minimize; matched to
#'   \code{"count"} or \code{"weight"}.
#' @param method Character; whether to exhaust all permutations of all axes
#'   (\code{"exhaustive"}) versus to perform heuristic optimization from random
#'   initial permutations (\code{"heuristic"}). Default, \code{NULL}, is to use
#'   the heuristic algorithm unless no axis has six or more strata.
#' @param niter Positive integer; if \code{method} is \code{"heuristic"}, the
#'   number of iterations to perform from randomly sampled seed permutation
#'   sets.
#' @example inst/examples/ex-optimize-strata.r
#' @export
optimize_strata <- function(
  data,
  key, value, id,
  weight = NULL,
  objective = "count", method = NULL, niter = 6
) {
  
  if (!is_alluvial_lodes(data = data, key = key, value = value, id = id,
                         weight = weight, logical = TRUE)) {
    stop("Data passed to 'optimize_strata()' must be in 'lodes' format.")
  }
  
  # augment 'data' with suitable weight variable
  # THIS IS A BIT CUMBERSOME
  objective <- match.arg(objective, c("count", "weight"))
  if (objective == "count") {
    if (!is.null(weight)) {
      message("Objective 'count' will ignore 'weight' variable.")
    } else {
      weight <- "weight"
    }
    data[[weight]] <- 1
  } else {
    if (is.null(weight)) {
      warning("Objective '", objective, "' requires 'weight' variable. ",
              "Unit weights will be used.")
      weight <- "weight"
      data[[weight]] <- 1
    }
  }
  
  # contiguate alluvia
  # WHY IS THIS NECESSARY?
  #data[[id]] <- contiguate(data[[id]])
  # governing parameters
  axes <- sort(unique(data[[key]]))
  n_axes <- length(axes)
  ns_strata <- sapply(axes, function(x) {
    length(unique(data[data[[key]] == x, ][[value]]))
  })
  # which strata appear at each axis
  strata <- sort(unique(data[[value]]))
  axis_strata <- lapply(axes, function(x) {
    sort(match(unique(data[[value]][data[[key]] == x]), strata))
  })
  
  # decide whether to iterate over permutations of all values
  # versus over ordered tuples of permutations of axis values
  n_strata <- dplyr::n_distinct(data[[value]])
  n_evals_agg <- factorial(n_strata)
  n_evals_sep <- prod(sapply(ns_strata, factorial))
  n_evals <- min(n_evals_agg, n_evals_sep)
  # decide, if not user-specified, whether to conduct an exhaustive search
  if (is.null(method)) {
    method <- if (n_evals < 1260) "exhaustive" else "heuristic"
  }
  method <- match.arg(method, c("exhaustive", "heuristic"))
  
  # minimize objective function
  if (method == "exhaustive") {
    
    min_obj <- Inf
    
    if (n_evals_agg < n_evals_sep) {
      
      # iterator for level permutations across axes
      I <- iterpc::iterpc(n_strata, ordered = TRUE)
      peb <- dplyr::progress_estimated(n_evals_agg + 1, 2)
      repeat {
        perm <- iterpc::getnext(I)
        peb$tick()$print()
        if (is.null(perm)) break
        perms <- lapply(axis_strata, function(x) {
          order(match(x, perm))
        })
        # calculate new objective function and reploce the old one if smaller
        obj <- objective_fun(data, key, value, id, weight, perms)
        if (obj < min_obj) {
          sol_perm <- perm
          sol_perms <- perms
          min_obj <- obj
        }
      }
      
    } else {
      
      # iterators for stratum permutations at each axis
      Is <- lapply(ns_strata, iterpc::iterpc, ordered = TRUE)
      nrep <- 0
      peb <- dplyr::progress_estimated(n_evals_sep, 2)
      repeat {
        nrep <- nrep + 1
        perms <- gnapply(Is)
        # if the permutations produce inconsistent stratum orderings, skip them
        # IDEALLY ONLY FEW CHECKS ARE NEEDED, REGARDING MOST RECENT STEP OF 'Is'
        perm_strata <- lapply(1:n_axes, function(i) {
          axis_strata[[i]][perms[[i]]]
        })
        recumbency <- FALSE
        for (i in 1:(n_axes - 1)) for (j in (i + 1):n_axes) {
          recumbency <- recumbency |
            is.unsorted(match(perm_strata[[j]], perm_strata[[i]]), na.rm = TRUE)
        }
        if (recumbency) next
        # calculate new objective function and reploce the old one if smaller
        obj <- objective_fun(data, key, value, id, weight, perms)
        if (obj < min_obj) {
          sol_perms <- perms
          sol_perm <- unique(unlist(lapply(sol_perms, function(x) {
            axis_strata[x]
          })))
          min_obj <- obj
        }
        peb$tick()$print()
        if (nrep == n_evals_sep) break
      }
      
    }
    
  } else {
    
    # take length-zero permutations as the baseline
    sol_perms <- lapply(ns_strata, seq_len)
    min_obj <- objective_fun(data, key, value, id, weight, sol_perms)
    # 'niter' times, start from a random permutation and heuristically minimize
    peb <- dplyr::progress_estimated(niter, 2)
    for (i in 1:niter) {
      init <- sample(n_strata)
      res <- optimize_strata_greedy(data, key, value, id, weight, axis_strata,
                                    init)
      if (res$obj < min_obj) {
        sol_perm <- res$perm
        sol_perms <- res$perms
        min_obj <- res$obj
      }
      peb$tick()$print()
    }
    
  }
  
  # replace with reversal if closer to original
  #perm_lens <- sum(sapply(sol_perms, permutation_length))
  rev_perms <- lapply(sol_perms, rev)
  #rev_perm_lens <- sum(sapply(rev_perms, permutation_length))
  if (sum(sapply(rev_perms, permutation_length)) <
      sum(sapply(sol_perms, permutation_length))) {
    sol_perms <- rev_perms
  }
  
  list(perm = sol_perm, perms = sol_perms, obj = min_obj)
}

# iterate a list of permutation iterators, *without* 'NULL's before recycling
# note: iterpc::getcurrent() returns the first permutation when the iterator is
# at the 'NULL' value
gnapply <- function(Is) {
  # if all 'NULL's, iterate each to first permutation
  if (all(sapply(lapply(Is, iterpc::getcurrent), is.null))) {
    return(lapply(Is, iterpc::getnext))
  } else {
    # find rightmost axis with non-maximum permutation;
    # iterate rightward permutations to NULLs
    l <- length(Is)
    i <- l
    while(is.null(iterpc::getnext(Is[[i]]))) {
      i <- i - 1
      if (i == 0) break
    }
    # increment index & rightward permutations (past NULLs)
    if (i < l) invisible(lapply(Is[(i + 1):l], iterpc::getnext))
    # return current permutations
    return(lapply(Is, iterpc::getcurrent))
  }
}

# greedy optimization over permutations of the strata
optimize_strata_greedy <- function(
  data, key, value, id, weight, axis_strata, init
) {
  
  # iteratively test adjacent transpositions for lower objective function values
  perm <- init
  perms <- lapply(axis_strata, function(x) {
    order(match(x, perm))
  })
  obj <- objective_fun(data, key, value, id, weight, perms)
  repeat {
    step <- FALSE
    for (i in seq_along(perm)[-1]) {
      new_perm <- perm
      new_perm[c(i - 1, i)] <- new_perm[c(i, i - 1)]
      new_perms <- lapply(axis_strata, function(x) {
        order(match(x, new_perm))
      })
      new_obj <- objective_fun(data, key, value, id, weight, new_perms)
      if (new_obj < obj) {
        perm <- new_perm
        perms <- new_perms
        obj <- new_obj
        step <- TRUE
      }
    }
    if (step == FALSE) break
  }
  
  list(perm = perm, perms = perms, obj = obj)
}

# greedy optimization over the direct sum of the permutohedra at the axes
optimize_axis_strata_greedy <- function(
  data, key, value, id, weight, axis_strata, inits
) {
  
  # iteratively test adjacent transpositions for lower objective function values
  perms <- inits
  obj <- objective_fun(data, key, value, id, weight, perms)
  repeat {
    step <- FALSE
    for (i in seq_along(perms)) {
      for (j in seq_along(perms[[i]])[-1]) {
        new_perms <- perms
        new_perms[[i]][c(j - 1, j)] <- new_perms[[i]][c(j, j - 1)]
        new_obj <- objective_fun(data, key, value, id, weight, new_perms)
        if (new_obj < obj) {
          perms <- new_perms
          obj <- new_obj
          step <- TRUE
        }
      }
    }
    if (step == FALSE) break
  }
  
  perm <- unique(unlist(lapply(perms, function(x) {
    axis_strata[x]
  })))
  list(perm = perm, perms = perms, obj = obj)
}

# THE OBJECTIVE FUNCTION MAY BE WAY MESSED UP (SEE THE EXAMPLES).
# PLOW THROUGH WITH A VERY SIMPLE EXAMPLE.
#' @rdname minimize-overlaps
#' @export
objective_fun <- function(data, key, value, id, weight, perms) {
  # ensure that 'data' is arranged by alluvium
  data <- data[do.call(order, data[, c(id, key, value)]), ]
  axes <- sort(unique(data[[key]]))
  # permute the strata at each axis in a consistent manner according to 'perms'
  for (i in seq_along(axes)) {
    this_axis <- which(data[[key]] == axes[i])
    these_values <- sort(unique(data[[value]][this_axis]))
    these_keyvalues <- if (is.factor(data[[value]])) {
      as.numeric(droplevels(data[[value]][this_axis]))
    } else {
      as.numeric(factor(data[[value]][this_axis], levels = these_values))
    }
    data[[value]][this_axis] <- these_values[perms[[i]][these_keyvalues]]
  }
  obj <- 0
  for (i in seq_along(axes)[-1]) {
    # alluvia and strata on each side of the flow segment
    d_left <- data[data[[key]] == axes[i - 1], c(id, value, weight)]
    d_right <- data[data[[key]] == axes[i], c(id, value, weight)]
    # arrangements of alluvia up to within-stratum permutations
    # (which are irrelevant to the objective function)
    p_left <- d_left[[id]][order(d_left[[value]])]
    p_right <- d_right[[id]][order(d_right[[value]])]
    # weights in order of alluvia
    w_left <- d_left[[weight]][order(d_left[[id]])]
    w_right <- d_right[[weight]][order(d_right[[id]])]
    w <- (w_left + w_right) / 2
    # identify the alluvia that intersect
    p_match <- match(p_right, p_left)
    p_combn <- utils::combn(p_match, 2)
    p_combn <- p_combn[, p_combn[1, ] > p_combn[2, ], drop = FALSE]
    # increment objective function
    obj <- obj + sum(apply(rbind(w[p_combn[1, ]], w[p_combn[2, ]]), 2, prod))
  }
  obj
}

permutation_length <- function(perm) {
  if (length(perm) == 1) return(0)
  pairs <- utils::combn(perm, 2)
  sum(pairs[1, ] > pairs[2, ])
}

#' @rdname minimize-overlaps
#' @param perm,perms An integer vector or list of integer vectors encoding 
#'   permutations of the strata, either all together (as output by 
#'   \code{optimize_strata()}) or for each axis (as output by
#'   \code{optimize_axis_strata()}).
#' @export
permute_strata <- function(data, key, value, id, perm) {
  stopifnot(is_alluvial_lodes(data, key, value, id))
  
  # obtain levels of the stratum variable, coercing to factor if necessary
  valuelevs <- levels(as.factor(data[[value]]))
  # reorder the factor levels according to 'perm'
  data[[value]] <- factor(data[[value]], levels = valuelevs[perm])
  
  data
}

#' @rdname minimize-overlaps
#' @export
permute_axis_strata <- function(data, key, value, id, perms) {
  warning("'permute_axis_strata()' is experimental.")
  stopifnot(is_alluvial_lodes(data, key, value, id))
  
  # introduce a key-value interaction variable in axis-stratum order
  data <- data[do.call(order, data[, c(key, value)]), ]
  keyvalue <- as.numeric(interaction(data[, c(key, value)],
                                     lex.order = TRUE, drop = TRUE))
  # replace any duplicates with adjusted names
  value_levs <- as.character(data[[value]][!duplicated(keyvalue)])
  while (anyDuplicated(value_levs)) {
    which_dupe <- duplicated(value_levs)
    value_levs[which_dupe] <- paste0(value_levs[which_dupe], " ")
  }
  # reorder value variable according to 'perms'
  # permute the order of 'keyvalue' at each axis
  perm_cums <- c(0, cumsum(sapply(perms, length)))
  perm_levs <- unlist(lapply(seq_along(perms),
                             function(i) perms[[i]] + perm_cums[i]))
  data$.stratum <- data[[value]]
  data[[value]] <- factor(value_levs[keyvalue], levels = value_levs[perm_levs])
  data[do.call(order, data[, c(key, id)]), ]
}
