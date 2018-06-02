#' Optimize strata
#' 
#' The function \code{optimize_strata()} attempts to order the strata at each 
#' axis so as to maximize the readability of the diagram, by minimizing a 
#' weighted total of the overlaps of crossing flows. The weight assigned to each
#' overlap is the product of the mean \code{weight}s of the overlapping flows at
#' each incident axis. The function \code{permute_strata()} applies the stratum 
#' permutations to the stratum variable of the data frame; the experimental 
#' function \code{permute_axis_strata()} applies a list of permutations 
#' axis-wise, potentially resulting in different orderings of the same strata at
#' different axes.
#' 
#' @name minimize-overlaps
#' @param data Data frame in lode form (see \code{\link{to_lodes}}).
#' @param key,value,id Numeric or character; the fields of \code{data} 
#'   corresponding to the axis (variable), stratum (value), and alluvium 
#'   (identifying) variables.
#' @param weight Numeric or character; the field of \code{data} corresponding to
#'   flow weights (heights in the diagram). The default value \code{NULL}
#'   assigns every alluvium, hence every overlap, unit weight.
#' @param method Character; whether to exhaust all permutations of all axes 
#'   (\code{"exhaustive"}) versus to perform heuristic optimization from random 
#'   initial permutations (\code{"heuristic"}). Default, \code{NULL}, is to use 
#'   the heuristic algorithm unless no axis has six or more strata.
#' @param niter Positive integer; if \code{method} is \code{"heuristic"}, the 
#'   number of iterations to perform from randomly sampled seed permutation 
#'   sets.
#' @example inst/examples/ex-minimize-overlaps.r
#' @export
optimize_strata <- function(
  data,
  key, value, id,
  weight = NULL,
  method = NULL, niter = 6
) {
  
  if (!is_lodes_form(data = data, key = key, value = value, id = id,
                     weight = weight, logical = TRUE)) {
    stop("Data passed to 'optimize_strata()' must be in 'lodes' format.")
  }
  
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
  perm_orig <- 1:n_strata
  perms_orig <- lapply(ns_strata, seq_len)
  
  # minimize objective function
  if (method == "exhaustive") {
    
    obj_orig <- objective_strata(data, key, value, id, weight, perm_orig)
    obj_min <- Inf
    
    if (n_evals_agg < n_evals_sep) {
      message("Exhausting aggregate permutations of all strata.")
      
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
        obj <- objective_strata(data, key, value, id, weight, perm)
        if (obj < obj_min) {
          sol_perm <- perm
          sol_perms <- perms
          obj_min <- obj
        }
      }
      
    } else {
      message("Exhausting separate permutations of axis strata.")
      
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
        obj <- objective_axis_strata(data, key, value, id, weight, perms)
        if (obj < obj_min) {
          sol_perms <- perms
          sol_perm <- unique(unlist(lapply(sol_perms, function(x) {
            axis_strata[x]
          })))
          obj_min <- obj
        }
        peb$tick()$print()
        if (nrep == n_evals_sep) break
      }
      
    }
    
  } else {
    
    # take length-zero permutations as the baseline
    sol_perm <- perm_orig
    obj_min <- objective_strata(data, key, value, id, weight, sol_perm)
    obj_orig <- obj_min
    # 'niter' times, start from a random permutation and heuristically minimize
    peb <- dplyr::progress_estimated(niter, 2)
    for (i in 1:niter) {
      init <- sample(n_strata)
      res <- optimize_strata_greedy(data, key, value, id, weight, axis_strata,
                                    init)
      if (res$obj < obj_min) {
        sol_perms <- res$perms
        sol_perm <- res$perm
        obj_min <- res$obj
      }
      peb$tick()$print()
    }
    
  }
  
  # replace with reversal if closer to original
  rev_perms <- lapply(sol_perms, rev)
  if (sum(sapply(rev_perms, permutation_length)) <
      sum(sapply(sol_perms, permutation_length))) {
    sol_perms <- rev_perms
  }
  
  list(perm = sol_perm, perms = sol_perms, obj = obj_min, obj_orig = obj_orig)
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
  obj <- objective_strata(data, key, value, id, weight, perm)
  repeat {
    step <- FALSE
    for (i in seq_along(perm)[-1]) {
      new_perm <- perm
      new_perm[c(i - 1, i)] <- new_perm[c(i, i - 1)]
      new_obj <- objective_strata(data, key, value, id, weight, new_perm)
      if (new_obj < obj) {
        perm <- new_perm
        obj <- new_obj
        step <- TRUE
      }
    }
    if (step == FALSE) break
  }
  
  perms <- lapply(axis_strata, function(x) {
    order(match(x, perm))
  })
  list(perm = perm, perms = perms, obj = obj)
}

# greedy optimization over the direct sum of the permutohedra at the axes
optimize_axis_strata_greedy <- function(
  data, key, value, id, weight, axis_strata, inits
) {
  
  # iteratively test adjacent transpositions for lower objective function values
  perms <- inits
  obj <- objective_axis_strata(data, key, value, id, weight, perms)
  repeat {
    step <- FALSE
    for (i in seq_along(perms)) {
      for (j in seq_along(perms[[i]])[-1]) {
        new_perms <- perms
        new_perms[[i]][c(j - 1, j)] <- new_perms[[i]][c(j, j - 1)]
        new_obj <- objective_axis_strata(data, key, value, id, weight, new_perms)
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

objective_strata <- function(
  data, key, value, id, weight, perm
) {
  
  data[[id]] <- contiguate(data[[id]])
  data <- permute_strata(data, key, value, id, perm)
  # sort alluvia within strata
  data <- data[do.call(order, data[, c(key, value, id)]), ]
  axes <- sort(unique(data[[key]]))
  
  if (is.null(weight)) {
    data[[".weight"]] <- 1
    weight <- ".weight"
  }
  
  obj <- 0
  for (i in seq_along(axes)[-1]) {
    # alluvia and strata on each side of the flow segment
    d_left <- data[data[[key]] == axes[i - 1], c(id, value, weight)]
    d_right <- data[data[[key]] == axes[i], c(id, value, weight)]
    # match the alluvia on either side
    lr_match <- match(d_left[[id]], d_right[[id]])
    rl_match <- match(d_right[[id]], d_left[[id]])
    # mean 'weight' in order of 'd_left' (zero if no match)
    w <- (d_left[[weight]] + d_right[[weight]][lr_match]) / 2
    w[is.na(w)] <- 0
    # identify pairings from the left->right perspective
    rl_combn <- utils::combn(rl_match, 2)
    # exclude pairings that don't intersect (relies on contiguation)
    # NOTE: SOME INTERSECTIONS WILL BE PREVENTED BY 'lode.ordering'
    # SO THIS PRODUCES AN OVERESTIMATE OF THE ACTUAL NUMBER OF INTERSECTIONS
    rl_combn <- rl_combn[, rl_combn[1, ] > rl_combn[2, ], drop = FALSE]
    # increment by the products of the weights of the intersecting alluvia
    obj <- obj + sum(apply(rbind(w[rl_combn[1, ]], w[rl_combn[2, ]]), 2, prod))
  }
  obj
}

objective_axis_strata <- function(
  data, key, value, id, weight, perms
) {
  
  data[[id]] <- contiguate(data[[id]])
  data <- permute_axis_strata(data, key, value, id, perms)
  # sort alluvia within strata
  data <- data[do.call(order, data[, c(key, value, id)]), ]
  axes <- sort(unique(data[[key]]))
  
  if (is.null(weight)) {
    data[[".weight"]] <- 1
    weight <- ".weight"
  }
  
  obj <- 0
  for (i in seq_along(axes)[-1]) {
    # alluvia and strata on each side of the flow segment
    d_left <- data[data[[key]] == axes[i - 1], c(id, value, weight)]
    d_right <- data[data[[key]] == axes[i], c(id, value, weight)]
    # match the alluvia on either side
    lr_match <- match(d_left[[id]], d_right[[id]])
    rl_match <- match(d_right[[id]], d_left[[id]])
    # mean 'weight' in order of 'd_left' (zero if no match)
    w <- (d_left[[weight]] + d_right[[weight]][lr_match]) / 2
    w[is.na(w)] <- 0
    # identify pairings from the left->right perspective
    rl_combn <- utils::combn(rl_match, 2)
    # exclude pairings that don't intersect (relies on contiguation)
    # NOTE: SOME INTERSECTIONS WILL BE PREVENTED BY 'lode.ordering'
    # SO THIS PRODUCES AN OVERESTIMATE OF THE ACTUAL NUMBER OF INTERSECTIONS
    rl_combn <- rl_combn[, rl_combn[1, ] > rl_combn[2, ], drop = FALSE]
    # increment by the products of the weights of the intersecting alluvia
    obj <- obj + sum(apply(rbind(w[rl_combn[1, ]], w[rl_combn[2, ]]), 2, prod))
  }
  obj
}

permutation_length <- function(perm) {
  if (length(perm) == 1) return(0)
  pairs <- utils::combn(perm, 2)
  sum(pairs[1, ] > pairs[2, ])
}

#' @rdname minimize-overlaps
#' @param perm,perms An integer vector (\code{perm}) or list of integer vectors 
#'   (\code{perms}) encoding permutation(s) of the strata, as output by
#'   \code{optimize_strata()}).
#' @export
permute_strata <- function(data, key, value, id, perm) {
  stopifnot(is_lodes_form(data, key, value, id))
  
  # obtain levels of the stratum variable, coercing to factor if necessary
  value_levs <- levels(as.factor(data[[value]]))
  # reorder the factor levels according to 'perm'
  data[[value]] <- factor(data[[value]], levels = value_levs[perm])
  
  data
}

#' @rdname minimize-overlaps
#' @export
permute_axis_strata <- function(data, key, value, id, perms) {
  warning("'permute_axis_strata()' is experimental.")
  stopifnot(is_lodes_form(data, key, value, id))
  
  # introduce a key-value interaction variable in axis-stratum order
  data <- data[do.call(order, data[, c(key, value)]), ]
  keyvalue <- as.numeric(interaction(data[, c(key, value)],
                                     lex.order = TRUE, drop = TRUE))
  # replace any duplicates with adjusted names
  value_levs <- as.character(data[[value]][!duplicated(keyvalue)])
  value_levs <- make.unique(value_levs, sep = "")
  # reorder value variable according to 'perms'
  # permute the order of 'keyvalue' at each axis
  perm_cums <- c(0, cumsum(sapply(perms, length)))
  perm_levs <- unlist(lapply(seq_along(perms),
                             function(i) perms[[i]] + perm_cums[i]))
  data$.stratum <- data[[value]]
  data[[value]] <- factor(value_levs[keyvalue], levels = value_levs[perm_levs])
  data[do.call(order, data[, c(key, id)]), ]
}
