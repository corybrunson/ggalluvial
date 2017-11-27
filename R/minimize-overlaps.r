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
#' @param free.strata Logical; whether to allow the same strata to be ordered
#'   differently at different axes.
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
  free.strata = TRUE,
  objective = "count", method = NULL, niter = 6
) {
  
  if (!is_alluvial_lodes(data = data, key = key, value = value, id = id,
                         weight = weight, logical = TRUE)) {
    stop("Data must be in 'lodes' format.")
  }
  
  # restrict to variables of interest
  #data <- dplyr::select_(data, key, value, id, weight)
  # contiguate alluvia
  data[[id]] <- contiguate(data[[id]])
  # governing parameters
  xs <- sort(unique(data[[key]]))
  n_strata <- sapply(xs, function(x) {
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
  
  # augment 'data' with suitable weight variable
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
  
  # minimize objective function
  if (is.null(method)) {
    method <- if (max(n_strata) < 6) "exhaustive" else "heuristic"
  }
  method <- match.arg(method, c("exhaustive", "heuristic"))
  if (method == "exhaustive") {
    
    min_obj <- Inf
    # iterators for stratum permutations at each axis
    Is <- lapply(n_strata, iterpc::iterpc, ordered = TRUE)
    perms <- lapply(Is, iterpc::getnext)
    sol_perms <- perms
    max_perms <- lapply(lapply(n_strata, seq_len), rev)
    peb <- dplyr::progress_estimated(prod(sapply(Is, iterpc::getlength)), 2)
    repeat {
      peb$tick()$print()
      if (identical(perms, max_perms)) break
      perms <- gnapply(Is)
      # if orderings of the same strata disagree across axes, skip
      if (!free.strata) {
        if (FALSE) next
      }
      # calculate the new objective function and reploce the old one if smaller
      obj <- objective_fun(data, key, value, id, weight, perms)
      if (obj < min_obj) {
        sol_perms <- perms
        min_obj <- obj
      }
    }
  } else {
    
    # take length-zero permutations as the baseline
    sol_perms <- lapply(n_strata, seq_len)
    min_obj <- objective_fun(data, key, value, id, weight, sol_perms)
    # 'niter' times, start from a random permutation and heuristically minimize
    peb <- dplyr::progress_estimated(niter, 2)
    for (i in 1:niter) {
      res <- optimize_strata_greedy(data, key, value, id, weight,
                                    lapply(n_strata, sample))
      if (res$obj < min_obj) {
        sol_perms <- res$perms
        min_obj <- res$obj
      }
      peb$tick()$print()
    }
  }
  
  # replace with reversal if closer to original
  perm_lens <- sum(sapply(sol_perms, permutation_length))
  rev_perms <- lapply(sol_perms, rev)
  rev_perm_lens <- sum(sapply(rev_perms, permutation_length))
  if (rev_perm_lens < perm_lens) {
    sol_perms <- rev_perms
  }
  
  sol_perms
}

# iterate a list of permutation iterators, with a single NULL before recycling
gnapply <- function(Is) {
  # find rightmost axis with non-maximum permutation;
  # iterate rightward permutations to NULLs
  l <- length(Is)
  i <- l
  while (is.null(iterpc::getnext(Is[[i]]))) {
    i <- i - 1
    if (i == 0) {
      return(NULL)
    }
  }
  # increment index & rightward permutations (past NULLs)
  if (i < l) invisible(lapply(Is[(i + 1):l], iterpc::getnext))
  # return current permutations
  lapply(Is, iterpc::getcurrent)
}

# greedy optimization over the direct sum of the permutohedra at the axes
optimize_strata_greedy <- function(data, key, value, id, weight, init) {
  
  # iteratively test adjacent transpositions for lower objective function values
  perms <- init
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
  
  list(perms = perms, obj = obj)
}

objective_fun <- function(data, key, value, id, weight, perms) {
  # ensure that 'data' is arranged by alluvium
  data <- data[do.call(order, data[, c(id, key, value)]), ]
  xs <- sort(unique(data[[key]]))
  # permute the strata at each axis in a consistent manner according to 'perms'
  for (i in seq_along(xs)) {
    this_axis <- which(data[[key]] == xs[i])
    these_values <- sort(unique(data[[value]][this_axis]))
    these_keyvalues <- if (is.factor(data[[value]])) {
      as.numeric(droplevels(data[[value]][this_axis]))
    } else {
      as.numeric(factor(data[[value]][this_axis], levels = these_values))
    }
    data[[value]][this_axis] <- these_values[perms[[i]][these_keyvalues]]
  }
  obj <- 0
  for (i in seq_along(xs)[-1]) {
    # alluvia and strata on each side of the flow segment
    d_left <- data[data[[key]] == xs[i - 1], c(id, value, weight)]
    d_right <- data[data[[key]] == xs[i], c(id, value, weight)]
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
#' @param permutations A list of integer vectors, one for each axis, each a
#'   permutation of the number of strata at the corresponding axis (as output by
#'   \code{optimize_strata()}).
#' @export
permute_strata <- function(data, key, value, id, permutations) {
  stopifnot(is_alluvial_lodes(data, key, value, id))
  
  # if any strata span multiple axes, replace them with multiple levels
  # SHOULD ONLY NEED TO DO THIS IF 'free.strata' IS 'TRUE'
  if (length(unique(data[[value]])) < nrow(unique(data[, c(key, value)]))) {
    warning("Some strata appear at multiple axes ",
            "and will be replaced by new levels with adjusted names.")
  }
  
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
  # reorder value variable according to permutations
  # permute the order of 'keyvalue' at each axis
  perm_cums <- c(0, cumsum(sapply(permutations, length)))
  perm_levs <- unlist(lapply(seq_along(permutations),
                             function(i) permutations[[i]] + perm_cums[i]))
  #perm_keyvalue <- perm_levs[keyvalue]
  #perm_keyvalue <- match(keyvalue, perm_levs)
  data[[value]] <- factor(data[[value]], levels = value_levs[perm_levs])
  data[do.call(order, data[, c(key, id)]), ]
}
