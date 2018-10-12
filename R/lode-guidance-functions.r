#' Lode guidance functions
#'
#' These functions control the order of lodes within strata in an alluvial
#' diagram. They are invoked by [stat_alluvium()] and can be passed to
#' the `lode.guidance` parameter.
#'

#' Each function orders the numbers 1 through `n`, starting at index
#' `i`. The choice of function made in [stat_alluvium()]
#' determines the order in which the other axes contribute to the sorting of
#' lodes within each index axis. After starting at `i`, the functions order
#' the remaining axes as follows:
#'
#' - `zigzag`: Zigzag outward from `i`
#' - `rightward`: Increasing order
#' - `leftward`: Decreasing order
#' - `rightleft`: Proceed rightward from `i` to `n`, then leftward to 1
#' - `leftright`: Proceed leftward from `i` to 1, then rightward to `n`
#'
#' @name lode-guidance-functions
#' @param n Numeric, a positive integer
#' @param i Numeric, a positive integer at most `n`
NULL

#' @rdname lode-guidance-functions
#' @export
lode_zigzag <- function(n, i) {

  # radii
  r1 <- i - 1
  r2 <- n - i
  r <- min(r1, r2)

  # attempt cohesion in the direction of the closer end
  leftward <- (i <= n / 2)

  # setup
  sgn <- if(r1 == r2) 0 else (r2 - r1) / abs(r2 - r1)
  rem <- (i + sgn * (r + 1)):((n+1)/2 + sgn * (n-1)/2)
  zz <- (1 - 2 * leftward) * c(1, -1)

  # order
  c(i,
    if(r == 0) c() else sapply(1:r, function(j) i + j * zz),
    if(sgn == 0) c() else rem)
}

#' @rdname lode-guidance-functions
#' @export
lode_rightward <- function(n, i) {
  if (i == 1) 1:n else if (i == n) c(n, 1:(n-1)) else c(i, 1:(i-1), (i+1):n)
}

#' @rdname lode-guidance-functions
#' @export
lode_leftward <- function(n, i) {
  if (i == 1) c(i, n:2) else if (i == n) n:1 else c(i, n:(i+1), (i-1):1)
}

#' @rdname lode-guidance-functions
#' @export
lode_rightleft <- function(n, i) {
  if (i == 1) 1:n else if (i == n) n:1 else c(i, (i+1):n, (i-1):1)
}

#' @rdname lode-guidance-functions
#' @export
lode_leftright <- function(n, i) {
  if (i == 1) 1:n else if (i == n) n:1 else c(i, (i-1):1, (i+1):n)
}
