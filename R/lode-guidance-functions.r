#' Lode guidance functions
#' 
#' Each function orders the numbers 1 through \code{n}, starting at index 
#' \code{i}. The choice of function made in \code{\link{stat_alluvium}} 
#' determines the order in which the other axes contribute to the sorting of 
#' lodes within each index axis. After starting at \code{i}, the functions order
#' the remaining axes as follows:
#' \itemize{
#'   \item \code{zigzag}: Zigzag outward from \code{i}
#'   \item \code{rightward}: Increasing order
#'   \item \code{leftward}: Decreasing order
#'   \item \code{rightleft}: Proceed rightward from \code{i} to \code{n}, then
#'         leftward to 1
#'   \item \code{leftright}: Proceed leftward from \code{i} to 1, then rightward
#'         to \code{n}
#' }
#' @name lode-guidance-functions
#' @param n Numeric, a positive integer
#' @param i Numeric, a positive integer at most \code{n}
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
