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
#' - `zigzag`: Zigzag outward from `i`, starting in the outward direction
#' - `zigzag`: Zigzag outward from `i`, starting in the inward direction
#' - `forward`: Increasing order (alias `rightward`)
#' - `backward`: Decreasing order (alias `leftward`)
#' - `frontback`: Proceed forward from `i` to `n`, then backward to 1
#'   (alias `rightleft`)
#' - `backfront`: Proceed backward from `i` to 1, then forward to `n`
#'   (alias `leftright`)
#' 
#' An extended discussion of how strata and lodes are arranged in alluvial
#' plots, including the effects of different lode guidance functions, can be
#' found in the vignette "The Order of the Rectangles" via
#' `vignette("order-rectangles", package = "ggalluvial")`.
#' 
#' @name lode-guidance-functions
#' @param n Numeric, a positive integer
#' @param i Numeric, a positive integer at most `n`
NULL

lode_zz <- function(n, i, outward) {
  
  # radii
  r1 <- i - 1
  r2 <- n - i
  r <- min(r1, r2)
  
  # attempt cohesion in the direction of the closer end
  backward <- (i <= n / 2) == outward
  
  # setup
  sgn <- if(r1 == r2) 0 else (r2 - r1) / abs(r2 - r1)
  rem <- (i + sgn * (r + 1)):((n+1)/2 + sgn * (n-1)/2)
  zz <- (1 - 2 * backward) * c(1, -1)
  
  # order
  c(i,
    if(r == 0) c() else sapply(1:r, function(j) i + j * zz),
    if(sgn == 0) c() else rem)
}

#' @rdname lode-guidance-functions
#' @export
lode_zigzag <- function(n, i) {
  lode_zz(n, i, outward = TRUE)
}

#' @rdname lode-guidance-functions
#' @export
lode_zagzig <- function(n, i) {
  lode_zz(n, i, outward = FALSE)
}

#' @rdname lode-guidance-functions
#' @export
lode_forward <- function(n, i) {
  if (i == 1) 1:n else if (i == n) c(n, 1:(n-1)) else c(i, 1:(i-1), (i+1):n)
}

#' @rdname lode-guidance-functions
#' @export
lode_rightward <- lode_forward

#' @rdname lode-guidance-functions
#' @export
lode_backward <- function(n, i) {
  if (i == 1) c(i, n:2) else if (i == n) n:1 else c(i, n:(i+1), (i-1):1)
}

#' @rdname lode-guidance-functions
#' @export
lode_leftward <- lode_backward

#' @rdname lode-guidance-functions
#' @export
lode_frontback <- function(n, i) {
  if (i == 1) 1:n else if (i == n) n:1 else c(i, (i+1):n, (i-1):1)
}

#' @rdname lode-guidance-functions
#' @export
lode_rightleft <- lode_frontback

#' @rdname lode-guidance-functions
#' @export
lode_backfront <- function(n, i) {
  if (i == 1) 1:n else if (i == n) n:1 else c(i, (i-1):1, (i+1):n)
}

#' @rdname lode-guidance-functions
#' @export
lode_leftright <- lode_backfront
