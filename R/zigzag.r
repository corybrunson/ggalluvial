#' Zigzag outward order
#' 
#' Order the numbers 1 through \code{n}, starting at some index and zigzagging
#' outward.
#' 
#' @param n Numeric, a positive integer
#' @param i Numeric, a positive integer at most \code{n}
zigzag <- function(n, i) {
  if(!(i %in% 1:n)) stop('the given index lies outside the range')
  
  # Radii
  r1 <- i - 1
  r2 <- n - i
  r <- min(r1, r2)
  
  # Attempt cohesion in the direction of the closer end
  leftward <- (i <= n / 2)
  
  # Setup
  sgn <- if(r1 == r2) 0 else (r2 - r1) / abs(r2 - r1)
  rem <- (i + sgn * (r + 1)):((n+1)/2 + sgn * (n-1)/2)
  zz <- (1 - 2 * leftward) * c(1, -1)
  
  # Order
  c(i,
    if(r == 0) c() else sapply(1:r, function(j) i + j * zz),
    if(sgn == 0) c() else rem)
}
