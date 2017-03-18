#' Leftward outward order
#' 
#' Order the numbers 1 through \code{n}, starting at some index \code{i} and
#' putting the remaining indices in decreasing order.
#' 
#' @param n Numeric, a positive integer
#' @param i Numeric, a positive integer at most \code{n}
lode_leftward <- function(n, i) {
  if(!(i %in% 1:n)) stop('the given index lies outside the range')
  
  # Order
  if (i == 1) c(i, n:2) else if (i == n) n:1 else c(i, n:(i+1), (i-1):1)
}
