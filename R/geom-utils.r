# convert rectangle to polygon
# (lifted from [ggplot2::geom_rect()])
rect_to_poly <- function(xmin, xmax, ymin, ymax) {
  data.frame(
    y = c(ymax, ymax, ymin, ymin, ymax),
    x = c(xmin, xmax, xmax, xmin, xmin)
  )
}

# alternative curve options
# each is a function that takes [0,1] to [0,1]
# degree-3 polynomial with degree-1 critical endpoints
unit_cubic <- function(x) 3*x^2 - 2*x^3
# degree-5 polynomial with degree-2 critical endpoints
unit_quintic <- function(x) 10*x^3 - 15*x^4 + 6*x^5
# sinusoidal function with crests at endpoints
unit_sine <- function(x) {
  t <- (x - .5) * pi
  sin(t) / 2 + .5
}
# inverse tangent function compressed from a specified symmetric domain
unit_arctangent <- function(x, curve_range) {
  if (is.na(curve_range)) curve_range <- 2 + sqrt(3)
  t <- (x - .5) * 2 * curve_range
  atan(t) / 2 / atan(curve_range) + .5
}
# sigmoid function compressed from a specified symmetric domain
unit_sigmoid <- function(x, curve_range) {
  if (is.na(curve_range)) curve_range <- 6
  t <- (x - .5) * 2 * curve_range
  (stats::plogis(t) - stats::plogis(-curve_range)) /
    diff(stats::plogis(c(-1, 1) * curve_range))
}

# return the desired flow curve function
make_curve_fun <- function(curve_type, curve_range) {
  curve_type <- match.arg(
    curve_type,
    c("linear", "cubic", "quintic", "sine", "arctangent", "sigmoid")
  )
  switch(
    curve_type,
    # polynomial curves
    linear = identity,
    cubic = unit_cubic,
    quintic = unit_quintic,
    # sinusoidal curve
    sine = unit_sine,
    # asymptotic curves (compressed from a specifiable range)
    arctangent = function(x) unit_arctangent(x, curve_range),
    sigmoid = function(x) unit_sigmoid(x, curve_range)
  )
}
