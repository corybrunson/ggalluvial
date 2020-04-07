#' @section Curves:

#' By default, `geom_alluvium()` and `geom_flow()` render flows between lodes as
#' splines about quartets of control points using [grid::xsplineGrob()]. These
#' curves are parameterized by the relative location of the knot (`knot.pos`)
#' that governs the bending of each end toward their meeting at the point of
#' inflection. They are quick to render and clear to read. Nevertheless, for any
#' number of reasons, users may prefer plots that use differently-shaped curves.
#'
#' A variety of such options are illustrated in [this blog post by Jeffrey
#' Shaffer](https://www.dataplusscience.com/Sigmoid.html). A subset of these,
#' some with added flexibility, are implemented here. Each is encoded as a
#' continuous, increasing, bijective function from the unit interval \eqn{[0,1]}
#' to itself, and each is rescaled, if necessary, so that its endpoints meet the
#' corresponding lodes. They are rendered piecewise-linearly, by default using
#' `segments = 48`. Summon each curve type by passing one of the following
#' strings to `curve`:
#'
#' - `"linear"`:     \eqn{f(x)=x}, the unique degree-1 polynomial that takes
#'                   0 to 0 and 1 to 1
#' - `"cubic"`:      \eqn{f(x)=3x^{2}-2x^{3}}{f(x)=3x^2-2x^3}, the unique
#'                   degree-3 polynomial that also is flat at both endpoints
#' - `"quintic"`:    \eqn{f(x)=10x^{3}-15x^{4}+6x^{5}}{f(x)=10x^3-15x^4+6x^5},
#'                   the unique degree-5 polynomial that also is flat at both
#'                   endpoints
#' - `"sine"`:       the unique sinusoidal function that is flat at both
#'                   endpoints
#' - `"arctangent"`: the inverse tangent function, scaled and re-centered to the
#'                   unit interval from a symmetric domain with radius `reach`
#' - `"sigmoid"`:    the sigmoid function, scaled and re-centered to the unit
#'                   interval from a symmetric domain with radius `reach`
#' 
#' Only the (default) `"xspline"` option uses the `knot.*` parameters, while
#' only the alternative curves use the `segments` parameter, and only
#' `"arctangent"` and `"sigmoid"` use the `reach` parameter. Larger values of
#' `reach` result in greater compression and steeper slopes. The `NULL` default
#' will be changed to `2+sqrt(3)` for `"arctangent"` and to `6` for `"sigmoid"`.
#' 
