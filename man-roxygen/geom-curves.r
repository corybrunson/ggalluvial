#' @section Curves:
#' By default, `geom_alluvium()` and `geom_flow()` render flows between lodes as
#' filled regions between parallel x-splines. These graphical elements,
#' generated using [`grid::xsplineGrob()`][grid::grid.xspline], are
#' parameterized by the relative location of the knot (`knot.pos`). They are
#' quick to render and clear to read, but users may prefer plots that use
#' differently-shaped ribbons.
#' 
#' A variety of such options are documented at, e.g., [this easing functions
#' cheat sheet](https://easings.net/) and [this blog post by Jeffrey
#' Shaffer](https://www.dataplusscience.com/Sigmoid.html). Easing functions are
#' not (yet) used in ggalluvial, but several alternative curves are available.
#' Each is encoded as a continuous, increasing, bijective function from the unit
#' interval \eqn{[0,1]} to itself, and each is rescaled so that its endpoints
#' meet the corresponding lodes. They are rendered piecewise-linearly, by
#' default using `segments = 48`. Summon each curve type by passing one of the
#' following strings to `curve_type`:
#' 
#' - `"linear"`:     \eqn{f(x)=x}, the unique degree-1 polynomial that takes
#'                   0 to 0 and 1 to 1
#' - `"cubic"`:      \eqn{f(x)=3x^{2}-2x^{3}}{f(x)=3x^2-2x^3}, the unique
#'                   degree-3 polynomial that also is flat at both endpoints
#' - `"quintic"`:    \eqn{f(x)=10x^{3}-15x^{4}+6x^{5}}{f(x)=10x^3-15x^4+6x^5},
#'                   the unique degree-5 polynomial that also has zero curvature
#'                   at both endpoints
#' - `"sine"`:       the unique sinusoidal function that is flat at both
#'                   endpoints
#' - `"arctangent"`: the inverse tangent function, scaled and re-centered to the
#'                   unit interval from the interval centered at zero with
#'                   radius `curve_range`
#' - `"sigmoid"`:    the sigmoid function, scaled and re-centered to the unit
#'                   interval from the interval centered at zero with radius
#'                   `curve_range`
#' 
#' Only the (default) `"xspline"` option uses the `knot.*` parameters, while
#' only the alternative curves use the `segments` parameter, and only
#' `"arctangent"` and `"sigmoid"` use the `curve_range` parameter. (Both are
#' ignored if not needed.) Larger values of `curve_range` result in greater
#' compression and steeper slopes. The `NULL` default will be changed to
#' `2+sqrt(3)` for `"arctangent"` and to `6` for `"sigmoid"`.
#' 
#' These package-specific options set global values for `curve_type`,
#' `curve_range`, and `segments` that will be defaulted to when not manually
#' set:
#' 
#' - `ggalluvial.curve_type`: defaults to `"xspline"`.
#' - `ggalluvial.curve_range`: defaults to `NA`, which triggers the
#'   curve-specific default values.
#' - `ggalluvial.segments`: defaults to `48L`.
#' 
#' See [base::options()] for how to use options.
#' 

