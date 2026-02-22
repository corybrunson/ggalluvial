# Alluvia across strata

`geom_alluvium` receives a dataset of the horizontal (`x`) and vertical
(`y`, `ymin`, `ymax`) positions of the **lodes** of an alluvial plot,
the intersections of the alluvia with the strata. It plots both the
lodes themselves, using [`geom_lode()`](geom_lode.md), and the flows
between them, using [`geom_flow()`](geom_flow.md).

## Usage

``` r
geom_alluvium(
  mapping = NULL,
  data = NULL,
  stat = "alluvium",
  position = "identity",
  width = 1/3,
  knot.pos = 1/4,
  knot.prop = TRUE,
  curve_type = NULL,
  curve_range = NULL,
  segments = NULL,
  outline.type = "both",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
)

data_to_alluvium(
  data,
  knot.prop = TRUE,
  curve_type = "spline",
  curve_range = NULL,
  segments = NULL
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  The data to be displayed in this layer. There are three options:

  If `NULL`, the default, the data is inherited from the plot data as
  specified in the call to
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

  A `data.frame`, or other object, will override the plot data. All
  objects will be fortified to produce a data frame. See
  [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
  for which variables will be created.

  A `function` will be called with a single argument, the plot data. The
  return value must be a `data.frame`, and will be used as the layer
  data. A `function` can be created from a `formula` (e.g.
  `~ head(.x, 10)`).

- stat:

  The statistical transformation to use on the data; override the
  default.

- position:

  Position adjustment, either as a string naming the adjustment (e.g.
  `"jitter"` to use `position_jitter`), or the result of a call to a
  position adjustment function. Use the latter if you need to change the
  settings of the adjustment.

- width:

  Numeric; the width of each stratum, as a proportion of the distance
  between axes. Defaults to 1/3.

- knot.pos:

  The horizontal distance of x-spline knots from each stratum (`width/2`
  from its axis), either (if `knot.prop = TRUE`, the default) as a
  proportion of the length of the x-spline, i.e. of the gap between
  adjacent strata, or (if `knot.prop = FALSE`) on the scale of the `x`
  direction.

- knot.prop:

  Logical; whether to interpret `knot.pos` as a proportion of the length
  of each flow (the default), rather than on the `x` scale.

- curve_type:

  Character; the type of curve used to produce flows. Defaults to
  `"xspline"` and can be alternatively set to one of `"linear"`,
  `"cubic"`, `"quintic"`, `"sine"`, `"arctangent"`, and `"sigmoid"`.
  `"xspline"` produces approximation splines using 4 points per curve;
  the alternatives produce interpolation splines between points along
  the graphs of functions of the associated type. See the **Curves**
  section.

- curve_range:

  For alternative `curve_type`s based on asymptotic functions, the value
  along the asymptote at which to truncate the function to obtain the
  shape that will be scaled to fit between strata. See the **Curves**
  section.

- segments:

  The number of segments to be used in drawing each alternative curve
  (each curved boundary of each flow). If less than 3, will be silently
  changed to 3.

- outline.type:

  Type of outline of each alluvium; one of `"both"`, `"lower"`,
  `"upper"`, and `"full"`.

- na.rm:

  Logical: if `FALSE`, the default, `NA` lodes are not included; if
  `TRUE`, `NA` lodes constitute a separate category, plotted in grey
  (regardless of the color scheme).

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes. It can also be a named logical
  vector to finely select the aesthetics to display.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behaviour from the default
  plot specification, e.g.
  [`borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).

- ...:

  Additional arguments passed to
  [`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html).

## Details

The helper function `data_to_alluvium()` takes internal **ggplot2** data
(mapped aesthetics) and curve parameters for a single alluvium as input
and returns a data frame of `x`, `y`, and `shape` used by
[`grid::xsplineGrob()`](https://rdrr.io/r/grid/grid.xspline.html) to
render the alluvium.

## Aesthetics

`geom_alluvium`, `geom_flow`, `geom_lode`, and `geom_stratum` understand
the following aesthetics (required aesthetics are in bold):

- **`x`**

- **`y`**

- **`ymin`**

- **`ymax`**

- `alpha`

- `colour`

- `fill`

- `linetype`

- `size`

- `group`

`group` is used internally; arguments are ignored.

Alluvium, flow, and lode geoms default to `alpha = 0.5`. Learn more
about setting these aesthetics in
[`vignette("ggplot2-specs", package = "ggplot2")`](https://ggplot2.tidyverse.org/articles/ggplot2-specs.html).

## Curves

By default, `geom_alluvium()` and [`geom_flow()`](geom_flow.md) render
flows between lodes as filled regions between parallel x-splines. These
graphical elements, generated using
[`grid::xsplineGrob()`](https://rdrr.io/r/grid/grid.xspline.html), are
parameterized by the relative location of the knot (`knot.pos`). They
are quick to render and clear to read, but users may prefer plots that
use differently-shaped ribbons.

A variety of such options are documented at, e.g., [this easing
functions cheat sheet](https://easings.net/) and [this blog post by
Jeffrey Shaffer](https://www.dataplusscience.com/Sigmoid.html). Easing
functions are not (yet) used in ggalluvial, but several alternative
curves are available. Each is encoded as a continuous, increasing,
bijective function from the unit interval \\(\[0,1\]\\) to itself, and
each is rescaled so that its endpoints meet the corresponding lodes.
They are rendered piecewise-linearly, by default using `segments = 48`.
Summon each curve type by passing one of the following strings to
`curve_type`:

- `"linear"`: \\(f(x)=x\\), the unique degree-1 polynomial that takes 0
  to 0 and 1 to 1

- `"cubic"`: \\(f(x)=3x^{2}-2x^{3}\\), the unique degree-3 polynomial
  that also is flat at both endpoints

- `"quintic"`: \\(f(x)=10x^{3}-15x^{4}+6x^{5}\\), the unique degree-5
  polynomial that also has zero curvature at both endpoints

- `"sine"`: the unique sinusoidal function that is flat at both
  endpoints

- `"arctangent"`: the inverse tangent function, scaled and re-centered
  to the unit interval from the interval centered at zero with radius
  `curve_range`

- `"sigmoid"`: the sigmoid function, scaled and re-centered to the unit
  interval from the interval centered at zero with radius `curve_range`

Only the (default) `"xspline"` option uses the `knot.*` parameters,
while only the alternative curves use the `segments` parameter, and only
`"arctangent"` and `"sigmoid"` use the `curve_range` parameter. (Both
are ignored if not needed.) Larger values of `curve_range` result in
greater compression and steeper slopes. The `NULL` default will be
changed to `2+sqrt(3)` for `"arctangent"` and to `6` for `"sigmoid"`.

These package-specific options set global values for `curve_type`,
`curve_range`, and `segments` that will be defaulted to when not
manually set:

- `ggalluvial.curve_type`: defaults to `"xspline"`.

- `ggalluvial.curve_range`: defaults to `NA`, which triggers the
  curve-specific default values.

- `ggalluvial.segments`: defaults to `48L`.

See [`base::options()`](https://rdrr.io/r/base/options.html) for how to
use options.

## Defunct parameters

The previously defunct parameters `axis_width` and `ribbon_bend` have
been discontinued. Use `width` and `knot.pos` instead.

## See also

[`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html)
for additional arguments and [`stat_alluvium()`](stat_alluvium.md) and
[`stat_flow()`](stat_flow.md) for the corresponding stats.

Other alluvial geom layers: [`geom_flow()`](geom_flow.md),
[`geom_lode()`](geom_lode.md), [`geom_stratum()`](geom_stratum.md)

## Examples

``` r
# basic
ggplot(as.data.frame(Titanic),
       aes(y = Freq,
           axis1 = Class, axis2 = Sex, axis3 = Age,
           fill = Survived)) +
  geom_alluvium() +
  scale_x_discrete(limits = c("Class", "Sex", "Age"))


gg <- ggplot(alluvial::Refugees,
             aes(y = refugees, x = year, alluvium = country))
# time series bump chart (sigmoid flows)
gg + geom_alluvium(aes(fill = country, colour = country),
                   width = 1/4, alpha = 2/3, decreasing = FALSE,
                   curve_type = "sigmoid")

# time series line plot of refugees data, sorted by country
gg + geom_alluvium(aes(fill = country, colour = country),
                   decreasing = NA, width = 0, knot.pos = 0)


# \donttest{
# irregular spacing between axes of a continuous variable
refugees_sub <- subset(alluvial::Refugees, year %in% c(2003, 2005, 2010, 2013))
gg <- ggplot(data = refugees_sub,
             aes(x = year, y = refugees, alluvium = country)) +
  theme_bw() +
  scale_fill_brewer(type = "qual", palette = "Set3")
# proportional knot positioning (default)
gg +
  geom_alluvium(aes(fill = country),
                alpha = .75, decreasing = FALSE, width = 1/2) +
  geom_stratum(aes(stratum = country), decreasing = FALSE, width = 1/2)

# constant knot positioning
gg +
  geom_alluvium(aes(fill = country),
                alpha = .75, decreasing = FALSE, width = 1/2,
                knot.pos = 1, knot.prop = FALSE) +
  geom_stratum(aes(stratum = country), decreasing = FALSE, width = 1/2)

# coarsely-segmented curves
gg +
  geom_alluvium(aes(fill = country),
                alpha = .75, decreasing = FALSE, width = 1/2,
                curve_type = "arctan", segments = 6) +
  geom_stratum(aes(stratum = country), decreasing = FALSE, width = 1/2)

# custom-ranged curves
gg +
  geom_alluvium(aes(fill = country),
                alpha = .75, decreasing = FALSE, width = 1/2,
                curve_type = "arctan", curve_range = 1) +
  geom_stratum(aes(stratum = country), decreasing = FALSE, width = 1/2)

# }
```
