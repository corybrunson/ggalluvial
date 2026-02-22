# Strata at axes

`geom_stratum` receives a dataset of the horizontal (`x`) and vertical
(`y`, `ymin`, `ymax`) positions of the strata of an alluvial plot. It
plots rectangles for these strata of a provided `width`.

## Usage

``` r
geom_stratum(
  mapping = NULL,
  data = NULL,
  stat = "stratum",
  position = "identity",
  show.legend = NA,
  inherit.aes = TRUE,
  width = 1/3,
  na.rm = FALSE,
  ...
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

- width:

  Numeric; the width of each stratum, as a proportion of the distance
  between axes. Defaults to 1/3.

- na.rm:

  Logical: if `FALSE`, the default, `NA` lodes are not included; if
  `TRUE`, `NA` lodes constitute a separate category, plotted in grey
  (regardless of the color scheme).

- ...:

  Additional arguments passed to
  [`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html).

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

## Defunct parameters

The previously defunct parameters `axis_width` and `ribbon_bend` have
been discontinued. Use `width` and `knot.pos` instead.

## See also

[`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html)
for additional arguments and [`stat_stratum()`](stat_stratum.md) for the
corresponding stat.

Other alluvial geom layers: [`geom_alluvium()`](geom_alluvium.md),
[`geom_flow()`](geom_flow.md), [`geom_lode()`](geom_lode.md)

## Examples

``` r
# full axis width
ggplot(as.data.frame(Titanic),
       aes(y = Freq,
           axis1 = Class, axis2 = Sex, axis3 = Age, axis4 = Survived)) +
  geom_stratum(width = 1) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Class", "Sex", "Age", "Survived"))

  
# use of facets
ggplot(as.data.frame(Titanic),
       aes(y = Freq,
           axis1 = Class, axis2 = Sex)) +
  geom_flow(aes(fill = Survived)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Class", "Sex")) +
  facet_wrap(~ Age, scales = "free_y")
```
