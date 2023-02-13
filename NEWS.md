# ggalluvial 0.12.5

This patch fixes a bug involivng the {dplyr} functions `first()` and `last()` that was shifted but not fixed in 0.12.4. Rather than being imported during build, they are accessed internally and thus imported during use. See issues #107 <https://github.com/corybrunson/ggalluvial/issues/107> and #108 <https://github.com/corybrunson/ggalluvial/issues/108> on GitHub for details.

# ggalluvial 0.12.4

## `linewidth` aesthetic (breaking change)

An upcoming release of *ggplot2* controls stroke width using the new `linewidth` aesthetic rather than `size`. This release adapts to this change internally by updating row and column layers as recommended here: <https://www.tidyverse.org/blog/2022/08/ggplot2-3-4-0-size-to-linewidth/>

## Curve constructors

Various curve constructors for alluvia and flows are consolidated into `data_to_alluvium()` and `positions_to_flow()`, which are now exported to the user as well as used internally by `GeomAlluvium$draw_group` and `GeomFlow$draw_panel`, respectively.

## Error handling

Rather than throw an error when `y` values are `NA`, the stat layers now follow **ggplot2** convention, using `remove_missing()` at the setup step with the `na.rm` parameter passed to each layer.

## Aesthetic defaults

To address #78 and for clarity, the legacy default `colour = 0` of three `Geom*()`s is changed to `colour = "transparent"`.

## Skipping examples

`\dontrun` markers have been replace by `\donttest`, per the advice here: <https://stackoverflow.com/a/68936484/4556798>. More have been added in order to reduce the time required to run.

## Vignette revisions

The primary vignette now uses the `HairEyeColor` data set, rather than over-using `Titanic`, to illustrate the parallel sets plot.

The Shiny vignette includes an embedded app using IFrame.

## Dependency upgrades

The deprecated `.dots` argument of `dplyr::group_by()` has been replaced with `dplyr::across()`, preventing a warning.

# ggalluvial 0.12.3

## Shiny vignette

A new vignette by Quentin D. Read shows how to build interactive Shiny apps with tooltips sensitive to the locations of graphical elements. This is especially important for the spline boundaries of flows and alluvia.

## Examples using `vaccinations` data

Corrections have been made to erroneous legacy code, found in one example and two vignettes, to reorder the factor levels of the `"response"` field in the data set `vaccinations`. The documentation is updated accordingly.

Also, an explanation of the (misleading) column names of this data set has been added to the main vignette.

# ggalluvial 0.12.2

This patch addresses a bug introduced in v0.12.0 that had `is_lodes_form()` return an error when a data frame contains duplicate id-axis pairings, which may be appropriate for producing faceted plots. The new `site` parameter can be passed one or more grouping variables for this purpose, and internally it is passed `"PANEL"` to prevent this error from being thrown.

# ggalluvial 0.12.1

This patch corrects a bug introduced in v0.12.0 that dropped missing values used internally by `StatFlow$compute_panel()` to keep track of flowless lodes. The problem was illustrated in issue #64.

# ggalluvial 0.12.0

## Data sets

Both installed data sets, `vaccinations` and `majors`, are better documented.

The `a` field of `vaccinations` (the within-survey fraction of respondents, which can be computed from the other fields) has been removed, and the `start_date` and `end_date` fields (`Date`s, obtained from the ALP website) have been added.

## Warning and error messages

The following changes broke no examples or tests but could change behavior in rare cases:

* `is_lodes_form()` now returns `FALSE` if any axis-alluvium pairs are duplicated, and throws the previous warning as a message. This should be more helpful than the previous behavior of suppressing the warning and leaving `tidyr::gather()` to throw an error referring to rows of the already-transformed internal data.
* Default aesthetic specifications `stratum = NULL` and `alluvium = NULL` have been added to the stats. This prevents the "unknown aesthetics" warnings that print when these aesthetics are passed to layers rather than to the plot initialization.

## Ordering of lodes/alluvia

### Behavior of `lode.ordering`

For consistency with the behavior of `aes.bind`, `stat_alluvium()` now invokes `lode.ordering` together with `lode.guidance`: If the vectors of `lode.ordering` include duplicates, i.e. they do not completely determine an order, then the remaining deposits are used to refine the order. Previously, `lode.ordering` was assumed to consist of permutation vectors, so the two parameters were mutually exclusive.

Additionally, for consistency with other influences on the lode order, the vectors of `lode.ordering` are reversed if `reverse = TRUE` (the default).
**This will change some plots but will not produce new errors.**

### Controlling lode order before guidance and aesthetics

The `lode.ordering` parameter of `stat_alluvium()` has been deprecated. Instead, the new `order` aesthetic gives priority to its argument over the differentiation aesthetics in arranging the lodes within each stratum, without producing graphical artifacts. This aesthetic can also be used in `stat_flow()`.

### Order of alluvia in negative strata

Alluvia within "deposits" are now consistently ordered in positive and negative strata, rather than according to `absolute`. This avoids the "twisting" of flows between strata of different signs. Whereas the orderings of the deposits matter to the stacked-histogram reading of the plot, the orderings of the alluvia should simply maximize its elegance and readability.
**This will change some plots but will not produce new errors.**

## Computed variables

The alluvial stats now compute four variables for use with `after_stat()`: numeric variables `n`, `count`, and `prop`; and character variables `lode` (when the `alluvium` aesthetic is specified) and `flow` (when using the flow stat). The numerical variables can be weighted using the `weight` aesthetic, which is dropped during computation (so that it does not confuse the geoms), while `lode` is distilled according to a new `distill` parameter. This use of `weight` may cause confusion with its use by the `is_*_form()` functions until they are upgraded in the next version.

These new variables complement the already-computed but heretofore undocumented variables `stratum` and `deposit`. `stratum` obviates the need for the `infer.label` parameter, which is deprecated. Its alias, `label.strata`, is now defunct. (The variable `alluvium` is often computed, but it is manipulated to be used by the geom layers and should not be passed to an aesthetic.) `deposit` takes contiguous integer values forward along the axes and upward along the (signed) strata at each axis.

## Flow upgrades and extensions

The `knot.pos` parameter of `geom_alluvium()` and `geom_flow()` is now interpreted as a proportion of the total length of each flow, i.e. of the gap between adjacent strata (_not_ axes). This means that values will vary with axis positions and stratum widths. Setting the new `knot.prop` parameter to `FALSE` prevents this by interpreting `knot.pos` as a constant value in the `x` direction.

These flows are rendered using `grid::xsplineGrob()` with four control points each: the endpoints and the two knots.
To complement them, several other curves are now available: linear (equivalent to `knot.pos = 0`), cubic, quintic, sinusoidal, arctangent, and sigmoid, summoned by the new `curve_type` parameter (which defaults to the x-spline). (The asymptotic functions, arctangent and sigmoid, are compressed according to the new `curve_range` parameter.) The new curves are rendered piecewise linearly, with resolution controlled by the new `segments` parameter (similar to `ggplot2::stat_ellipse()`).

## Options

The stratum and lode ordering parameters now default to `NULL`, in which case they are reassigned to global options internally. This simplifies their documentation. The new curve parameters `curve_type`, `curve_range`, and `segments` can also be set as options and are documented in the same way.

# ggalluvial 0.11.3

## Dependencies

In response to **ggplot2** v3.2.0, which removes the **plyr** dependency, the dependency has been removed from **ggalluvial** as well.

# ggalluvial 0.11.2

The function `self_adjoin()` is debugged for use with a continuous-valued `x` variable. An example, taking `x` to be the date of each vaccination survey in `vaccinations`, is documented with `stat_stratum()`.

# ggalluvial 0.11.1

This patch fixes a bug with including negative observations in alluvia-form data due to outdated code that prohibited negative `y` values. This was discovered while drafting two examples of this usage, which are included in the documentation.

# ggalluvial 0.11.0

## Parameter renamings, deprecations, and additions

- The `min.height` and `max.height` parameters of `stat_stratum()` are deprecated in favor of `min.y` and `max.y` (which better adhere to **ggplot2** conventions) and extended to the other `stat_*()` layers.
- The `label.strata` parameter of `stat_stratum()` is deprecated in favor of `infer.label`, which is extended to the other `stat_*()` layers and sets `label` to `alluvium` in those cases rather than to `stratum`.
- The `aggregate.y` parameter of `stat_alluvium()` is deprecated in favor of `cement.alluvia`, and the underlying procedure is debugged.
- The `aes.bind` parameter of `stat_flow()` and `stat_alluvium()` now prefers character string options to logical values, described in the lode ordering vignette: `"none"`, `"flows"`, and `"alluvia"`. The default `"none"` produces different behavior than the previous default `FALSE`, in that under this setting the aesthetic variables are _not at all_ prioritized.
- The previously defunct stat parameters `weight` and `aggregate.wts` are discontinued.

## Negative observations

Negative values can now be meaningfully passed to `y`, producing behavior that mimics that of `geom_bar()`. The new logical parameter `absolute` controls whether negative strata, and lodes within them, are ordered vertically in the same way as positive strata and lodes (`FALSE`) or in the opposite way (`TRUE`).
Additionally, the `negate.strata` parameter can be used to negate the observations associated with specific strata, in order to situate them below rather than above the `x` axis.

## New lode guidance function

The new lode guidance function `lode_zagzig()` mimics the behavior of `lode_zigzag()` except in initially "zagging" toward the farther end rather than "zigging" toward the closer end.

## Stat layer consistency

`stat_*()` internals have been simplified and standardized, in particular the manner in which lodes are ordered within strata.

## Layer tests

Tests have been added for the statistical transformations.
Visual regression tests using **vdiffr** have been added for the geoms.

# ggalluvial 0.10.0

## Lode guidance and ordering

The lode guidance functions have been renamed as follows and their original names retained as aliases:

| original  | renamed   |
|-----------|-----------|
| rightward | forward   |
| leftward  | backward  |
| rightleft | frontback |
| leftright | backfront |

Additionally, `lode.ordering` now accepts a single integer vector of length the number of cases (alluvia), and will use the vector to sort the lodes within strata at each axis.

Finally, a new vignette showcases this and related functionality using a small example.

## Defunct parameters

The following parameters, deprecated in previous versions, are now defunct (with informative messages):

- `weight` in the `stat_*()`s (replaced with `y`)
- `aggregate.wts` in `stat_alluvium()` (replaced with `aggregate.y`)
- `logical` in the `is_*_form()`s

## Default geom layer parameters

The `width` and `knot.pos` parameters sometimes required by `Geom*$setup_data()` are now set to the same defaults as in the `geom_*()`s when called from a stat. Previously-implemented warnings have been removed.

## Custom lode guidance functions

The `lode.guidance` argument of `stat_alluvial()` now accepts functions as input, making the use of custom functions easier as demonstrated in an example.

# ggalluvial 0.9.2

## Height limits on strata

Parameters `min.height` and `max.height` are introduced to `stat_stratum()` to allow users to omit strata outside a given height range. This is probably most relevant for stratum labeling, as illustrated in the updated vignette.

# ggalluvial 0.9.1

## Suggest **sessioninfo** for `session_info()`

Because the only functional (e.g. out `README.md`) occurrence of **devtools** is to call `session_info()` at the ends of the vignettes, this suggestion and usage are switched to **sessioninfo**.

## markdown formatting

Documentation is slightly reformatted due to switching **roxygen** syntax to markdown.

## z-ordering patch

The internal z-ordering function `z_order_aes` failed to recognize contiguous segments of alluvia, thereby assigning later segments missing values of `'group'` and preventing them from being rendered. This has been corrected.

# ggalluvial 0.9.0

## `geom_alluvium()` patch

An occurrence of `weight` in `geom_alluvium()` was not updated for v0.8.0 and caused `geom_alluvium()` to throw an error in some cases. This has been corrected.

## `geom_flow()` patch

An earlier solution to the z-ordering problem sufficed for matched layers (`*_alluvium()` and `*_flow()`) but failed for the combination of `stat_alluvium()` with `geom_flow()`. This is been corrected in the code for `GeomFlow$draw_panel()`, though a more elegant and general solution is preferred.

## Deprecated parameters removed

The deprecated parameters `axis_width` (all geom layers) and `ribbon_bend` (`geom_alluvium()` and `geom_flow()`) are removed and an explanatory note added to the layers' documentation.

## Vignette on labeling small strata

A vignette illustrating two methods for labeling small strata, using other **ggplot2** extensions, is included.

## `self_adjoin()` export

The internal function `self_adjoin()`, invoked by `geom_flow()`, is revised, exported, documented, and exemplified.

# ggalluvial 0.8.0

## Stat layer functionality

- The `weight` aesthetic for the three `stat_*()` functions is replaced by the `y` aesthetic, so that `scale_y_continuous()` will correctly transform the vertical scales of the layers. An example is provided in the documentation for `stat_alluvium()`. _The `y` aesthetic must be present in order for scales to be correctly transformed._ The `weight` parameter is still available but deprecated.
- For consistency with the switch from `weight` to `y`, the `aggregate.wts` parameter to `stat_alluvium()` is replaced with `aggregate.y`; `aggregate.wts` is deprecated.

## Alluvial data functionality

- Tests for alluvial format are silenced inside the `stat_*()` functions.

# ggalluvial 0.7.0

## Alluvial data functionality

These changes make the functions that test for and convert between alluvial formats behave more like popular functions in the **tidyverse**. Some of the changes introduce backward incompatibilities, but most result in deprecation warnings.

- The functions `is_alluvial_*()` and `to_*()` are renamed to `is_*_form()` and `to_*_form()` for consistency. Their old names are deprecated.
- `is_alluvial()` is deprecated and will be removed in a future version.
- The parameter `logical` is deprecated. In a future version, the functions `is_*_form()` will only return logical values.
- The setting `silent = TRUE` now silences all messages.
- The functions `is_*_form()` now return `FALSE` if any weights are negative, with a message to this effect.
- These functions now accept unquoted variable names for the `key`, `value`, `id`, `weight`, and `diffuse` parameters, using up-to-date **rlang** and **tidyselect** functionality.
- The `axes` parameter in `is_alluvia_form()` and `to_lodes_form()` now accepts `dplyr::vars()` objects, as in `dplyr::select_at()`. Alternatively, variables can be fed to these functions as in `dplyr::select()`, to be collected by `rlang::quos(...)` and used as axis variables. If `axes` is not `NULL`, then such additional arguments are ignored.
- The functions `to_*_form()` now merge their internal reshapen data frames with the distilled or diffused variables in a consistent order, placing the distilled or diffused variables to the left.

# ggalluvial 0.6.0

## CRAN checks for v0.5.0

- The package now `Depends` on R `v3.3.0` (patch number zero) instead of `v3.3.1`. I've been unable to install this version locally, so there is a slight chance of incompatibility that i'll be watchful for going forward.
- The **grid** and **alluvial** packages are now `Suggests` rather than `Imports`.

## Alluvial data functionality

- Source files and documentation for `is_alluvial_*()` and `to_*()` functions are combined; see `help("alluvial-data")`.
- `is_alluvial_alluvia` now prints a message rather than a warning when some combinations of strata are not linked by any alluvia.
- `to_lodes()` now has a `diffuse` parameter to join any original variables to the reformatted data by the `id` variable (alluvium). This makes it possible to assign original variables to aesthetics after reformatting, as illustrated in a new example.
- `to_alluvia()` now has a `distill` parameter to control the inclusion of any original variables that vary within values of `id` into the reformatted data, based on a distilling function that returns a single value from a vector.
- `to_lodes()` now has a logical `discern` parameter that uses `make.unique()` to make stratum values that appear at different axes distinct. The `stat_*()` functions can pass the same parameter internally and print a warning if the data is already in lodes form.

## Layer internals

- `GeomFlow$draw_panel()` now begins by restricting to `complete.cases()`, corresponding to flows with both starting and terminating axes. (This is not done in `StatFlow$compute_panel()`, which would have the effect of excluding missing aesthetic values from legends.)
- `GeomAlluvium$setup_data()` now throws a warning if some color or differentiation aesthetics vary within alluvia.
- A bug in the processing of a custom `lode.ordering` argument by `StatAlluvium$compute_panel()` has been fixed.

# ggalluvial 0.5.0

## Backward incompatibilities

The `ggalluvial()` shortcut function, which included a formula interface, deprecated in version 0.4.0, is removed.

# ggalluvial < 0.5.0

I only started maintaining `NEWS.md` with version 0.5.0.
