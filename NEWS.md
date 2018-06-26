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

# earlier versions

I only started maintaining `NEWS.md` with version 0.5.0.
