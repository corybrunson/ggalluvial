# ggalluvial 0.6.1

## Alluvial data functionality

- The functions `is_alluvial_*()` and `to_*()` are renamed to `is_*_form()` and `to_*_form()` for consistency. Their old names are deprecated.
- The setting `silent = TRUE` now silences all messages.
- The functions `is_*_form()` now return `FALSE` if any weights are negative, with a message to this effect.
- These functions now accept unquoted variable names for the `key`, `value`, `id`, `weight`, and `diffuse` parameters, using up-to-date **rlang** and **tidyselect** functionality.
- The `axes` parameter in `is_alluvia_form()` and `to_lodes_form()` now accepts `dplyr::vars()` objects, as in `dplyr::select_at()`. Alternatively, variables can be fed to these functions as in `dplyr::select()`, to be collected by `rlang::quos(...)` and used as axis variables. If `axes` is not `NULL`, then such additional arguments are ignored, in `is_alluvial()` as well as in `is_alluvia_form()`.
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
