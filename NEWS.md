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

- `GeomFlow$draw_panel()` now begins by restricting to `complete.cases()`, corresponding to one-sided flows. (These are not removed in `StatFlow$compute_panel()`, which would exclude missing aesthetic values from legends.)
- `GeomAlluvium$setup_data()` now throws a warning if some color or differentiation aesthetics vary within alluvia.
- A bug in the processing of a custom `lode.ordering` argument by `StatAlluvium$compute_panel()` has been fixed.

# ggalluvial 0.5.0

## Backward incompatibilities

The `ggalluvial()` shortcut function, which included a formula interface, deprecated in version 0.4.0, is removed.

# earlier versions

I only started maintaining `NEWS.md` with version 0.5.0.
