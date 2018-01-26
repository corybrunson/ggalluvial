# ggalluvial 0.5.0.9002

- BC: Change `keep` parameter to `diffuse` (more mnemonic with `distill`).
- `geom_alluvium()` now throws a warning if some color or differentiation aesthetics vary within alluvia.
- `to_lodes()` now has a logical `discern` parameter that uses `make.unique()` to make stratum values that appear at different axes distinct. The `stat_*()` functions can pass the same parameter internally and print a warning if the data is already in lodes form.

# ggalluvial 0.5.0.9001

- `GeomFlow$draw_panel()` now begins by removing `complete.cases()`, corresponding to one-sided flows. (These are not removed in `StatFlow$compute_panel()`, which would exclude missing aesthetic values from legends.)
- `to_alluvia()` now has a `distill` parameter to control the inclusion of any original variables that vary within values of `id` into the reformatted data, based on a distilling function that returns a single value from a vector.
- `is_alluvial_alluvia` now prints a message rather than a warning when some combinations of strata are not linked by any alluvia.

# ggalluvial 0.5.0.9000

- Per CRAN checks, the package now `Depends` on R `v3.3.0` (patch number zero) instead of `v3.3.1`. I've been unable to install this version locally, so there is a slight chance of incompatibility that i'll be watchful for going forward.
- Per CRAN checks, the **grid** and **alluvial** packages are now `Suggests` rather than `Imports`.
- Source files and documentation for `is_alluvial_*()` and `to_*()` functions are combined.
- `to_lodes()` now has a `keep` parameter to join any original variables to the reformatted data by the `id` variable (alluvium). This makes it possible to assign original variables to aesthetics after reformatting, as illustrated in a new example.

# ggalluvial 0.5.0

## Backward incompatibilities

The `ggalluvial()` shortcut function, which included a formula interface, deprecated in version 0.4.0, is removed.

# earlier versions

I only started maintaining `NEWS.md` with version 0.5.0.
