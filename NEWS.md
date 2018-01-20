# ggalluvial 0.5.0.9001

- `to_lodes()` now has a `relevel.strata` parameter to sort or customize the factor levels of the variable containing the aggregated strata. The `stat_*()` functions can pass the same parameter internally and print a warning if the data is already in lodes form.
- `GeomFlow$draw_panel()` now begins by removing `complete.cases()`, corresponding to one-sided flows. (These are not removed in `StatFlow$compute_panel()`, which would exclude missing aesthetic values from legends.)

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
