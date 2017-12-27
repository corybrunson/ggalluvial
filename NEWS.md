# ggalluvial 0.5.0.9000

- Per CRAN checks, the **grid** and **alluvial** packages are now `Suggests` rather than `Imports`.
- Source files and documentation for `is_alluvial_*()` and `to_*()` functions are combined.
- `to_lodes()` now has a `keep` parameter to join any original variables to the reformatted data by the `id` variable (alluvium). This makes it possible to assign original variables to aesthetics after reformatting, as illustrated in a new example.

# ggalluvial 0.5.0

## Backward incompatibilities

The `ggalluvial()` shortcut function, which included a formula interface, deprecated in version 0.4.0, is removed.

# earlier versions

I only started maintaining `NEWS.md` with version 0.5.0.
