## Test environments

* local OS X install, R 4.0.0 (via `devtools::check()`)
* Rhub (via `rhub::check_for_cran()`)
* win-builder (devel, current, and previous; via `devtools::check_win_*()`)

There were no ERRORs, WARNINGs, or NOTEs.
In response to a previous failed submission, **vdiffr** tests are now skipped on CRAN.

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs.

One comment flagged that examples for one reference page (`stat_alluvium`) took > 10s to run. I have trimmed the total number of examples, but each showcases functionality that has previously caused users difficulty.

## Downstream dependencies

There are three downstream dependencies on CRAN:

- **easyalluvial**
- **sigminer**
- **immunarch**

There are also two Bioconductor dependencies:

- **CrossICC**
- **projectR**

`revdepcheck::revdep_check()` on both CRAN and GitHub source code produced no ERRORs, WARNINGs, or NOTEs.
