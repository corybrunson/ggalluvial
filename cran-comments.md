## Test environments

* local OS X install, R 4.0.0
* Rhub (via `devtools::check_rhub()`)
* win-builder (devel, current, and previous; via `devtools::check_win_*()`)

Test on Rhub produced several of one type of WARNING: "Non-file package-anchored link(s) in documentation". These concerned cross-references formatted using roxygen2 that were not flagged by other checks.

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
