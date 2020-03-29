## Test environments

* local OS X install, R 3.6.1
* Rhub (via `devtools::check_rhub()`)
* win-builder (devel, current, and previous; via `devtools::check_win_*()`)

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs.

One comment flagged that examples for one reference page took > 10s to run. (The elapsed time for `stat_alluvium` was 10.03s.) I have trimmed the total number of examples, but each showcases functionality that has previously caused users difficulty.

## Downstream dependencies

There is one downstream dependency on CRAN:

- **easyalluvial**

`revdepcheck::revdep_check()` on both CRAN and GitHub source code produced no ERRORs, WARNINGs, or NOTEs.

(There is also one Bioconductor dependency, **CrossICC**, which has also been checked.)
