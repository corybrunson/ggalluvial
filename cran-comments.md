## Test environments

* local OS X install, R 4.5.2
  * `devtools::check()`
  * `devtools::check(env_vars = c('_R_CHECK_DEPENDS_ONLY_' = "true"))`
  * `devtools::check(manual = TRUE, remote = TRUE)`
* Win-Builder
  * `devtools::check_win_oldrelease()`
  * `devtools::check_win_release()`
  * `devtools::check_win_devel()`

In response to a previous failed submission, {vdiffr} tests are now skipped on CRAN and when not installed, using `testthat::skip_*()`.

### local OS X install

There were no ERRORs, WARNINGs, or NOTEs.

### WinBuilder

There were no ERRORs or WARNINGs.
The only NOTE concerned a (possibly) broken URL, which has been checked.

## Downstream dependencies

Most checks were problem-free.
The check for {DOtools} raised problems, apparently due to my own system's Python limitations.
The check for {bandle} failed, apparently due to my own system's C++ limitations.
I've raised issues at the two repos just in case.
