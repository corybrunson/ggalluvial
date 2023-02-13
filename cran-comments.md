## Timing

Version 0.12.4 was just released in order to resolve a bug that arose with the upgrade to {dplyr} 1.1.0. Unfortunately, that fix simply shifted the bug to interoperation with {dplyr} <= 1.0.10. This quick patch resolves the bug for all versions. Discussion can be found under issues #107 <https://github.com/corybrunson/ggalluvial/issues/107> and #108 <https://github.com/corybrunson/ggalluvial/issues/108> on GitHub.

## Test environments

* local OS X install, R 4.2.1
  * `devtools::check()`
  * `devtools::check(env_vars = c('_R_CHECK_DEPENDS_ONLY_' = "true"))`
* Win-Builder
  * `devtools::check_win_oldrelease()`
  * `devtools::check_win_release()`
  * `devtools::check_win_devel()`
* R-hub
  * `rhub::check_for_cran()`
  * `rhub::check_for_cran(platforms = "macos-highsierra-release-cran")`

In response to a previous failed submission, {vdiffr} tests are now skipped on CRAN and when not installed, using `testthat::skip_*()`.

### R CMD check results

There were no ERRORs, WARNINGs, or NOTEs.

### WinBuilder

There were no ERRORs or WARNINGs.

On one platform (x86_64-w64-mingw32 (64-bit); the old release), one NOTE flagged several last names in the DESCRIPTION as possibly misspelled words (these have been checked), while another NOTE flagged examples that took > 10s to run:
```
** running examples for arch 'i386' ... [39s] NOTE
Examples with CPU (user + system) or elapsed time > 10s
              user system elapsed
stat_alluvium 9.86   0.06   10.14
** running examples for arch 'x64' ... [42s] NOTE
Examples with CPU (user + system) or elapsed time > 10s
               user system elapsed
stat_alluvium 10.64   0.05   10.97
```
I still don't know why this is, as the number of examples outside `\donttest` sections in the documentation for each function is no larger than previous submissions.

### Rhub

There were no WARNINGs.

On one platform (Windows Server 2022, R-devel, 64 bit), one NOTE read as follows:
```
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```
As documented, this note is probably due to a MiKTeX bug that can be ignored:
<https://github.com/r-hub/rhub/issues/503>

## Downstream dependencies

CRAN reports the following reverse depends:

- {iotarelr}

CRAN reports the following reverse imports:

- {bandle} (Bioconductor)
- {easyalluvial}
- {ethnobotanyR}
- {immunarch}
- {MutationalPatterns} (Bioconductor)
- {projectR} (Bioconductor)
- {scRepertoire} (Bioconductor)
- {standR} (Bioconductor)

CRAN reports the following reverse suggests:

- {ISAnalytics} (Bioconductor)
- {longmixr}
- {MicrobiotaProcess} (Bioconductor)
- {Platypus}
- {plotly}
- {SCpubr}
- {sigminer}

`revdepcheck::revdep_check()` on CRAN source code produced no ERRORs, WARNINGs, or NOTEs. Checks on GitHub/Bioconductor source code produced the ERROR with {projectR} reproduced here: <https://github.com/genesofeve/projectR/issues/27#issuecomment-1415490762>
The maintainer indicated on that issue that this is a problem with the development version of {projectR}, not with the dependency.
