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

On some runs, one NOTE flagged several last names in the DESCRIPTION as possibly misspelled words. (These have been checked.)

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

On another platform (Ubuntu Linux 20.04.1 LTS, R-release, GCC), one NOTE flagged that examples for five reference pages took > 5s to run:
```
* checking examples ... [30s/113s] NOTE
Examples with CPU (user + system) or elapsed time > 5s
               user system elapsed
stat_alluvium 7.594  0.016  28.568
stat_flow     4.520  0.019  17.349
stat_stratum  4.045  0.004  15.033
geom_flow     3.298  0.016  11.816
alluvial-data 2.431  0.021   9.928
geom_lode     2.355  0.007   9.036
geom_alluvium 1.607  0.040   6.237
```
I don't know why this is, as the number of examples outside `\donttest` sections is smaller than previous submissions.

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
