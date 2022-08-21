
<!-- README.md is generated from README.rmd. Please edit that file -->

# ggalluvial

[![Travis](https://travis-ci.org/corybrunson/ggalluvial.svg?branch=main)](https://travis-ci.org/corybrunson/ggalluvial)
[![CRAN](http://www.r-pkg.org/badges/version/ggalluvial)](https://cran.r-project.org/package=ggalluvial)
[![downloads](https://cranlogs.r-pkg.org/badges/ggalluvial)](https://cran.r-project.org/package=ggalluvial)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3836748.svg)](https://doi.org/10.5281/zenodo.3836748)
[![JOSS](https://joss.theoj.org/papers/10.21105/joss.02017/status.svg)](https://doi.org/10.21105/joss.02017)

This is a [**ggplot2** extension](https://exts.ggplot2.tidyverse.org/)
for alluvial plots.

## Design

The alluvial plots implemented here can be used to visualize frequency
distributions over time or frequency tables involving several
categorical variables. The design is inspired by the
[**alluvial**](https://github.com/mbojan/alluvial) package, but the
**ggplot2** framework induced several conspicuous differences:

-   **alluvial** understands a variety of inputs (vectors, lists, data
    frames), whereas **ggalluvial** requires a single data frame;
-   **alluvial** uses each variable of these inputs as a dimension of
    the data, whereas **ggalluvial** requires the user to specify the
    dimensions, either as separate aesthetics or as [key-value
    pairs](https://tidyr.tidyverse.org/);
-   **alluvial** produces both the *alluvia*, which link cohorts across
    multiple dimensions, and (what are here called) the *strata*, which
    partition the data along each dimension, in a single function;
    whereas **ggalluvial** relies on separate layers (stats and geoms)
    to produce strata, alluvia, and alluvial segments called *lodes* and
    *flows*.

Additionally, **ggalluvial** arranges these layers vertically without
gaps, so that the secondary plotting axis indicates the cumulative
values of the strata at each dimension.

## Installation

The latest stable release can be installed from CRAN:

``` r
install.packages("ggalluvial")
```

The [`cran` branch](https://github.com/corybrunson/ggalluvial/tree/cran)
will contain the version most recently submitted to
[CRAN](https://cran.r-project.org/package=ggalluvial). It is duplicated
in the `master` branch, from which source the
[website](https://corybrunson.github.io/ggalluvial/) is built.

The development version can be installed from the (default) `main`
branch on GitHub:

``` r
remotes::install_github("corybrunson/ggalluvial@main", build_vignettes = TRUE)
```

Note that, in order to build the vignettes, the imported packages
**alluvial**, **ggfittext**, and **ggrepel** must be installed. To skip
this step, leave `build_vignettes` unspecified or set it to `FALSE`.

The [`optimization`
branch](https://github.com/corybrunson/ggalluvial/tree/optimization)
contains a development version with experimental functions to reduce the
number or area of alluvial overlaps (see issue
[\#6](https://github.com/corybrunson/ggalluvial/issues/6)). Install it
as follows:

``` r
remotes::install_github("corybrunson/ggalluvial", ref = "optimization")
```

Note, however, that this branch has not kept pace with the `main` branch
or with recent upgrades on CRAN.

## Usage

### Example

Here is how to generate an alluvial plot representation of the
multi-dimensional categorical dataset of passengers on the Titanic:

``` r
titanic_wide <- data.frame(Titanic)
head(titanic_wide)
#>   Class    Sex   Age Survived Freq
#> 1   1st   Male Child       No    0
#> 2   2nd   Male Child       No    0
#> 3   3rd   Male Child       No   35
#> 4  Crew   Male Child       No    0
#> 5   1st Female Child       No    0
#> 6   2nd Female Child       No    0
ggplot(data = titanic_wide,
       aes(axis1 = Class, axis2 = Sex, axis3 = Age,
           y = Freq)) +
  scale_x_discrete(limits = c("Class", "Sex", "Age"), expand = c(.2, .05)) +
  xlab("Demographic") +
  geom_alluvium(aes(fill = Survived)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_minimal() +
  ggtitle("passengers on the maiden voyage of the Titanic",
          "stratified by demographics and survival")
```

![](man/figures/README-unnamed-chunk-6-1.png)<!-- -->

The data is in “wide” format, but **ggalluvial** also recognizes data in
“long” format and can convert between the two:

``` r
titanic_long <- to_lodes_form(data.frame(Titanic),
                              key = "Demographic",
                              axes = 1:3)
head(titanic_long)
#>   Survived Freq alluvium Demographic stratum
#> 1       No    0        1       Class     1st
#> 2       No    0        2       Class     2nd
#> 3       No   35        3       Class     3rd
#> 4       No    0        4       Class    Crew
#> 5       No    0        5       Class     1st
#> 6       No    0        6       Class     2nd
ggplot(data = titanic_long,
       aes(x = Demographic, stratum = stratum, alluvium = alluvium,
           y = Freq, label = stratum)) +
  geom_alluvium(aes(fill = Survived)) +
  geom_stratum() + geom_text(stat = "stratum") +
  theme_minimal() +
  ggtitle("passengers on the maiden voyage of the Titanic",
          "stratified by demographics and survival")
```

![](man/figures/README-unnamed-chunk-7-1.png)<!-- -->

### Documentation

For detailed discussion of the data formats recognized by **ggalluvial**
and several examples that illustrate its flexibility and limitations,
read the technical vignette:

``` r
vignette(topic = "ggalluvial", package = "ggalluvial")
```

Several additional vignettes offer detailed solutions to specific needs:

-   “Labeling small strata” (`"labels"`) for how to elegantly label
    strata of a wide range of heights in an alluvial plot;
-   “The Order of the Rectangles” (`"order-rectangles"`) for how to
    control the positioning of strata and lodes in an alluvial plot; and
-   “Tooltips for ggalluvial plots in Shiny apps” (`"shiny"`) for how to
    incorporate alluvial plots into interactive apps.

The object documentation includes several more examples. Use `help()` to
call forth more detail on

-   any layer (`stat_*` or `geom_*`),
-   the conversion functions (`to_*_form`), and
-   the data sets installed with the package (`vaccinations` and
    `majors`).

### Short form

For some more digestible guidance on using **ggalluvial**, check out
three cheat sheets and demos by students in Joyce Robbins’s [Exploratory
Data Analysis and Visualization Community Contribution
Project](https://jtr13.github.io/cc21fall2/index.html):

-   an
    [introduction](https://jtr13.github.io/cc21fall2/ggalluvial-cheatsheet.html)
    by Meggie Wen (Chapter 33)
-   a [cheat
    sheet](https://cheatography.com/seleven/cheat-sheets/ggalluvial/) by
    Qingyi Zhang (Chapter 14)
-   a [blog about how this package implements alluvial
    plots](https://medium.com/@arnavsaxena96/all-about-alluvial-diagrams-21da1505520b)
    by Arnav Saxena (Chapter 134)

## Acknowledgments

### Resources

Development of this package benefitted from the use of equipment and the
support of colleagues at [UConn Health](https://health.uconn.edu/) and
at [UF Health](https://ufhealth.org/).

### Contribute

Contributions in any form are more than welcome! Pretty much every fix
and feature of this package derives from a problem or question posed by
someone with datasets or design goals i hadn’t anticipated. See the
[CONTRIBUTING](https://github.com/corybrunson/ggalluvial/blob/main/CONTRIBUTING.md)
file for guidance, and please respect the [Code of
Conduct](https://github.com/corybrunson/ggalluvial/blob/main/CODE_OF_CONDUCT.md).

### Cite

If you use **ggalluvial**-generated figures in publication, i’d be
grateful to hear about it! You can also cite the package according to
`citation("ggalluvial")`.
