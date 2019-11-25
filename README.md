
<!-- README.md is generated from README.rmd. Please edit that file -->

# ggalluvial

[![Travis build
status](https://travis-ci.org/corybrunson/ggalluvial.svg?branch=master)](https://travis-ci.org/corybrunson/ggalluvial)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/ggalluvial)](https://cran.r-project.org/package=ggalluvial)

This is a [**ggplot2** extension](http://www.ggplot2-exts.org/) for
alluvial plots.

## Design

The alluvial plots implemented here can be used to visualize frequency
distributions over time or frequency tables involving several
categorical variables. The design is inspired by the
[**alluvial**](https://github.com/mbojan/alluvial) package, but the
**ggplot2** framework induced several conspicuous differences:

  - **alluvial** understands a variety of inputs (vectors, lists, data
    frames), whereas **ggalluvial** requires a single data frame;
  - **alluvial** uses each variable of these inputs as a dimension of
    the data, whereas **ggalluvial** requires the user to specify the
    dimensions, either as separate aesthetics or as [key-value
    pairs](http://tidyr.tidyverse.org/);
  - **alluvial** produces both the *alluvia*, which link cohorts across
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

The [cran branch](https://github.com/corybrunson/ggalluvial/tree/cran)
will contain the version most recently submitted to
[CRAN](https://cran.r-project.org/package=ggalluvial).

Development versions can be installed from GitHub:

``` r
remotes::install_github("corybrunson/ggalluvial", build_vignettes = TRUE)
```

The [optimization
branch](https://github.com/corybrunson/ggalluvial/tree/optimization)
contains a development version with experimental functions to reduce the
number or area of alluvial overlaps (see issue
[\#6](https://github.com/corybrunson/ggalluvial/issues/6)). Install it
as follows:

``` r
remotes::install_github("corybrunson/ggalluvial", ref = "optimization")
```

Note, however, that this branch has not kept pace with the `master`
branch or with recent upgrades on CRAN.

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
  scale_x_discrete(limits = c("Class", "Sex", "Age"), expand = c(.1, .05)) +
  xlab("Demographic") +
  geom_alluvium(aes(fill = Survived)) +
  geom_stratum() + geom_text(stat = "stratum", label.strata = TRUE) +
  theme_minimal() +
  ggtitle("passengers on the maiden voyage of the Titanic",
          "stratified by demographics and survival")
#> Warning: The parameter `label.strata` is deprecated.
#> Pass arguments to `infer.label` instead.
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

The documentation contains several examples; use `help()` to call forth
examples of any layer (`stat_*` or `geom_*`) or of the conversion
functions (`to_*_form`).

## Acknowledgment

### Resources

Development of this package benefitted from the use of equipment and the
support of colleagues at [UConn Health](https://health.uconn.edu/).

### Contribute

[Issues](https://github.com/corybrunson/ggalluvial/issues) and [pull
requests](https://github.com/corybrunson/ggalluvial/pulls) are more than
welcome\! Pretty much every fix and feature of this package derives from
a problem or question posed by someone with datasets or design goals i
hadn’t anticipated.

### Cite

If you use **ggalluvial**-generated figures in publication, i’d be
grateful to hear about it\! You can also cite the package according to
`citation("ggalluvial")`.
