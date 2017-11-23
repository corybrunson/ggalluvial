# ggalluvial

## Design

This is a [**ggplot2** extension](http://www.ggplot2-exts.org/) for alluvial diagrams. The alluvial plots implemented here can be used to visualize frequency distributions over time or frequency tables involving several categorical variables. The design is derived mostly from the [**alluvial**](https://github.com/mbojan/alluvial) package, but the **ggplot2** framework induced several conspicuous differences:

- **alluvial** understands a variety of inputs (vectors, lists, data frames), while **ggalluvial** requires a single data frame;
- **alluvial** uses each variable of these inputs as a dimension of the data, whereas **ggalluvial** requires the user to specify the dimensions, either as separate aesthetics or as [key-value pairs](http://tidyr.tidyverse.org/);
- **alluvial** produces both the *alluvia*, which link cohorts across multiple dimensions, and (what are here called) the *strata*, which partition the data along each dimension, in a single function; whereas **ggalluvial** relies on separate layers (stats and geoms) to produce strata, alluvia, and alluvial segments called *lodes* and *flows*.

Issues and pull requests are more than welcome.

## Usage

The latest stable release can be installed from CRAN:

```r
install.packages("ggalluvial")
```

Development versions can be installed from GitHub:

```r
devtools::install_github("corybrunson/ggalluvial", build_vignettes = TRUE)
```

For detailed discussion of the data formats recognized by **ggalluvial** and several examples that illustrate its flexibility and limitations, read the vignette:

```r
vignette(topic = "ggalluvial", package = "ggalluvial")
```
