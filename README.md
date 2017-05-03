# ggalluvial

A ggplot2 extension for alluvial diagrams.

## Background

The alluvial plots implemented here can be used to visualize frequency distributions over time or frequency tables involving several categorical variables. Much of the infrastructure comes from the [**alluvial**](https://github.com/mbojan/alluvial) package, but the **ggplot2** framework induced several conspicuous differences:

- **alluvial** understands a variety of inputs (vectors, lists, data frames), while **ggalluvial** requires a single data frame;
- **alluvial** uses each variable of these inputs as an axis, whereas **ggalluvial** requires the user to specify each axis individually, either by calling separate aesthetics or via a `tidyr::gather()`ed data format;
- **alluvial** produces both the alluvial flows (alluvia) and what are here called the strata in a single function (`alluvial()`), whereas **ggalluvial** relies on separate functions to produce strata, alluvia, and alluvial segments called *lodes* and *flows*.

There's much to be improved on here still. Comments or pull requests are more than welcome.

## Install

This package is not yet CRAN-ready. Here's how to install in the meantime:

```{r}
if (!require(devtools)) {
  install.packages("devtools")
}
devtools::install_github("corybrunson/ggalluvial", build_vignettes = TRUE)
library(ggalluvial)
```

For detailed discussion of the data formats recognized by **ggalluvial** and several examples that illustrate its flexibility and limitations, see the vignette:

```{r}
vignette(topic = "ggalluvial")
```

The shortcut function `ggalluvial()`, which includes a formula interface, is not included in the vignette and may be deprecated in the next version and excluded from the eventual CRAN release. (Let me know if you think it should be kept or improved!)
