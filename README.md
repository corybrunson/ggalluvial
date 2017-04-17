# ggalluvial

A ggplot2 extension for alluvial diagrams, which visualize frequency tables in several dimensions.

## Background

While alluvial plots are most popularly used to visualize frequency distributions over time, i usually use them to visualize frequency tables involving several categorical variables.

I've relied for several tasks on [mbojan](https://github.com/mbojan)'s timely [**alluvial**](https://github.com/mbojan/alluvial) package, from which much of the alluvial infrastructure used here is derived. Besides being tailored to ggplot2, there are several conspicuous differences between these packages:

- **alluvial** understands a variety of inputs (vectors, lists, data frames), while **ggalluvial** requires a single data frame;
- **alluvial** uses each variable of these inputs as an axis, whereas **ggalluvial** requires the user to specify each axis individually, either by calling separate aesthetics or via a `tidyr::gather()`ed data format;
- **alluvial** produces both the alluvial flows (alluvia) and what are here called the strata in a single function (`alluvial()`), whereas **ggalluvial** relies on the separate functions `*_alluvium()` and `*_stratum()` to produce these elements.

There's much to be improved on here. Comments or pull requests are more than welcome.

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

The shortcut function `ggalluvial()`, which includes a formula interface, is not included in the vignette and may not be included in the eventual CRAN release. (Let me know if you think it should be kept or improved!)
