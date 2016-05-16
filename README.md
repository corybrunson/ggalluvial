# ggalluvium

A ggplot2 extension for alluvial diagrams.

## Background

While alluvial plots are most popularly used to visualize frequency distributions over time (cite examples), i usually use them to visualize frequency tables involving several categorical variables.

I've relied for several tasks on mbojan's timely [alluvial package](https://github.com/mbojan/alluvial), from which much of the alluvial infrastructure used here is derived. There's been talk of a ggplot2 extension for some time, and i couldn't think of a better way to spend a recent Amtrak ride.

I'm sure that there's much to be improved on here. Comments or pull requests are more than welcome!

## Install

This package is not stable! For one thing, hopefully the argument syntax can be made more elegant. So i don't anticipate sending this to CRAN any time soon. Here's how to install in the meantime:

```{r}
if (require(devtools)) {
    install.packages("devtools")
}
devtools::install_github("corybrunson/ggalluvium")
```

## Examples

### Basic

```r
ggplot(as.data.frame(Titanic),
       aes(freq = Freq,
       axis1 = Class, axis2 = Sex, axis3 = Age, axis4 = Survived)) +
    geom_alluvium() +
    geom_stratum() +
    geom_text(stat = "stratum") +
    ggtitle("Titanic passenger demographic and survival data") +
    theme_bw()
```

### Aesthetics

```{r}
ggplot(as.data.frame(Titanic),
       aes(freq = Freq,
       axis1 = Class, axis2 = Sex, axis3 = Age)) +
    geom_alluvium(aes(fill = Age:Sex, alpha = Class, color = Survived)) +
    scale_color_manual(values = c("black", "white")) +
    ggtitle("Titanic passenger demographic and survival data") +
    theme_bw()
```

### Facets

```{r}
ggplot(as.data.frame(Titanic),
       aes(freq = Freq, axis1 = Class, axis2 = Sex)) +
    geom_alluvium(aes(fill = Age)) +
    geom_stratum() + geom_text(stat = "stratum") +
    facet_wrap(~ Survived, scales = "free_y")
```

## Agenda

Note to self: incorporate these as issues after pushing to GitHub.

- axis labels (horizontal axis of plot)
- invert y values so that factor levels proceed downward
- special case of no axes (or throw error)
- gaps between factors within axis (and, if so, remove vertical scale)
- control within-factor group orderings at each axis (default is zigzag)
- split flows into ribbon segments and control plotting order of segments
- make axis input more natural
- extend to categorical-versus-ordinal (e.g. time series) diagrams
