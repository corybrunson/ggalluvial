# ggalluvium

A ggplot2 extension for alluvial diagrams.

## Background

While alluvial plots are most popularly used to visualize frequency distributions over time (cite examples), i usually use them to visualize frequency tables involving several categorical variables.

I've relied for several tasks on mbojan's timely [alluvial package](https://github.com/mbojan/alluvial), from which much of the alluvial infrastructure used here is derived. There's been talk of a ggplot2 extension for some time, and i couldn't think of a better way to spend a recent Amtrak ride.

There's much to be improved on here, including some items at the bottom of this page. Comments or pull requests are more than welcome.

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

### Shortcut

```{r}
ggalluvial(as.data.frame(Titanic),
           aes(freq = Freq, axis1 = Class, axis2 = Sex, axis3 = Age,
               fill = Survived)
```

Many more examples can be found in the examples subdirectory.

## Agenda

Here are some remaining tasks:

### Extension to time series

See the [alluvial package](https://github.com/mbojan/alluvial) for examples. More generally, ggalluvium should be able to track the frequencies of the factors of one categorical variable along the factors of another. Using as an example dataset `as.data.frame(as.table(WorldPhones))`, It might make sense for this to be done using `aes(x = Var1, y = Var2, freq = Freq)`, and for `x` and `y` to override any passes to axis aesthetics. Another dataset for exemplary use would be `Seatbelts`.

I'll refer to these as "bivariate" alluvial diagrams, in contrast to the "multivariate" alluvial diagrams already implemented here.

### Interface

The use of `axis[0-9\\.]*` aesthetics to identify axis variables and "smuggle in" axis position information feels ridiculous, but i haven't yet found a better way to allow an arbitrary number of variables as axis aesthetics.
- Should it be possible to specify different widths for different axes? If so, this will have to wait until the syntax for axis variables is changed.

### Formatting

Several problems persist in the current attempt:

- ggplot2 treats the horizontal axis as continuous; the default should be for it to be treated as categorical, with placement determined by axis aesthetics (in the multivariate setting) or by `as(x, "numeric")` (in the bivariate setting), and labeled accordingly. (All categorical variables should be stored as factors anyway.)
- By default, the factors of each axis should proceed downward, not upward, unless they are sorted by frequency within each axis (as in a time series plot). This should simply require a transformation of `y` in both `geom`s.
- While it would require disabling the vertical metric (ticks, frequency labels, and grid), the option should be available to impose spacing between the strata within each axis. In a multivariate diagram, a fixed cumulative gap height should be evenly distributed between the strata in each axis, so that the axis stack up to the same height. In a bivariate diagram, a fixed gap height should separate the strata within each axis.
- For each axis, the data are resorted, by the current axis first and then by the remaining axes in some order. The default order used here (`zigzag`) proceeds to the remaining axes in order of proximity to the current axis, in order to minimize overlaps of alluvia. Other orders are possible and in some settings desirable, and should be available as options.
- Another way to minimize alluvial overlaps is to reorder the strata within each axis (i.e. the levels within each factor variable). Should this be an option, or should it be up to the user to reorder the factor levels before plotting?
- Currently, the alluvium for each group is plotted as a a contiguous closed spline. An alternative would be to plot the alluvial segment connecting each pair of adjacent axes. This would allow the user to control the order in which the alluvia are plotted at each step.
- Should `geom_text(stat = "stratum")` be incorporated as a logical option in `geom_stratum` that defaults to `TRUE`? Would this require writing a separate `geom_stratum_label`

### Miscellany

- If neither `x` and `y` nor any axes are provided, then should the `geom`s throw an error or produce a single-axis plot?
- Print a warning or throw an error if the user specifies a `group` aesthetic in `stat_stratum`, at least unless and until it serves some purpose. Currently calls including a `group` specification either don't matter (when they respect the interaction of the discrete variables), or destroy the plot (otherwise). (The `group` argument allows a `stat_*` or `geom_*` to perform subset-level calculations within single panels, but both alluvial and stratal calculations must take the entire panel into account; it's not clear to me what role subsetting within a panel (other than to specify within-flow aesthetic differences, which is done using other aesthetic arguments) would play in an alluvial diagram.)
