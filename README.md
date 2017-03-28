# ggalluvial

A ggplot2 extension for alluvial diagrams, which visualize frequency tables in several dimensions.

## Background

While alluvial plots are most popularly used to visualize frequency distributions over time, i usually use them to visualize frequency tables involving several categorical variables.

I've relied for several tasks on [mbojan](https://github.com/mbojan)'s timely [alluvial](https://github.com/mbojan/alluvial) package, from which much of the alluvial infrastructure used here is derived. Besides being tailored to ggplot2, there are several conspicuous differences between these packages:

* alluvial understands a variety of inputs (vectors, lists, data frames), while ggalluvial requires a single data frame;
* alluvial uses each variable of these inputs as an axis, whereas ggalluvial requires the user to specify each axis individually;
* alluvial produces both the alluvial flows (alluvia) and what are here called the stratal blocks (or just strata) in a single function (`alluvial()`), whereas ggalluvial relies on the separate functions `*_alluvium()` and `*_stratum()` to produce these elements.

There's much to be improved on here, including some items at the bottom of this page. Comments or pull requests are more than welcome.

## Install

This package is serviceable (in the narrow sense of producing the kinds of images i had wanted), *but it should not be viewed as stable*. For one thing, hopefully the argument syntax can be made more elegant. So i don't anticipate sending this to CRAN any time soon. Here's how to install in the meantime:

```{r}
if (!require(devtools)) {
  install.packages("devtools")
}
devtools::install_github("corybrunson/ggalluvial")
library(ggalluvial)
```

## Examples

### Basic

```r
png(height = 360, width = 600, file = "inst/fig/example-basic.png")
ggplot(as.data.frame(UCBAdmissions),
       aes(weight = Freq, axis1 = Gender, axis2 = Dept)) +
  geom_alluvium(aes(fill = Admit)) +
  geom_stratum() + geom_text(stat = "stratum") +
  ggtitle("UC Berkeley admissions and rejections, by sex and department")
dev.off()
```

![Basic](inst/fig/example-basic.png)

### Facets

The following example demonstrates `ggalluvial`'s compatibility with facets. It also illustrates the effect of using aesthetics beyond those assigned to axes: Since they are incorporated into the data frame fed to `StatAlluvium` or `StatStratum`, whose `setup_data()` functions aggregate (hence sort) these data by all available columns besides the `weight` assignment (frequencies), the resulting alluvia end up stratified by these variables as well.

```{r}
png(height = 360, width = 600, file = "inst/fig/example-facet.png")
ggplot(as.data.frame(Titanic),
       aes(weight = Freq, axis1 = Class, axis2 = Sex)) +
  geom_alluvium(aes(fill = Age)) +
  geom_stratum() + geom_text(stat = "stratum") +
  facet_wrap(~ Survived, scales = "free_y") +
  scale_x_continuous(breaks = 1:2, labels = c("Class", "Sex")) +
  ggtitle("Titanic passenger data, faceted by survival")
dev.off()
```

![Facets](inst/fig/example-facet.png)

### Alternate style

The following adjustments to `width` and the `geom_stratum` aesthetics mimic a popular style for alluvial diagrams, in which axes are thin and black and number but two, while labels are placed on the outer side of each. These diagrams are quite elegant, but to my mind the intrinsic rigidity of ggplot2 requires keeping the strata contiguous, so that the total height of each axis equals the cumulative weight (and hence the vertical axis makes sense).

```{r}
png(height = 360, width = 600, file = "inst/fig/example-style.png")
ggplot(as.data.frame(Titanic),
       aes(axis1 = Age, axis2 = Sex, axis3 = Class,
           fill = Survived, weight = Freq)) +
  geom_alluvium(width = 1/12) +
  geom_stratum(fill = "black", color = "lightgrey", width = 1/12) +
  scale_x_continuous(breaks = 1:3, labels = c("Age", "Sex", "Class")) +
  geom_label(stat = "stratum") +
  ggtitle("Titanic passenger data")
dev.off()
```

![Style](inst/fig/example-style.png)

### Time series

A more straightforward class of alluvial diagrams track discrete distributions over time. I refer to these as time series alluvial diagrams, in contrast to the categorical alluvial diagrams originally implemented. As in the categorical case, these diagrams are compatible with facets.

```{r}
png(height = 360, width = 600, file = "inst/fig/example-ts.png")
wph <- as.data.frame(as.table(WorldPhones))
names(wph) <- c("Year", "Region", "Telephones")
ggplot(wph,
       aes(x = Year, group = Region, weight = Telephones)) +
  geom_alluvium_ts(aes(fill = Region, colour = Region)) +
  ggtitle("Number of telephones across world regions over time")
dev.off()
```

![Time series](inst/fig/example-ts.png)

### Shortcut

The default shortcut `ggalluvial.default` requires that a multidimensional frequency table be reformatted as a data frame, consistent with the principles of ggplot2 (see [here](https://rpubs.com/hadley/ggplot2-layers), section "Data"). The formula interface `ggalluvial.formula` is triggered when "formula" is an argument in the function call, or if the first (unnamed) argument is a call; as shown below, it accepts frequency tables as well, in which case the flows will automatically be weighted by the frequencies. It will produce a categorical alluvial diagram if the RHS of the formula has at least two variables, and a time series diagram if the RHS has only one variable. Both shortcut functions include the axis labels fix used explicitly in the previous example.

```{r}
png(height = 360, width = 600, file = "inst/fig/example-shortcut-1.png")
ggalluvial(Survived ~ Class + Sex + Age, Titanic)
dev.off()
```

![Shortcut](inst/fig/example-shortcut-1.png)

The following example reproduces the original time series example from [alluvial](https://github.com/mbojan/alluvial). Some distinguishability is lost due to the absence of gaps between the alluvial flows, but on the whole the effect is quite nice.

```{r}
png(height = 360, width = 600, file = "inst/fig/example-shortcut-2.png")
ggalluvial(country ~ year, alluvial::Refugees, weight = "refugees")
dev.off()
```

![Shortcut](inst/fig/example-shortcut-2.png)

Many more examples can be found in the examples subdirectory (or via `help()`).

## Under the hood

The core of the package consists in two `stat_*`-`geom_*` pairs of layer functions, `*_alluvium` and `*_stratum`.

- `ggplot()` first processes the given data and aesthetics to produce a data frame having columns the aesthetic defaults and/or declarations, plus `PANEL` (for faceting) and `group` (for subset-wise transformation or plotting). At present it is best not to declare the `group` aesthetic, since the main functions make use of its default values (the interaction of the discrete variables).
- `stat_alluvium()` takes the processed data, aggregates the `weight` variable (frequencies) along all of the other variables (which has the effect of sorting the data), and calculates the cumulative frequency within each axis in the order determined by the lode parameters `load_favor` and `load_order` (see the documentation), each of which sums to the total frequency. Thus, the number of steps in the cumulative frequency is the same at each axis. It then returns the coordinates (`x`, `xmin`, `xmax`, `y`, `ymin`, `ymax`) of the group flows at each axis, from left to right. (Try `geom_line(stat = "alluvium")` in place of `geom_alluvium()` to more directly discern the output of `stat_alluvium()`.)
- `geom_alluvium()` derives coordinates for, and plots, closed splines centered at the coordinates provided by `stat_alluvium()`.
- `stat_stratum()` similarly aggregates the processed data at each axis, but according to that axis's distinct values (in order of level, for factor variables), so that the number of steps at each axis depends on the number of values of the corresponding variable. It row-binds these aggregated data, with each row corresponding to a stratum of one axis. Finally, it appends the coordinates of the center of each stratum (block), along with its width. (The value column is named `label` so that `geom_text(stat = "stratum")` places appropriate labels at the locations of the strata.)
- `geom_stratum()` uses the coordinates provided by `stat_stratum()` to plot the rectangles that form the strata.

Time series alluvial diagrams are produced by `stat_alluvium_ts()` and `geom_alluvium_ts()` in a more straightforward way. In contrast to the functions that produce categorial alluvial diagrams (see "Idiosyncrasies" below), to these functions the `group` parameter is essential: it defines the alluvia that widen and tighten with changes in each group's volume over time (see "Examples" above). There are no corresponding stratum functions for time series diagrams.

## Idiosyncrasies

For categorial (rather than time series) data, nested mosaic plots (see [here](https://cran.r-project.org/web/packages/vcdExtra/vignettes/vcd-tutorial.pdf), also [here](http://vita.had.co.nz/papers/prodplots.pdf)) are a more natural candidate for a grammar of graphics implementation since the coordinate dimensions of the plot window arise as compositions of the values of the categorical variables with their frequencies—these being the two dimensions of the basic bar chart (`geom_bar()`). Alluvial diagrams, in contrast, take the categorical variables themselves (the "axes") as the values distributed along one dimension of the plot window. This avoids the hierarchicality of nested mosaics but imposes its own ordinality on the axes. More consequentially for this implementation, though, it violates the synchrony of data "tidiness" with graphic "grammar" built into ggplot2.

The workaround used here is to allow to declare several numbered axis aesthetics (see section "Examples"), which are then used in an ad hoc way (in particular, by extracting numerical information from their names) to determine the order of the axes in the diagram. This prevents ggplot2 from recognizing the original variables as axis names, so that the user must contribute these using `scale_x_continuous()`, being careful to ensure that the `breaks` and `labels` match aesthetic assignments. The trick does allow non-integer values to be used as axis numbers, so the user can control the positions of the axes along the horizontal plot axis if they prefer (because why not). This workaround also relies on the automated `group` variable for handling the data, so `group` declarations in the `ggplot()` are ignored (as illustrated in the package examples; execute `example(package = "ggalluvial", topic = "alluvium")` in R).

An alternative would be to first "alluviate" the data, i.e. to melt the axis variables into two columns, perhaps "Axis" (indicating the variable name) and "Stratum" (the value), holding the frequency variable as an index variable. This raises a different problem, of how to stratify other aesthetics (e.g. `color`) by a single axis variable. I've toyed with this but haven't hit upon a satisfactory approach. I'd love to see a more "grammatical" implementation than the one used here.

## Agenda

Here are some remaining tasks:

### Interface

- The use of `axis[0-9\\.]*` aesthetics to identify axis variables and "smuggle in" axis position information feels ridiculous, but i haven't yet found a better way to allow an arbitrary number of variables as axis aesthetics.

### Formatting

Several problems persist in the current attempt:

- ggplot2 treats the horizontal axis as continuous; the default should be for it to be treated as categorical, with placement determined by axis aesthetics (in the categorical setting) or by `as(x, "numeric")` (in the time series setting), and labeled accordingly. (All categorical variables should be stored as factors anyway.) The present implementation makes this challenging, but an easy fix is to add a `scale_x_continuous()` layer (see `help(geom_alluvium)`).
- Should `geom_text(stat = "stratum")` be incorporated as a logical option in `geom_stratum()` that defaults to `TRUE`? Would this require writing a separate `geom_stratum_label()`?

### Miscellany

- It might make sense to allow the axis strata to overlap, rather than stack, under `position_identity`, but make `position_stack` the default that produces the traditional alluvial diagram, and make other `position_*`s throw errors. In the time series setting without strata, this would produce overlaid filled area plots.
