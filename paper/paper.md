Summary
=======

Alluvial diagrams use stacked bar plots and variable-width ribbons to
represent multi-dimensional or repeated-measures data comprising
categorical or ordinal variables (Rosvall and Bergstrom 2010; Bojanowski
and Edwards 2016). The ggalluvial package extends the layered grammar of
graphics of ggplot2 (Wickham 2016) to generate alluvial diagrams from
tidy data (Wickham 2014). The package makes two key contributions to the
R ecosystem, which i summarize here and discuss in more detail below.

First, ggalluvial anchors the imprecise notion of an alluvial diagram to
the rigid grammar of graphics (Wilkinson 2006), which imbues the
resulting plots with greater meaning, most notably a cumulative axis,
and imparts many combinatorial possibilities, e.g. with the text and
point–line geoms of ggplot2. Meanwhile, the package gives users a high
degree of control over underlying data transformations, and thereby over
the messages conveyed in the plot, whereas other extensions are not so
flexible.[1] The package also integrates alluvial plots to a tidyverse
workflow (Wickham et al. 2019) by recognizing two common tidy data
formats and exporting data transformation functions that follow tidyr
and dplyr conventions.

Second, ggalluvial cements some concepts and terms that are
inconsistently applied by developers and users of Sankey, parallel sets,
and alluvial diagrams. For example, the diagrams that came to be known
as parallel sets plots (Kosara, Bendix, and Hauser 2006) were
anticipated in the “start-up state” of an interactive generator of
Sankey diagrams (Riehmann, Hanfler, and Froehlich 2005), using wholly
different terms for their graphical elements. Since alluvial diagrams
were introduced, implementations and applications have borrowed
terminology from both previous diagram types. Instead, ggalluvial adopts
a distinctive, geological nomenclature for its graphical elements. While
this risks exacerbating confusion, the “alluvial plots” described here
are, i think, distinctive enough from Sankey diagrams and parallel sets
plots to warrant it. I hope that these conventions can serve as useful
points of reference as these visualization tools converge toward common
standards.

Functionality
-------------

[The titular
vignette](http://corybrunson.github.io/ggalluvial/articles/ggalluvial.html)
thoroughly describes and illustrates the functionality of ggalluvial,
and the reader is encouraged to browse [the package
documentation](http://corybrunson.github.io/ggalluvial/reference/index.html)
for more thorough illustrations. In brief, the package contains stat and
geom functions to add the following layers to a ggplot2 object:

-   *strata*, or stacked bar plots, located at each of multiple
    horizontal positions (*axes* or *dimensions*)
-   *alluvia*, ribbons through strata at different axes that encode
    individual cases or cohorts measured along each axis
-   *lodes*, subdivisions of strata by their intersections with alluvia
-   *flows*, segments of alluvia between adjacent axes (excluding lodes)

The layers of a ggplot2 graphic are formed by pairing stats (statistical
transformations of the input data) with geoms (geometric mappings from
the transformed data); while every stat and geom has a conventional
default, alternative pairings are the primary source of combinatorial
richness for this layered grammar. The following alluvial plot depicts
several meaningful stat–geom combinations that appear in the
documentation. Default pairings, other within-package (“alluvial”)
pairings, and pairings of alluvial stats with other geoms are
differentiated by `fill` color:

![](paper/figures/figure-1.png)

The alluvial stats require custom aesthetics—either `stratum` and/or
`alluvium` in combination with `x`, if the data are in long format, or
some number of axis specifications (`axis1`, `axis2`, etc.), if the data
are in wide format. Because these aesthetics are not recognized by
ggplot2, they produce warnings under some conditions. The long (one row
per lode) and wide (one row per alluvium) formats are also detailed in
the vignette, and are related to each other by the pivot operations of
tidyr. Because the alluvial geoms are highly specialized to these stats,
no pairings with outside stats are currently supported.

Most of the stat parameters control how the strata at each axis, and the
lodes within each stratum, are ordered vertically. By default, these
orderings are independent of differentiation aesthetics, so that layers
are consistent within and across plots unless otherwise specified. [An
auxiliary
vignette](http://corybrunson.github.io/ggalluvial/articles/order-rectangles.html)
details the effects of each of these parameters. For convenience, they
can also be set as global options.

While questions and requests by users have prompted improvements
throughout ggalluvial’s development, the current version is expected to
be feature complete, with one exception: alternative curves will be
implemented for the `alluvium` and `flow` geoms, controlled by a new
parameter. The current splines will remain the default, so that the
output of existing code will not change. This is expected to be the last
change before version `1.0`.

Concepts
--------

Statistical graphics of multivariate categorical data tend to take one
of two forms: ordinations of continuous-valued transformations
(e.g. residual plots from multiple regression; ternary plots from
tripartition data; biplots from correspondence analysis and factor
analysis) and arrangements of length- or area-encoded shapes
(e.g. mosaic plots (Friendly 2002); product plots (Wickham and Hofmann
2011); stacked area charts). In particular, parallel sets plots, adapted
from parallel coordinates plots (Inselberg and Dimsdale 1987; Wegman
1990), represent cohorts of equivalent cases as ribbons connecting
categories represented as boxes (Kosara, Bendix, and Hauser 2006).

Meanwhile, visualizations of flow processes have long encoded
magnitudes, such as volumes and counts, as ribbon widths, and this broad
genre have come to be called Sankey diagrams (Schmidt 2008); see
<a href="http://www.sankey-diagrams.com/" class="uri">http://www.sankey-diagrams.com/</a>
for many examples. A widely-used and -implemented subtype of Sankey
diagram encodes longitudinal categorical data as rectangular nodes,
representing categories, threaded by ribbons that encode the
trajectories and magnitudes of cases (Riehmann, Hanfler, and Froehlich
2005).

Most recently, Rosvall and Bergstrom (2010) proposed “alluvial diagrams”
to visualize changes in cluster membership among cases over successive
cross-sections, with cohorts of equivalent cases encoded as ribbons
intersecting stacked bar plots at each time point. These diagrams have
since been used to represent a wide range of discrete-valued data, over
repeated measures or multiple dimensions.

Several R packages, including ggplot2 extensions, have been developed to
generate these types of data visualizations. Here are the most relevant
for this discussion:

-   alluvial for alluvial diagrams in base R (Bojanowski and
    Edwards 2016)
-   ggforce for parallel sets plots in ggplot2 (Pedersen 2019)
-   ggparallel and ggpcp for parallel sets, hammock, and common angle
    plots in ggplot2 (Hofmann and Vendettuoli 2013; Ge and Hofmann 2019)
-   ggalluvial for alluvial diagrams in ggplot2 (Brunson 2019)

While Sankey diagrams, parallel sets plots, and alluvial diagrams have
accrued their own characteristic use cases, the terms are often used
interchangeably, and there is currently no consensus on what features,
if any, are distinctive to each type. Moreover, these visualization
techniques in practice vary widely in their flexibility or rigidity—some
rely on non-deterministic layout algorithms and can even be
interactively manipulated, while others are wholly determined by the
data; most give the user limited control through parameter settings.

The diagrams produced by ggalluvial are statistical graphics in the
sense that they communicate statistical information using graphical
methods (Friendly 2005) and, more narrowly, are uniquely determined from
data by a fixed set of plotting rules (Wilkinson 2006). The package
adopts for them the less common term *alluvial plot*.[2] These plots are
distinct from Sankey diagrams (they exhibit a much narrower range of
behavior, e.g. no cycles, and do not necessarily represent flow) and
constitute a subtype of parallel sets plots distinguished by two
features: a strict order on the stacked elements at each axis, including
both the values of the discrete variables and the ribbons connecting
cases or cohorts between them; and a meaningful positional dimension
along which these elements are stacked, which precludes inserting gaps
between them (as is typical of most diagrams of each type), features
shared only by ggparallel. The plot elements (strata, either alluvia or
flows, and optionally lodes or others as discussed above) are rendered a
separate layers, following the additive (`+`) syntax of ggplot2, a
feature shared only by ggpcp.

Applications
------------

ggalluvial has seen widespread use in research and scholarship. While
many of these uses would be served equally well by other parallel sets
plots or Sankey diagrams, i showcase here three settings to which
alluvial plots seem exceptionally well-suited: repeated ordinal measures
data, incomplete longitudinal data, and signed categorical data. To be
sure, this is a subjective assessment that may be refuted by
visualization effectiveness research.

**Repeated ordinal measures data:** Parallel sets plots and Sankey
diagrams are commonly used to represent repeated categorical measures
data. Most implementations stack each bar plot in order of name or of
size, though some follow hierarchies provided by the user, often with
vertical space between category for easy visual discrimination. In
contrast, ordinal variables are more appropriately stacked in their own
intrinsic and consistent order, and, when the number of categories
changes from time to time, vertical separations can obscure whether the
cases and cohorts changed in number as well. A use case by Schlotter et
al. (2019), to represent changes (or not) in patients’ physical
limitations following an investigational right heart valve repair
technique, illustrates the use of an ordinal stratum variable (a heart
failure functional classification). Another, by North et al. (2019), to
represent ranked preferences among several definitions of veganism by
survey respondents, illustrates the importance of consistency in order
across axes (in this case preference ranks rather than time points).

**Incomplete longitudinal data:** Alluvial plots very clearly delineate
times at which longitudinal data are censored, discontinued, or
otherwise missing: Certain strata, or the alluvia or flows connecting
them, are present at one time point but absent at a previous or future
one. Seekatz et al. (2018) use this feature to include in one alluvial
plot a sample of *Clostridium difficile*–infected (CDI) patients who had
their infections ribotyped at multiple times. Patients were classified
by dominant ribotype, and the alluvial plot showcased variability in
this dominant type. While all 32 patients had at least two samples
taken, only 3 had four, so that the bar plots shortened with each time
point. Sjoding et al. (2019) use a similar plot to trace patient groups
receiving mechanical ventilation based on discretized tidal volumes,
including a grey stratum for patients discontinued from intubation
rather than omitting them entirely from the plot. <!--
Even in cases of intermediate missingness, when a case is lost, or its value diminished to zero, between two time points at which it is positively measured, alluvial diagrams can preserve the continuity of such cases by shrinking their ribbon widths to zero at the intermediate point. (See the example in the `stat_alluvium()` examples using the babynames package.)
-->

**Signed categorical data:** Edwards and Pinkerton (2019) recently
produced a novel alluvial plot to represent changes in ownership
category (owner-operated, corporate, etc.) of owners in a halibut
fishery. Not only the categorical distribution but the total number of
owners changed from year to year as exeters were not exactly matched by
new entrants. In order to depict an accurate total but include both new
entrants and exeters at each year, the authors affixed a negative
stratum for the exeter category to each bar plot, below the horizontal
axis. Such a feature has no analogue in Sankey diagrams or parallel sets
plots (which do not commonly employ a cumulative axis) but potentially
wide-ranging applications: Bar plots may include both “positive” and
“negative” bars to represent definitionally negative groups, such as
revenues versus deficits, or to contrast the bars divided along a binary
variable such as gender across age groups in a population (so-called
“pyramid plots”). Alluvial plots provide a way to track cases and
cohorts across such bar plots, even when cases may change sign. Future
use cases may demonstrate the practical utility of this functionality.

Acknowledgments
===============

I am grateful to many users for their feedback on every version of this
package. Development benefitted from the use of resources and support of
colleagues at UConn Health, and i have been supported in part by T90
training grant from the National Institute of Dental and Craniofacial
Research (5T90DE021989-07).

References
==========

Bojanowski, Michał, and Robin Edwards. 2016. “alluvial: R Package for
Creating Alluvial Diagrams.”
<https://cran.r-project.org/package=alluvial>.

Brunson, Jason Cory. 2019. “ggalluvial: Alluvial Plots in ’ggplot2’.”
<https://cran.r-project.org/package=ggalluvial>.

Edwards, Danielle N., and Evelyn Pinkerton. 2019. “Rise of the investor
class in the British Columbia Pacific halibut fishery.” *Marine Policy*
109 (November). <https://doi.org/10.1016/j.marpol.2019.103676>.

Friendly, Michael. 2002. “A Brief History of the Mosaic Display.”
<https://doi.org/10.1198/106186002317375631>.

———. 2005. “Milestones in the History of Data Visualization: A Case
Study in Statistical Historiography.” In *Classification — the
Ubiquitous Challenge. Studies in Classification, Data Analysis, and
Knowledge Organization*, 34–52. Berlin/Heidelberg: Springer-Verlag.
<https://doi.org/10.1007/3-540-28084-7_4>.

Ge, Yawei, and Heike Hofmann. 2019. “ggpcp: Parallel Coordinate Plots in
the ggplot2 Framework.” <https://yaweige.github.io/ggpcp/>.

Hofmann, Heike, and Marie Vendettuoli. 2013. “Common angle plots as
perception-true visualizations of categorical associations.” *IEEE
Transactions on Visualization and Computer Graphics* 19 (12): 2297–2305.
<https://doi.org/10.1109/TVCG.2013.140>.

Inselberg, Alfred, and Bernard Dimsdale. 1987. “Parallel Coordinates for
Visualizing Multi-Dimensional Geometry.” In *Computer Graphics 1987*,
25–44. Tokyo: Springer Japan.
<https://doi.org/10.1007/978-4-431-68057-4_3>.

Koneswarakantha, Bjoern. 2019. “easyalluvial: Generate Alluvial Plots
with a Single Line of Code.”
<https://cran.r-project.org/package=easyalluvial>.

Kosara, Robert, Fabian Bendix, and Helwig Hauser. 2006. “Parallel sets:
Interactive exploration and visual analysis of categorical data.” In
*IEEE Transactions on Visualization and Computer Graphics*, 12:558–68.
4. <https://doi.org/10.1109/TVCG.2006.76>.

North, Madelon, Emily Kothe, Anna Klas, and Mathew Ling. 2019. “‘It’s
not just a diet, it’s a lifestyle’: An Exploratory Study into Community
Preferences of Vegan Definitions.” PsyArXiv.
<https://doi.org/10.31234/OSF.IO/MKQN4>.

Pedersen, Thomas Lin. 2019. “ggforce: Accelerating ’ggplot2’.”
<https://cran.r-project.org/package=ggforce>.

Riehmann, P., M. Hanfler, and B. Froehlich. 2005. “Interactive Sankey
diagrams.” In *IEEE Symposium on Information Visualization, 2005.
INFOVIS 2005.*, 233–40. IEEE.
<https://doi.org/10.1109/INFVIS.2005.1532152>.

Rosvall, Martin, and Carl T. Bergstrom. 2010. “Mapping Change in Large
Networks.” Edited by Fabio Rapallo. *PLoS ONE* 5 (1): e8694.
<https://doi.org/10.1371/journal.pone.0008694>.

Schlotter, Florian, Mathias Orban, Karl‐Philipp Rommel, Christian
Besler, Maximilian Roeder, Daniel Braun, Matthias Unterhuber, et al.
2019. “Aetiology‐based clinical scenarios predict outcomes of
transcatheter edge‐to‐edge tricuspid valve repair of functional
tricuspid regurgitation.” *European Journal of Heart Failure* 21 (9):
1117–25. <https://doi.org/10.1002/ejhf.1547>.

Schmidt, Mario. 2008. “The Sankey Diagram in Energy and Material Flow
Management.” *Journal of Industrial Ecology* 12 (1): 82–94.
<https://doi.org/10.1111/j.1530-9290.2008.00004.x>.

Seekatz, Anna M., Emily Wolfrum, Christopher M. DeWald, Rosemary K. B.
Putler, Kimberly C. Vendrov, Krishna Rao, and Vincent B. Young. 2018.
“Presence of multiple Clostridium difficile strains at primary infection
is associated with development of recurrent disease.” *Anaerobe* 53
(October): 74–81. <https://doi.org/10.1016/j.anaerobe.2018.05.017>.

Sjoding, Michael W., Michelle N. Gong, Carl F. Haas, and Theodore J.
Iwashyna. 2019. “Evaluating Delivery of Low Tidal Volume Ventilation in
Six ICUs Using Electronic Health Record Data.” *Critical Care Medicine*
47 (1): 56–61. <https://doi.org/10.1097/CCM.0000000000003469>.

Wegman, Edward J. 1990. “Hyperdimensional Data Analysis Using Parallel
Coordinates.” *Journal of the American Statistical Association* 85
(411): 664–75. <https://doi.org/10.1080/01621459.1990.10474926>.

Wickham, Hadley. 2014. “Tidy Data.” *Journal of Statistical Software* 59
(10): 1–23. <https://doi.org/10.18637/jss.v059.i10>.

———. 2016. *ggplot2: Elegant Graphics for Data Analysis*. New York:
Springer-Verlag.
<https://ggplot2.tidyverse.org%20https://link.springer.com/book/10.1007/978-0-387-98141-3>.

Wickham, Hadley, Mara Averick, Jennifer Bryan, Winston Chang, Lucy
McGowan D’Agostino, Romain François, Garrett Grolemund, et al. 2019.
“Welcome to the Tidyverse.” *Journal of Open Source Software* 4 (43):
1686. <https://doi.org/10.21105/joss.01686>.

Wickham, Hadley, and Heike Hofmann. 2011. “Product plots.” *IEEE
Transactions on Visualization and Computer Graphics* 17 (12): 2223–30.
<https://doi.org/10.1109/TVCG.2011.227>.

Wilkinson, Leland. 2006. *The Grammar of Graphics*. 2nd ed. Springer
Science & Business Media.
<https://books.google.com/books?id=NRyGnjeNKJIC>.

[1] Indeed, the dependency package easyalluvial (Koneswarakantha 2019)
was built on top of ggalluvial to exchange much of this flexibility for
expedited data exploration using alluvial plots.

[2] This has the unfortunate side effect of conflating search results
from the geology literature.
