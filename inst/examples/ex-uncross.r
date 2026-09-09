if (requireNamespace("wompwomp", quietly = TRUE)) {

data(vaccinations)
gg <- ggplot(vaccinations,
             aes(x = survey, stratum = response, alluvium = subject,
                 y = freq))

# Default (unsorted)
print(gg + geom_flow() + geom_stratum() +
        ggtitle("default stratum order"))

# sort strata with wompwomp (a heuristic that reduces crossings), global
old <- options(ggalluvial.sort_strata = "wompwomp")
print(gg + geom_flow() + geom_stratum() +
        ggtitle("strata sorted by wompwomp (package option)"))
options(old)

# sort strata with wompwomp (a heuristic that reduces crossings), individual
# arguments -- every layer needs the same value
print(gg +
        geom_flow(sort_strata = "wompwomp") +
        geom_stratum(sort_strata = "wompwomp") +
        ggtitle("strata sorted by wompwomp (layer parameters)"))

# sort strata with wompwomp (a heuristic that reduces crossings), individual
# arguments with additional customization (see wompwomp::sort_to_uncross() for
# details).
wompwomp_options <- list("wompwomp", alpha = 3)
print(gg +
        geom_flow(sort_strata = wompwomp_options) +
        geom_stratum(sort_strata = wompwomp_options) +
        ggtitle("strata sorted by wompwomp with custom options"))

# sort and color strata with wompwomp, individual arguments
print(gg +
        geom_flow(sort_strata = "wompwomp") +
        geom_stratum(aes(fill = after_stat(cluster)),
                     sort_strata = "wompwomp",
                     color_strata = "wompwomp") +
        ggtitle("strata colored by wompwomp cluster"))

# sort and color strata with wompwomp, individual arguments with additional
# customization (see wompwomp::get_lode_clusters() for details)
print(gg +
        geom_flow(sort_strata = "wompwomp") +
        geom_stratum(aes(fill = after_stat(cluster)),
                     sort_strata = "wompwomp",
                     color_strata = list("wompwomp", resolution = 2)) +
        ggtitle("strata colored by wompwomp cluster"))


# sort axes with wompwomp
set.seed(0)
axis_order <- sort_axes(vaccinations,
                        key = "survey", value = "response", id = "subject",
                        weight = "freq",
                        engine = "wompwomp")
print(gg +
        geom_flow(sort_strata = "wompwomp") +
        geom_stratum(sort_strata = "wompwomp") +
        scale_x_discrete(limits = axis_order) +
        ggtitle("axes and strata both sorted"))

}
