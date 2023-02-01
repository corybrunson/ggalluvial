# basic
ggplot(as.data.frame(Titanic),
       aes(y = Freq,
           axis1 = Class, axis2 = Sex, axis3 = Age,
           fill = Survived)) +
  geom_alluvium() +
  scale_x_discrete(limits = c("Class", "Sex", "Age"))

gg <- ggplot(alluvial::Refugees,
             aes(y = refugees, x = year, alluvium = country))
# time series bump chart (quintic flows)
gg + geom_alluvium(aes(fill = country, colour = country),
                   width = 1/4, alpha = 2/3, decreasing = FALSE,
                   curve_type = "sigmoid")
# time series line plot of refugees data, sorted by country
gg + geom_alluvium(aes(fill = country, colour = country),
                   decreasing = NA, width = 0, knot.pos = 0)

\donttest{
# irregular spacing between axes of a continuous variable
refugees_sub <- subset(alluvial::Refugees, year %in% c(2003, 2005, 2010, 2013))
gg <- ggplot(data = refugees_sub,
             aes(x = year, y = refugees, alluvium = country)) +
  theme_bw() +
  scale_fill_brewer(type = "qual", palette = "Set3")
# proportional knot positioning (default)
gg +
  geom_alluvium(aes(fill = country),
                alpha = .75, decreasing = FALSE, width = 1/2) +
  geom_stratum(aes(stratum = country), decreasing = FALSE, width = 1/2)
# constant knot positioning
gg +
  geom_alluvium(aes(fill = country),
                alpha = .75, decreasing = FALSE, width = 1/2,
                knot.pos = 1, knot.prop = FALSE) +
  geom_stratum(aes(stratum = country), decreasing = FALSE, width = 1/2)
# coarsely-segmented curves
gg +
  geom_alluvium(aes(fill = country),
                alpha = .75, decreasing = FALSE, width = 1/2,
                curve_type = "arctan", segments = 6) +
  geom_stratum(aes(stratum = country), decreasing = FALSE, width = 1/2)
# custom-ranged curves
gg +
  geom_alluvium(aes(fill = country),
                alpha = .75, decreasing = FALSE, width = 1/2,
                curve_type = "arctan", curve_range = 1) +
  geom_stratum(aes(stratum = country), decreasing = FALSE, width = 1/2)
}
