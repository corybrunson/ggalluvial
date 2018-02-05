# basic
ggplot(as.data.frame(Titanic),
       aes(weight = Freq,
           axis1 = Class, axis2 = Sex, axis3 = Age,
           fill = Survived)) +
  geom_alluvium() +
  scale_x_discrete(limits = c("Class", "Sex", "Age"))

gg <- ggplot(alluvial::Refugees,
             aes(weight = refugees, x = year, alluvium = country))
# time series bump chart
gg + geom_alluvium(aes(fill = country, colour = country),
                   width = 1/4, alpha = 2/3, decreasing = FALSE)
# time series line plot of refugees data, sorted by country
gg + geom_alluvium(aes(fill = country, colour = country),
                   decreasing = NA, width = 0, knot.pos = 0)
