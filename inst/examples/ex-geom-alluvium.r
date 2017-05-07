# basic
ggplot(as.data.frame(Titanic),
       aes(weight = Freq,
           axis1 = Class, axis2 = Sex, axis3 = Age,
           fill = Survived)) +
  geom_alluvium() +
  scale_x_continuous(breaks = 1:3, labels = c("Class", "Sex", "Age"))

# time series bump chart
ggplot(alluvial::Refugees,
       aes(weight = refugees,
           x = year, alluvium = country,
           fill = country, colour = country)) +
  geom_alluvium(width = 1/4, alpha = 2/3, decreasing = FALSE)

# time series line plot of refugees data, sorted by country
ggplot(data = alluvial::Refugees,
       aes(x = year, weight = refugees, alluvium = country)) +
  geom_alluvium(aes(fill = country),
                colour = "black", decreasing = NA, width = 0, knot.pos = 0)
