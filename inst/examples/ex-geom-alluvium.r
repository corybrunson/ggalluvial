# basic
ggplot(as.data.frame(Titanic),
       aes(weight = Freq,
           axis1 = Class, axis2 = Sex, axis3 = Age,
           fill = Survived)) +
  geom_alluvium() +
  scale_x_continuous(breaks = 1:3, labels = c("Class", "Sex", "Age"))

# declaration of groups (ignored)
ggplot(as.data.frame(Titanic),
       aes(weight = Freq,
           axis1 = Class, axis2 = Sex, axis3 = Age,
           group = Survived)) +
  geom_alluvium() +
  scale_x_continuous(breaks = 1:3, labels = c("Class", "Sex", "Age"))

# time series bump chart
ggplot(alluvial::Refugees,
       aes(weight = refugees,
           x = year, stratum = country,
           fill = country)) +
  geom_alluvium(width = 1/4, alpha = 2/3, decreasing = FALSE)

# load refugees data from alluvial
data(Refugees, package = "alluvial")
# time series alluvia faceted by region
country_regions <- c(
  Afghanistan = "Middle East",
  Burundi = "Central Africa",
  `Congo DRC` = "Central Africa",
  Iraq = "Middle East",
  Myanmar = "Southeast Asia",
  Palestine = "Middle East",
  Somalia = "Horn of Africa",
  Sudan = "Central Africa",
  Syria = "Middle East",
  Vietnam = "Southeast Asia"
)
Refugees$region <- country_regions[Refugees$country]
ggplot(data = Refugees,
       aes(x = year, weight = refugees, stratum = country)) +
  geom_alluvium(aes(fill = country), decreasing = FALSE) +
  facet_wrap(~ region, scales = "fixed")
