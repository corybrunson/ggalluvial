# load refugees data from alluvial
data(Refugees, package = "alluvial")

# basic time series alluvia
ggplot(data = Refugees,
       aes(x = year, weight = refugees, group = country)) +
  geom_alluvium_ts()

# time series alluvia with some aesthetics
ggplot(data = Refugees,
       aes(x = year, weight = refugees, group = country)) +
  geom_alluvium_ts(aes(fill = country), colour = "black")

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
       aes(x = year, weight = refugees, group = country)) +
  geom_alluvium_ts(aes(fill = country), colour = "black") +
  facet_wrap(~ region, scales = "fixed")
