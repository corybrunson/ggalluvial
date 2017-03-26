# time series alluvia of WorldPhones data
wph <- as.data.frame(as.table(WorldPhones))
names(wph) <- c("Year", "Region", "Telephones")
ggplot(wph,
       aes(x = Year, group = Region, weight = Telephones)) +
  geom_alluvium_ts(aes(fill = Region, colour = Region))

# time series line plot of refugees data, sorted by country
ggplot(data = alluvial::Refugees,
       aes(x = year, weight = refugees, group = country)) +
  geom_alluvium_ts(aes(fill = country), colour = "black",
                   decreasing = NA, knot.pos = 0)

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
       aes(x = year, weight = refugees, group = country)) +
  geom_alluvium_ts(aes(fill = country), colour = "black") +
  facet_wrap(~ region, scales = "fixed")
