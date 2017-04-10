# time series alluvia of WorldPhones data
wph <- as.data.frame(as.table(WorldPhones))
names(wph) <- c("Year", "Region", "Telephones")
ggplot(wph,
       aes(x = Year, stratum = Region, weight = Telephones)) +
  geom_flow(aes(fill = Region, colour = Region), width = 0)

# time series line plot of refugees data, sorted by country
ggplot(data = alluvial::Refugees,
       aes(x = year, weight = refugees, stratum = country)) +
  geom_flow(aes(fill = country),
            colour = "black", decreasing = NA, width = 0, knot.pos = 0)

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
