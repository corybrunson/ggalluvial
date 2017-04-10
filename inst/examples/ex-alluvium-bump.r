
# pass lode aesthetics forward along flows
ggplot(titanic_lodes,
       aes(weight = Freq,
           x = Variable, stratum = Value, alluvium = Index,
           fill = Value)) +
  geom_alluvium(aes.flow = "forward") +
  geom_stratum() + geom_text(stat = "stratum")

# time series bump chart
ggplot(alluvial::Refugees,
       aes(weight = refugees,
           x = year, stratum = country, alluvium = country,
           fill = country)) +
  geom_alluvium() + geom_lode()
