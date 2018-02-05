# use of strata and labels
ggplot(as.data.frame(Titanic),
       aes(weight = Freq,
           axis1 = Class, axis2 = Sex, axis3 = Age)) +
  geom_flow() +
  scale_x_discrete(limits = c("Class", "Sex", "Age")) +
  geom_stratum() + geom_text(stat = "stratum", label.strata = TRUE) +
  ggtitle("Alluvial diagram of Titanic passenger demographic data")

# use of facets
ggplot(as.data.frame(Titanic),
       aes(weight = Freq,
           axis1 = Class, axis2 = Sex)) +
  geom_flow(aes(fill = Age)) +
  geom_stratum() + geom_text(stat = "stratum", label.strata = TRUE) +
  scale_x_discrete(limits = c("Class", "Sex")) +
  facet_wrap(~ Survived, scales = "fixed")

# time series alluvia of WorldPhones data
wph <- as.data.frame(as.table(WorldPhones))
names(wph) <- c("Year", "Region", "Telephones")
ggplot(wph,
       aes(x = Year, alluvium = Region, weight = Telephones)) +
  geom_flow(aes(fill = Region, colour = Region), width = 0)

# rightward flow aesthetics for vaccine survey data
data(vaccinations)
levels(vaccinations$response) <- rev(levels(vaccinations$response))
ggplot(vaccinations,
       aes(x = survey, stratum = response, alluvium = subject,
           weight = freq, fill = response, label = round(a, 3))) +
  geom_lode() + geom_flow() +
  geom_stratum(alpha = 0) +
  geom_text(stat = "stratum")
