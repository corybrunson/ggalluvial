# use of strata and labels
ggplot(as.data.frame(Titanic),
       aes(y = Freq,
           axis1 = Class, axis2 = Sex, axis3 = Age)) +
  geom_flow() +
  scale_x_discrete(limits = c("Class", "Sex", "Age")) +
  geom_stratum() + geom_text(stat = "stratum", infer.label = TRUE) +
  ggtitle("Alluvial plot of Titanic passenger demographic data")

# use of facets, with sigmoid flows
ggplot(as.data.frame(Titanic),
       aes(y = Freq,
           axis1 = Class, axis2 = Sex)) +
  geom_flow(aes(fill = Age), width = .4, curve = "sigmoid") +
  geom_stratum(width = .4) +
  geom_text(stat = "stratum", infer.label = TRUE, size = 3) +
  scale_x_discrete(limits = c("Class", "Sex")) +
  facet_wrap(~ Survived, scales = "fixed")

# time series alluvia of WorldPhones data
wph <- as.data.frame(as.table(WorldPhones))
names(wph) <- c("Year", "Region", "Telephones")
ggplot(wph,
       aes(x = Year, alluvium = Region, y = Telephones)) +
  geom_flow(aes(fill = Region, colour = Region), width = 0)
# treat 'Year' as a number rather than as a factor
wph$Year <- as.integer(as.character(wph$Year))
ggplot(wph,
       aes(x = Year, alluvium = Region, y = Telephones)) +
  geom_flow(aes(fill = Region, colour = Region), width = 0)
# hold the knot positions fixed
ggplot(wph,
       aes(x = Year, alluvium = Region, y = Telephones)) +
  geom_flow(aes(fill = Region, colour = Region), width = 0, knot.fix = TRUE)

# rightward flow aesthetics for vaccine survey data, with cubic flows
data(vaccinations)
levels(vaccinations$response) <- rev(levels(vaccinations$response))
ggplot(vaccinations,
       aes(x = survey, stratum = response, alluvium = subject,
           y = freq, fill = response, label = round(a, 3))) +
  geom_lode() + geom_flow(curve = "cubic") +
  geom_stratum(alpha = 0) +
  geom_text(stat = "stratum")
