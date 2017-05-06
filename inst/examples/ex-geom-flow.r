# use of strata and labels
ggplot(as.data.frame(Titanic),
       aes(weight = Freq,
           axis1 = Class, axis2 = Sex, axis3 = Age)) +
  geom_flow() +
  scale_x_continuous(breaks = 1:3, labels = c("Class", "Sex", "Age")) +
  geom_stratum() + geom_text(stat = "stratum", label.strata = TRUE) +
  ggtitle("Alluvial diagram of Titanic passenger demographic data")

# use of facets
ggplot(as.data.frame(Titanic),
       aes(weight = Freq,
           axis1 = Class, axis2 = Sex)) +
  geom_flow(aes(fill = Age)) +
  geom_stratum() + geom_text(stat = "stratum", label.strata = TRUE) +
  scale_x_continuous(breaks = 1:2, labels = c("Class", "Sex")) +
  facet_wrap(~ Survived, scales = "fixed")

# time series alluvia of WorldPhones data
wph <- as.data.frame(as.table(WorldPhones))
names(wph) <- c("Year", "Region", "Telephones")
ggplot(wph,
       aes(x = Year, stratum = Region, weight = Telephones)) +
  geom_flow(aes(fill = Region, colour = Region), width = 0)
