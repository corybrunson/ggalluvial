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
           fill = country, colour = country)) +
  geom_alluvium(width = 1/4, alpha = 2/3, decreasing = FALSE)

# rightward flow aesthetics for vaccine survey data
data(nsa)
ggplot(nsa,
       aes(x = survey, stratum = response, alluvium = subject,
           weight = freq, fill = response, label = round(a, 3))) +
  geom_lode() + geom_flow(lode.guidance = "rightward") +
  geom_stratum(alpha = 0) +
  geom_text(stat = "stratum")
