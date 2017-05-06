data(vaccinations)
# lode data: positioning with weight labels
ggplot(vaccinations,
       aes(weight = freq,
           x = survey, stratum = response, alluvium = subject,
           label = freq)) +
  geom_stratum() +
  geom_text(stat = "stratum")
# lode data: positioning with stratum labels
ggplot(vaccinations,
       aes(weight = freq,
           x = survey, stratum = response, alluvium = subject,
           label = response)) +
  geom_stratum() +
  geom_text(stat = "stratum")

# alluvium data: positioning with weight labels
ggplot(as.data.frame(Titanic),
       aes(weight = Freq,
           axis1 = Class, axis2 = Sex, axis3 = Age, axis4 = Survived,
           label = Freq)) +
  geom_text(stat = "stratum") +
  geom_errorbar(stat = "stratum") +
  scale_x_continuous(breaks = 1:4,
                     labels = c("Class", "Sex", "Age", "Survived"))
# alluvium data: positioning with stratum labels
ggplot(as.data.frame(Titanic),
       aes(weight = Freq,
           axis1 = Class, axis2 = Sex, axis3 = Age, axis4 = Survived)) +
  geom_text(stat = "stratum", label.strata = TRUE) +
  geom_errorbar(stat = "stratum") +
  scale_x_continuous(breaks = 1:4,
                     labels = c("Class", "Sex", "Age", "Survived"))
