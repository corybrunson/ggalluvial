# illustrate positioning
ggplot(as.data.frame(Titanic),
       aes(weight = Freq,
           axis1 = Class, axis2 = Sex, axis3 = Age,
           color = Survived)) +
  geom_errorbar(stat = "stratum") +
  geom_line(stat = "flow") +
  geom_pointrange(stat = "flow") +
  geom_text(stat = "stratum") +
  scale_x_continuous(breaks = 1:3,
                     labels = c("Class", "Sex", "Age"))

# use of lode controls
ggplot(as.data.frame(Titanic),
       aes(weight = Freq,
           axis1 = Class, axis2 = Sex, axis3 = Age)) +
  geom_flow(aes(fill = Survived), aes.bind = TRUE) +
  geom_stratum() + geom_text(stat = "stratum") +
  scale_x_continuous(breaks = 1:3, labels = c("Class", "Sex", "Age"))

data(vaccinations)
# rightward alluvial aesthetics for vaccine survey data
ggplot(vaccinations,
       aes(x = survey, stratum = response, alluvium = subject,
           weight = freq, fill = response)) +
  geom_flow(stat = "alluvium", lode.guidance = "rightward") +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum")
# memoryless flows for vaccine survey data
ggplot(vaccinations,
       aes(x = survey, stratum = response, alluvium = subject,
           weight = freq, fill = response)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum")

# both partition aesthetics and interpolation aesthetics
data(vaccinations)
vaccinations$subgroup <- LETTERS[1:2][rbinom(
  n = length(unique(vaccinations$subject)), size = 1, prob = .5
) + 1][vaccinations$subject]
ggplot(vaccinations,
       aes(x = survey, stratum = response, alluvium = subject,
           weight = freq, fill = response)) +
  geom_flow(aes(alpha = subgroup)) +
  scale_alpha_discrete(range = c(1/3, 2/3)) +
  geom_stratum(alpha = .5) +
  #geom_text(stat = "stratum")
  geom_label(stat = "stratum")
