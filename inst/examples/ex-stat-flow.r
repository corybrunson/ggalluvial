# illustrate positioning
ggplot(as.data.frame(Titanic),
       aes(y = Freq,
           axis1 = Class, axis2 = Sex, axis3 = Age,
           color = Survived)) +
  stat_stratum(geom = "errorbar") +
  geom_line(stat = "flow") +
  stat_flow(geom = "pointrange") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Class", "Sex", "Age"))

# alluvium--flow comparison
data(vaccinations)
gg <- ggplot(vaccinations,
             aes(x = survey, stratum = response, alluvium = subject,
                 y = freq, fill = response)) +
  geom_stratum(alpha = .5) +
  geom_text(aes(label = response), stat = "stratum")
# rightward alluvial aesthetics for vaccine survey data
gg + geom_flow(stat = "alluvium", lode.guidance = "forward")
# memoryless flows for vaccine survey data
gg + geom_flow()

# size filter examples
gg <- ggplot(vaccinations,
       aes(y = freq,
           x = survey, stratum = response, alluvium = subject,
           fill = response, label = response)) +
  stat_stratum(alpha = .5) +
  geom_text(stat = "stratum")
# omit small flows
gg + geom_flow(min.y = 50)
# omit large flows
gg + geom_flow(max.y = 100)

# negate missing entries
ggplot(vaccinations,
       aes(y = freq,
           x = survey, stratum = response, alluvium = subject,
           fill = response, label = response,
           alpha = response != "Missing")) +
  stat_stratum(negate.strata = "Missing") +
  geom_flow(negate.strata = "Missing") +
  geom_text(stat = "stratum", alpha = 1, negate.strata = "Missing") +
  scale_alpha_discrete(range = c(.2, .6)) +
  guides(alpha = "none")

\donttest{
# aesthetics that vary betwween and within strata
data(vaccinations)
vaccinations$subgroup <- LETTERS[1:2][rbinom(
  n = length(unique(vaccinations$subject)), size = 1, prob = .5
) + 1][vaccinations$subject]
ggplot(vaccinations,
       aes(x = survey, stratum = response, alluvium = subject,
           y = freq, fill = response, label = response)) +
  geom_flow(aes(alpha = subgroup)) +
  scale_alpha_discrete(range = c(1/3, 2/3)) +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum")
# can even set aesthetics that vary both ways
ggplot(vaccinations,
       aes(x = survey, stratum = response, alluvium = subject,
           y = freq, label = response)) +
  geom_flow(aes(fill = interaction(response, subgroup)), aes.bind = "flows") +
  scale_alpha_discrete(range = c(1/3, 2/3)) +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum")
}
