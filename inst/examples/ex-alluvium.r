# illustrate positioning
ggplot(as.data.frame(Titanic),
       aes(weight = Freq,
           axis1 = Class, axis2 = Sex, axis3 = Age,
           color = Survived)) +
  geom_errorbar(stat = "stratum") +
  geom_line(stat = "alluvium") +
  geom_pointrange(stat = "alluvium") +
  geom_text(stat = "stratum") +
  scale_x_continuous(breaks = 1:3,
                     labels = c("Class", "Sex", "Age"))

# declaration of groups (ignored)
ggplot(as.data.frame(Titanic),
       aes(weight = Freq,
           axis1 = Class, axis2 = Sex, axis3 = Age,
           group = Survived)) +
  geom_alluvium() +
  scale_x_continuous(breaks = 1:3, labels = c("Class", "Sex", "Age"))
