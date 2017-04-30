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
