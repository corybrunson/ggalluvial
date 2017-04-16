# illustrate positioning
ggplot(as.data.frame(Titanic),
       aes(weight = Freq,
           axis1 = Class, axis2 = Sex, axis3 = Age, axis4 = Survived)) +
  geom_text(stat = "stratum") +
  geom_errorbar(stat = "stratum") +
  scale_x_continuous(breaks = 1:4,
                     labels = c("Class", "Sex", "Age", "Survived"))
