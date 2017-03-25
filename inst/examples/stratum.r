# full axis width
ggplot(as.data.frame(Titanic),
       aes(weight = Freq,
           axis1 = Class, axis2 = Sex, axis3 = Age, axis4 = Survived)) +
  geom_stratum(width = 1) + geom_text(stat = "stratum")

# use of facets
ggplot(as.data.frame(Titanic),
       aes(weight = Freq,
           axis1 = Class, axis2 = Sex,
           fill = Survived)) +
  geom_alluvium() +
  geom_stratum() + geom_text(stat = "stratum") +
  scale_x_continuous(breaks = 1:2, labels = c("Class", "Sex")) +
  facet_wrap(~ Age, scales = "free_y")
