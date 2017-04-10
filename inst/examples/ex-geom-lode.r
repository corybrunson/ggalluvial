# one axis
ggplot(as.data.frame(Titanic),
       aes(weight = Freq,
           axis = Class)) +
  geom_lode(aes(fill = Class, alpha = Survived)) +
  scale_x_continuous(breaks = 1, labels = c("Class")) +
  scale_alpha_manual(values = c(.25, .75))

# alluvia and lodes
ggplot(as.data.frame(Titanic),
       aes(weight = Freq,
           axis1 = Class, axis2 = Sex, axis3 = Age,
           fill = Survived)) +
  geom_alluvium() + geom_lode()

# lodes as strata
ggplot(as.data.frame(Titanic),
       aes(weight = Freq,
           axis1 = Class, axis2 = Sex, axis3 = Age,
           fill = Survived)) +
  geom_alluvium() +
  geom_stratum(stat = "alluvium")
