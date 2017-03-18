# basic boxes (strata) with labels
ggplot(as.data.frame(Titanic),
       aes(weight = Freq, axis1 = Class, axis2 = Sex, axis3 = Age)) +
  geom_stratum() + geom_text(stat = "stratum")

# full axis width
ggplot(as.data.frame(Titanic),
       aes(weight = Freq,
           axis1 = Class, axis2 = Sex, axis3 = Age, axis4 = Survived)) +
  geom_stratum(axis_width = 1) + geom_text(stat = "stratum")

# declaration of groups (unused)
ggplot(as.data.frame(Titanic),
       aes(weight = Freq,
           axis1 = Class, axis2 = Sex, axis3 = Age, group = Survived)) +
  geom_stratum() + geom_text(stat = "stratum")

# use of facets
ggplot(as.data.frame(Titanic),
       aes(weight = Freq,
           axis1 = Class, axis2 = Sex)) +
  geom_alluvium(aes(fill = Age)) +
  geom_stratum() + geom_text(stat = "stratum") +
  scale_x_continuous(breaks = 1:2, labels = c("Class", "Sex")) +
  facet_wrap(~ Survived, scales = "fixed")

# use of lode controls
ggplot(as.data.frame(Titanic),
       aes(weight = Freq,
           axis1 = Class, axis2 = Sex, axis3 = Age)) +
  geom_alluvium(aes(fill = Survived),
                lode_favor = "aes", lode_order = "leftward") +
  geom_stratum() + geom_text(stat = "stratum") +
  scale_x_continuous(breaks = 1:3, labels = c("Class", "Sex", "Age"))
