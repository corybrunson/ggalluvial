# degeneracy (one axis; unavailable through shortcut function)
ggplot(as.data.frame(Titanic),
       aes(weight = Freq,
           axis = Class)) +
  geom_alluvium(aes(fill = Class, alpha = Survived)) +
  scale_x_continuous(breaks = 1, labels = c("Class")) +
  scale_alpha_manual(values = c(.25, .75))

# declaration of groups (ignored)
ggplot(as.data.frame(Titanic),
       aes(weight = Freq,
           axis1 = Class, axis2 = Sex, axis3 = Age,
           group = Survived)) +
  geom_alluvium() +
  scale_x_continuous(breaks = 1:3, labels = c("Class", "Sex", "Age"))

# control of horizontal spacing: parallel sets
ggplot(as.data.frame(Titanic),
       aes(weight = Freq,
           axis1 = Class, axis2 = Sex, axis3 = Age)) +
  geom_alluvium(aes(fill = Survived),
                width = 0, knot.pos = 0) +
  scale_x_continuous(breaks = 1:3, labels = c("Class", "Sex", "Age"))

# use of strata and labels
ggplot(as.data.frame(Titanic),
       aes(weight = Freq,
           axis1 = Class, axis2 = Sex, axis3 = Age)) +
  geom_alluvium() +
  scale_x_continuous(breaks = 1:3, labels = c("Class", "Sex", "Age")) +
  geom_stratum() + geom_text(stat = "stratum") +
  ggtitle("Alluvial diagram of Titanic passenger demographic data")

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
                bind.by.aes = TRUE, lode.guidance = "rightward") +
  geom_stratum() + geom_text(stat = "stratum") +
  scale_x_continuous(breaks = 1:3, labels = c("Class", "Sex", "Age"))

# use of lode ordering
lode_ord <- replicate(n = 3, expr = sample(x = 32), simplify = FALSE)
ggplot(as.data.frame(Titanic),
       aes(weight = Freq,
           axis1 = Class, axis2 = Sex, axis3 = Age)) +
  geom_alluvium(aes(fill = Survived),
                lode.ordering = lode_ord) +
  geom_stratum() + geom_text(stat = "stratum") +
  scale_x_continuous(breaks = 1:3, labels = c("Class", "Sex", "Age"))
