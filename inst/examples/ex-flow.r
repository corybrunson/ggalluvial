# parallel sets
ggplot(as.data.frame(Titanic),
       aes(weight = Freq,
           axis1 = Class, axis2 = Sex, axis3 = Age)) +
  geom_flow(aes(fill = Survived),
            width = 1/8, knot.pos = 0) +
  geom_stratum(width = 1/8) + geom_text(stat = "stratum") +
  scale_x_continuous(breaks = 1:3, labels = c("Class", "Sex", "Age")) +
  coord_flip()

# use of strata and labels
ggplot(as.data.frame(Titanic),
       aes(weight = Freq,
           axis1 = Class, axis2 = Sex, axis3 = Age)) +
  geom_flow() +
  scale_x_continuous(breaks = 1:3, labels = c("Class", "Sex", "Age")) +
  geom_stratum() + geom_text(stat = "stratum") +
  ggtitle("Alluvial diagram of Titanic passenger demographic data")

# use of facets
ggplot(as.data.frame(Titanic),
       aes(weight = Freq,
           axis1 = Class, axis2 = Sex)) +
  geom_flow(aes(fill = Age)) +
  geom_stratum() + geom_text(stat = "stratum") +
  scale_x_continuous(breaks = 1:2, labels = c("Class", "Sex")) +
  facet_wrap(~ Survived, scales = "fixed")

# use of lode controls
ggplot(as.data.frame(Titanic),
       aes(weight = Freq,
           axis1 = Class, axis2 = Sex, axis3 = Age)) +
  geom_flow(aes(fill = Survived),
            aes.bind = TRUE, lode.guidance = "rightward") +
  geom_stratum() + geom_text(stat = "stratum") +
  scale_x_continuous(breaks = 1:3, labels = c("Class", "Sex", "Age"))

# use of lode ordering
lode_ord <- replicate(n = 3, expr = sample(x = 32), simplify = FALSE)
ggplot(as.data.frame(Titanic),
       aes(weight = Freq,
           axis1 = Class, axis2 = Sex, axis3 = Age)) +
  geom_flow(aes(fill = Survived),
            lode.ordering = lode_ord) +
  geom_stratum() + geom_text(stat = "stratum") +
  scale_x_continuous(breaks = 1:3, labels = c("Class", "Sex", "Age"))
