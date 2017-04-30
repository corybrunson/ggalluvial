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

# time series alluvia of WorldPhones data
wph <- as.data.frame(as.table(WorldPhones))
names(wph) <- c("Year", "Region", "Telephones")
ggplot(wph,
       aes(x = Year, stratum = Region, weight = Telephones)) +
  geom_flow(aes(fill = Region, colour = Region), width = 0)

# rightward flow aesthetics for vaccine survey data
data(vaccinations)
ggplot(vaccinations,
       aes(x = survey, stratum = response, alluvium = subject,
           weight = freq, fill = response)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum")

# memoryless flows for vaccine survey data
data(vaccinations)
ggplot(vaccinations,
       aes(x = survey, stratum = response, alluvium = subject,
           weight = freq, fill = response)) +
  geom_flow(stat = "flow") +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum")

ggplot(as.data.frame(Titanic),
       aes(weight = Freq,
           axis1 = Sex, axis2 = Age)) +
  geom_flow(stat = "flow", aes(fill = Survived)) +
  geom_stratum() +
  geom_text(stat = "stratum") +
  scale_x_continuous(breaks = 1:3,
                     labels = c("Class", "Sex", "Age"))

data(vaccinations)
vaccinations$subgroup <- LETTERS[1:2][rbinom(
  n = length(unique(vaccinations$subject)), size = 1, prob = .5
) + 1][vaccinations$subject]
ggplot(vaccinations,
       aes(x = survey, stratum = response, alluvium = subject,
           weight = freq, fill = response)) +
  geom_flow(stat = "flow", aes(alpha = subgroup)) +
  scale_alpha_discrete(range = c(1/3, 2/3)) +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum")
