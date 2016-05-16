# basic boxes (strata)
ggplot(as.data.frame(Titanic),
       aes(freq = Freq, axis1 = Class, axis2 = Sex)) +
    geom_stratum()

# combining flows and boxes
ggplot(as.data.frame(Titanic),
       aes(freq = Freq, axis1 = Class, axis2 = Sex)) +
    geom_alluvium(aes(fill = Age)) +
    geom_stratum()

# combining flows and boxes and using facets
ggplot(as.data.frame(Titanic),
       aes(freq = Freq, axis1 = Class, axis2 = Sex)) +
    geom_alluvium(aes(fill = Age)) +
    geom_stratum() +
    facet_wrap(~ Survived, scales = "free_y")
