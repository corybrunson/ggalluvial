# basic boxes (strata) with labels
ggplot(as.data.frame(Titanic),
       aes(weight = Freq, axis1 = Class, axis2 = Sex)) +
    geom_stratum() + geom_text(stat = "stratum")

# full axis width
ggplot(as.data.frame(Titanic),
       aes(weight = Freq,
           axis1 = Class, axis2 = Sex, axis3 = Age, axis4 = Survived)) +
    geom_stratum(axis_width = 1) + geom_text(stat = "stratum")

# adding flows to boxes
ggplot(as.data.frame(Titanic),
       aes(weight = Freq, axis1 = Class, axis2 = Sex)) +
    geom_stratum() + geom_text(stat = "stratum") +
    geom_alluvium(aes(fill = Age))

# combining flows and boxes and using facets
ggplot(as.data.frame(Titanic),
       aes(weight = Freq, axis1 = Class, axis2 = Sex)) +
    geom_alluvium(aes(fill = Age)) +
    geom_stratum() + geom_text(stat = "stratum") +
    facet_wrap(~ Survived, scales = "free_y")
