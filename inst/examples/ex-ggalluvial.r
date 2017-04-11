# shortcut using ggplot2 syntax (requires data frame input)
ggalluvial(as.data.frame(Titanic),
           aes(axis1 = Age, axis2 = Sex, axis3 = Class, weight = Freq))
ggalluvial(to_lodes(as.data.frame(Titanic), axes = 1:4),
           aes(x = key, stratum = value, alluvium = id, weight = Freq))

# shortcut using formula interface
ggalluvial(data = as.data.frame(Titanic),
           formula = ~ Age + Sex + Class,
           weight = "Freq")

# formula interface with frequency array input and within-flow stratification
ggalluvial(Survived ~ Age + Sex + Class, Titanic)

# formula interface with only one dependent variable (for time series)
ggalluvial(Survived ~ Class, Titanic)

# formula interface for time series of Refugee data
ggalluvial(country ~ year, alluvial::Refugees, weight = "refugees",
           incl.strata = FALSE)
