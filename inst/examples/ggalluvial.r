# shortcut using ggplot2 syntax (requires data frame input)
ggalluvial(as.data.frame(Titanic),
           aes(axis1 = Age, axis2 = Sex, axis3 = Class, weight = Freq))

# shortcut using formula interface
ggalluvial(data = as.data.frame(Titanic),
           formula = ~ Age + Sex + Class,
           weight = "Freq")

# formula interface with frequency array input and within-flow stratification
ggalluvial(Survived ~ Age + Sex + Class, Titanic)
