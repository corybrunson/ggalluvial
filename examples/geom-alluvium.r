# basic flows (alluvia)
ggplot(as.data.frame(Titanic),
       aes(freq = Freq, axis1 = Class, axis2 = Sex)) +
    geom_alluvium()

# use of aesthetics
ggplot(as.data.frame(Titanic),
       aes(freq = Freq,
           axis1 = Class, axis2 = Sex, axis3 = Age, axis4 = Survived)) +
    geom_alluvium(aes(fill = Age, alpha = Sex, color = Survived)) +
    scale_color_manual(values = c("black", "white"))

# use of facets
ggplot(as.data.frame(Titanic),
       aes(freq = Freq,
           axis1 = Class, axis2 = Sex, axis3 = Age)) +
    geom_alluvium(aes(fill = Age, alpha = Sex)) +
    facet_wrap(~ Survived, scales = "free_y")

# degeneracy (one axis)
ggplot(as.data.frame(Titanic), aes(freq = Freq, axis = Class)) +
    geom_alluvium(aes(fill = Class, color = Survived)) +
    scale_color_manual(values = c("black", "white"))

# degeneracy (no axis)
if (FALSE) {
    ggplot(as.data.frame(Titanic), aes(freq = Freq)) +
        geom_alluvium(aes(fill = Class, color = Survived)) +
        scale_color_manual(values = c("black", "white"))
}

# control of horizontal spacing
ggplot(as.data.frame(Titanic),
       aes(freq = Freq,
           axis1 = Class, axis2 = Sex, axis3 = Age, axis4 = Survived)) +
    geom_alluvium(aes(fill = Age, alpha = Sex),
                  axis_width = 1/5, ribbon_bend = 1/3)

# control of axis positions
if (FALSE) {
    # ridiculous syntax
    ggplot(as.data.frame(Titanic),
           aes(freq = Freq,
               axis1 = Class, axis1.5 = Age, axis2.5 = Sex, axis3 = Survived)) +
        geom_alluvium(aes(fill = Age, alpha = Sex))
}

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

# use of annotation and labels
ggplot(as.data.frame(Titanic),
       aes(freq = Freq, axis1 = Class, axis2 = Sex, axis3 = Age)) +
    geom_alluvium() +
    geom_text(stat = "stratum") +
    ggtitle("Alluvial diagram of Titanic passenger demographic data")
