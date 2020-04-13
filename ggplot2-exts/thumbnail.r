png(filename = "ggalluvial.png",
    width = 350, height = 300)
ggplot(data = to_lodes_form(as.data.frame(Titanic),
                            alluvia_to = "Cohort",
                            axes_to = "Demographic",
                            strata_to = "Value",
                            axes = 1:3),
       aes(x = Demographic, stratum = Value, alluvium = Cohort,
           y = Freq, label = Value)) +
  geom_alluvium(aes(fill = Survived)) +
  geom_stratum() + geom_text(stat = "stratum") +
  theme(legend.position = "bottom") +
  ggtitle("passengers on the maiden voyage of the Titanic")
dev.off()
