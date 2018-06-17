png(filename = "ggalluvial.png",
    width = 350, height = 300)
ggplot(data = to_lodes(as.data.frame(Titanic),
                       key = "Demographic",
                       axes = 1:3),
       aes(x = Demographic, stratum = value, alluvium = id,
           weight = Freq, label = value)) +
  geom_alluvium(aes(fill = Survived)) +
  geom_stratum() + geom_text(stat = "stratum") +
  theme(legend.position = "bottom") +
  ggtitle("passengers on the maiden voyage of the Titanic")
dev.off()
