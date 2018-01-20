# Titanic data in alluvia format
titanic_alluvia <- as.data.frame(Titanic)
head(titanic_alluvia)
is_alluvial(titanic_alluvia,
            weight = "Freq")
# Titanic data in lodes format
titanic_lodes <- to_lodes(titanic_alluvia,
                          key = "x", value = "stratum", id = "alluvium",
                          axes = 1:4)
head(titanic_lodes)
is_alluvial(titanic_lodes,
            key = "x", value = "stratum", id = "alluvium",
            weight = "Freq")
# again in lodes format, this time keeping 'Class' as a variable
titanic_lodes2 <- to_lodes(titanic_alluvia,
                           key = "variable", value = "value", id = "passenger",
                           axes = 1:3, keep = "Class")
head(titanic_lodes2)
is_alluvial(titanic_lodes2,
            key = "variable", value = "value", id = "passenger",
            weight = "Freq")

# curriculum data in lodes format
data(majors)
head(majors)
is_alluvial(majors,
            key = "semester", value = "curriculum", id = "student",
            logical = FALSE)
# curriculum data in alluvia format
majors2 <- to_alluvia(majors,
                      key = "semester", value = "curriculum", id = "student")
head(majors2)
is_alluvial(majors2,
            axes = 2:9,
            logical = FALSE)

# options to relevel strata
gg <- ggplot(titanic_alluvia,
             aes(axis1 = Class, axis2 = Sex, axis3 = Age, weight = Freq))
gg +
  geom_flow(aes(fill = Survived), color = "darkgray") +
  geom_stratum() +
  geom_text(stat = "stratum", label.strata = TRUE)
gg +
  geom_flow(aes(fill = Survived), color = "darkgray", relevel.strata = TRUE) +
  geom_stratum(relevel.strata = TRUE) +
  geom_text(stat = "stratum", relevel.strata = TRUE, label.strata = TRUE)
relevs <- c("Adult", "Child")
gg +
  geom_flow(aes(fill = Survived), color = "darkgray", relevel.strata = relevs) +
  geom_stratum(relevel.strata = relevs) +
  geom_text(stat = "stratum", relevel.strata = relevs, label.strata = TRUE)
# warning when inappropriate
ggplot(titanic_lodes,
       aes(x = x, stratum = stratum, alluvium = alluvium, weight = Freq,
           label = stratum)) +
  geom_flow(color = "darkgray", relevel.strata = TRUE) +
  geom_stratum(relevel.strata = TRUE) +
  geom_text(stat = "stratum", relevel.strata = TRUE)
