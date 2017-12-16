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
