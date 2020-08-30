# Titanic data in alluvia format
titanic_alluvia <- as.data.frame(Titanic)
head(titanic_alluvia)
is_alluvia_form(titanic_alluvia,
                weight = "Freq")
# Titanic data in lodes format
titanic_lodes <- to_lodes_form(titanic_alluvia,
                               key = "x", value = "stratum", id = "alluvium",
                               axes = 1:4)
head(titanic_lodes)
is_lodes_form(titanic_lodes,
              key = "x", value = "stratum", id = "alluvium",
              weight = "Freq")
# again in lodes format, this time diffusing the `Class` variable
titanic_lodes2 <- to_lodes_form(titanic_alluvia,
                                key = variable, value = value,
                                id = cohort,
                                1:3, diffuse = Class)
head(titanic_lodes2)
is_lodes_form(titanic_lodes2,
              key = variable, value = value, id = cohort,
              weight = Freq)
# use `site` to separate data before lode testing
is_lodes_form(titanic_lodes2,
              key = variable, value = value, id = Class,
              weight = Freq)
is_lodes_form(titanic_lodes2,
              key = variable, value = value, id = Class,
              weight = Freq, site = cohort)

# curriculum data in lodes format
data(majors)
head(majors)
is_lodes_form(majors,
              key = "semester", value = "curriculum", id = "student")
# curriculum data in alluvia format
majors_alluvia <- to_alluvia_form(majors,
                                  key = "semester", value = "curriculum",
                                  id = "student")
head(majors_alluvia)
is_alluvia_form(majors_alluvia, tidyselect::starts_with("CURR"))

# distill variables that vary within `id` values
set.seed(1)
majors$hypo_grade <- LETTERS[sample(5, size = nrow(majors), replace = TRUE)]
majors_alluvia2 <- to_alluvia_form(majors,
                                   key = "semester", value = "curriculum",
                                   id = "student",
                                   distill = "most")
head(majors_alluvia2)

# options to distinguish strata at different axes
gg <- ggplot(majors_alluvia,
             aes(axis1 = CURR1, axis2 = CURR7, axis3 = CURR13))
gg +
  geom_alluvium(aes(fill = as.factor(student)), width = 2/5, discern = TRUE) +
  geom_stratum(width = 2/5, discern = TRUE) +
  geom_text(stat = "stratum", discern = TRUE, aes(label = after_stat(stratum)))
gg +
  geom_alluvium(aes(fill = as.factor(student)), width = 2/5, discern = FALSE) +
  geom_stratum(width = 2/5, discern = FALSE) +
  geom_text(stat = "stratum", discern = FALSE, aes(label = after_stat(stratum)))
# warning when inappropriate
ggplot(majors[majors$semester %in% paste0("CURR", c(1, 7, 13)), ],
       aes(x = semester, stratum = curriculum, alluvium = student,
           label = curriculum)) +
  geom_alluvium(aes(fill = as.factor(student)), width = 2/5, discern = TRUE) +
  geom_stratum(width = 2/5, discern = TRUE) +
  geom_text(stat = "stratum", discern = TRUE)
