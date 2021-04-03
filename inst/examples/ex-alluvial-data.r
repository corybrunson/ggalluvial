# Titanic data in alluvia format
titanic_alluvia <- as.data.frame(Titanic)
head(titanic_alluvia)
is_alluvia_form(titanic_alluvia,
                y = "Freq")
# Titanic data in lodes format
titanic_lodes <- to_lodes_form(titanic_alluvia,
                               key = "x", value = "stratum", id = "alluvium",
                               axes = c(Class, Age, Sex, Survived))
head(titanic_lodes)
is_lodes_form(titanic_lodes,
              key = "x", value = "stratum", id = "alluvium",
              y = "Freq")
# again in lodes format, this time diffusing the `Class` variable
titanic_lodes2 <- to_lodes_form(titanic_alluvia,
                                key = variable, value = value,
                                id = cohort,
                                axes = 1:3, diffuse = 1)
head(titanic_lodes2)
is_lodes_form(titanic_lodes2,
              key = variable, value = value, id = cohort,
              y = Freq)
# use `site` to separate data before lode testing
is_lodes_form(titanic_lodes2,
              key = variable, value = value, id = Class,
              y = Freq)
is_lodes_form(titanic_lodes2,
              key = variable, value = value, id = Class,
              y = Freq, site = cohort)
# once more in lodes format, this time specifying a `y` variable
titanic_lodes3 <- to_lodes_form(titanic_alluvia,
                                axes = 1:4,
                                key = demographic, value = value,
                                id = cohort, y = Freq,
                                y_to = "count")
head(titanic_lodes3)
is_lodes_form(titanic_lodes3,
              id = cohort, key = demographic, value = value, y = count)

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

# vaccination data in lodes format
data(vaccinations)
head(vaccinations)
is_lodes_form(vaccinations,
              id = subject, key = survey, value = response, y = freq)
# vaccination data in alluvia format, with fixed `y`
head(to_alluvia_form(vaccinations,
                     id = subject, key = survey, value = response,
                     y = freq))
# vaccination data in alluvial format, with variable `y`
set.seed(1)
vaccinations$perm <- sample(vaccinations$freq)
head(vaccinations)
head(to_alluvia_form(vaccinations,
                     id = subject, key = survey, value = response,
                     y = perm))

\dontrun{
# refugee data in lodes format
refugees <- alluvial::Refugees
head(refugees)
is_lodes_form(refugees,
              id = country, key = year, value = country, y = refugees)
# refugee data in alluvial format, without `y` values
to_alluvia_form(refugees,
                key = year, id = country, value = country)
# refugee data in alluvial format, with variable `y` values
to_alluvia_form(refugees,
                key = year, id = country, value = country, y = refugees) ->
  refugees_alluvia
print(refugees_alluvia)
# back to lodes format
head(to_lodes_form(refugees_alluvia,
                   axes = `2003`:`2013`, y = starts_with("refugees_"),
                   id = "id", key = "year", value = "origin",
                   y_to = "count"), n = 12)
}
