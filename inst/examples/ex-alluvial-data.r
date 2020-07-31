# Titanic data in alluvia format
titanic_alluvia <- as.data.frame(Titanic)
head(titanic_alluvia)
is_alluvia_form(titanic_alluvia,
                y = "Freq")
# Titanic data in lodes format
titanic_lodes <- to_lodes_form(titanic_alluvia,
                               alluvia_to = "alluvium",
                               axes_to = "x",
                               strata_to = "stratum",
                               axes = c(Class, Age, Sex, Survived))
head(titanic_lodes)
is_lodes_form(titanic_lodes,
              alluvia_from = "alluvium",
              axes_from = "x",
              strata_from = "stratum",
              y = "Freq")
# again in lodes format, this time diffusing the `Class` variable
titanic_lodes2 <- to_lodes_form(titanic_alluvia,
                                alluvia_to = "passenger",
                                axes_to = "variable",
                                strata_to = "value",
                                axes = 1:3, diffuse = 1)
head(titanic_lodes2)
is_lodes_form(titanic_lodes2,
              alluvia_from = passenger,
              axes_from = variable,
              strata_from = value,
              y = Freq)
# once more in lodes format, this time specifying a `y` variable
titanic_lodes3 <- to_lodes_form(titanic_alluvia,
                                axes = 1:4,
                                alluvia_to = "passenger",
                                axes_to = "demographic",
                                strata_to = "value",
                                y = Freq,
                                y_to = "count")
head(titanic_lodes3)
is_lodes_form(titanic_lodes3,
              alluvia_from = passenger, axes_from = demographic,
              strata_from = value, y = count)

# curriculum data in lodes format
data(majors)
head(majors)
is_lodes_form(majors,
              alluvia_from = "student", axes_from = "semester",
              strata_from = "curriculum")
# curriculum data in alluvia format
majors_alluvia <- to_alluvia_form(majors,
                                  alluvia_from = "student",
                                  axes_from = "semester",
                                  strata_from = "curriculum")
head(majors_alluvia)
is_alluvia_form(majors_alluvia, tidyselect::starts_with("CURR"))

# distill variables that vary within `id` values
set.seed(1)
majors$hyp_grade <- LETTERS[sample(5, size = nrow(majors), replace = TRUE)]
majors_alluvia2 <- to_alluvia_form(majors,
                                   alluvia_from = "student",
                                   axes_from = "semester",
                                   strata_from = "curriculum",
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
              alluvia_from = subject, axes_from = survey,
              strata_from = response, y = freq)
# vaccination data in alluvia format, with fixed `y`
head(to_alluvia_form(vaccinations,
                     alluvia_from = subject, axes_from = survey,
                     strata_from = response, y = freq))
# vaccination data in alluvial format, with variable `y`
set.seed(1)
vaccinations$perm <- sample(vaccinations$freq)
head(vaccinations)
head(to_alluvia_form(vaccinations,
                     alluvia_from = subject, axes_from = survey,
                     strata_from = response, y = perm,
                     distill = FALSE))

\dontrun{
# refugee data in lodes format
refugees <- alluvial::Refugees
head(refugees)
is_lodes_form(refugees,
              alluvia_from = country, axes_from = year, strata_from = country,
              y = refugees)
# refugee data in alluvial format, without `y` values
to_alluvia_form(refugees,
                axes_from = year, alluvia_from = country, strata_from = country)
# refugee data in alluvial format, with variable `y` values
to_alluvia_form(refugees,
                axes_from = year,
                alluvia_from = country,
                strata_from = country,
                y = refugees) ->
  refugees_alluvia
print(refugees_alluvia)
# back to lodes format
head(to_lodes_form(refugees_alluvia,
                   axes = `2003`:`2013`, y = starts_with("refugees_"),
                   alluvia_to = "id", axes_to = "year", strata_to = "origin",
                   y_to = "count"), n = 12)
}

# advanced conversion options
test_alluvia <- data.frame(
  id = LETTERS[seq(4L)],
  key1 = letters[sample(4L, replace = TRUE)],
  key2 = letters[sample(4L, replace = TRUE)],
  key3 = letters[sample(4L, replace = TRUE)],
  n1 = sample(6, 4L, replace = TRUE),
  n2 = sample(6L, 4L, replace = TRUE),
  n3 = sample(6L, 4L, replace = TRUE),
  wt1 = runif(4L, 0, 1),
  wt2 = runif(4L, 0, 1),
  wt3 = runif(4L, 0, 1)
)
# no heights or weights
to_lodes_form(test_alluvia,
              axes = starts_with("key"),
              alluvia_to = "alluv",
              axes_to = "instance", axes_prefix = "key",
              strata_to = "value")
# uniform heights
to_lodes_form(test_alluvia,
              axes = starts_with("key"), y = "n1",
              alluvia_to = "alluv",
              axes_to = "instance", axes_prefix = "key",
              strata_to = "value")
# variable heights
to_lodes_form(test_alluvia,
              axes = starts_with("key"), y = starts_with("n"),
              alluvia_to = "alluv",
              axes_to = "instance", axes_prefix = "key",
              strata_to = "value")
# variable heights with custom name
to_lodes_form(test_alluvia,
              axes = starts_with("key"),
              y = starts_with("n"),
              alluvia_to = "alluv",
              axes_to = "instance", axes_prefix = "key",
              strata_to = "value",
              y_to = "count")
# uniform heights and variable weights
to_lodes_form(test_alluvia,
              axes = starts_with("key"),
              y = "n2", weight = starts_with("wt"),
              alluvia_to = "alluv",
              axes_to = "instance", axes_prefix = "key",
              strata_to = "value",
              y_to = "count")
# variable heights and weights with custom names
to_lodes_form(test_alluvia,
              axes = starts_with("key"),
              y = starts_with("n"), weight = starts_with("wt"),
              alluvia_to = "alluv",
              axes_to = "instance", axes_prefix = "key",
              strata_to = "value",
              y_to = "count", weight_to = "contribution")
test_lodes <- .Last.value
# no heights or weights
to_alluvia_form(test_lodes,
                alluvia_from = id,
                axes_from = instance,
                strata_from = value)
# variable heights
to_alluvia_form(test_lodes,
                alluvia_from = id,
                axes_from = instance,
                strata_from = value,
                y = count)
# variable heights with distilled weights and prefixed axes
to_alluvia_form(test_lodes,
                alluvia_from = id,
                axes_from = instance,
                axes_prefix = "axis", axes_sep = "_for_",
                strata_from = value,
                y = count,
                distill = "mean")
# variable heights and weights
to_alluvia_form(test_lodes,
                alluvia_from = id,
                axes_from = instance,
                strata_from = value,
                y = count,
                weight = contribution)
# variable heights and wights with custom impution
test_lodes <- test_lodes[-sample(nrow(test_lodes), 2L), , drop = FALSE]
to_alluvia_form(test_lodes,
                alluvia_from = id,
                axes_from = instance,
                strata_from = value,
                strata_fill = list(count = 0L, contribution = 0),
                y = count,
                weight = contribution)
# un-factor stratum variable to use custom imputation
test_lodes$value <- as.character(test_lodes$value)
to_alluvia_form(test_lodes,
                alluvia_from = id,
                axes_from = instance,
                strata_from = value,
                strata_fill = list(value = "z", count = 0L, contribution = 0),
                y = count,
                weight = contribution)
