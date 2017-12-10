data(Titanic)
titanic <- to_lodes(as.data.frame(Titanic), axes = 1:4)
ggplot(titanic,
       aes(x = x, stratum = stratum, alluvium = alluvium,
           weight = Freq, label = stratum)) +
  geom_alluvium() +
  geom_stratum() + geom_text(stat = "stratum")
weight_perm <- optimize_strata(
  titanic,
  id = "alluvium", key = "x", value = "stratum",
  weight = "Freq"
)
weight_perm
titanic_weight <- permute_axis_strata(
  titanic,
  id = "alluvium", key = "x", value = "stratum",
  perms = weight_perm$perms
)
ggplot(titanic_weight,
       aes(x = x, stratum = stratum, alluvium = alluvium,
           weight = Freq, label = stratum)) +
  geom_alluvium() +
  geom_stratum() + geom_text(stat = "stratum")

# multiple axes with the same strata
data(vaccinations)
ggplot(vaccinations,
       aes(x = survey, stratum = response, alluvium = subject,
           weight = freq, fill = response,
           label = response)) +
  geom_flow() +
  geom_stratum() + geom_text(stat = "stratum")
count_perm <- optimize_strata(
  vaccinations,
  id = "subject", key = "survey", value = "response"
)
count_perm
vaccinations_count <- permute_strata(
  vaccinations,
  id = "subject", key = "survey", value = "response",
  perm = count_perm$perm
)
ggplot(vaccinations_count,
       aes(x = survey, stratum = response, alluvium = subject,
           weight = freq, fill = response,
           label = response)) +
  geom_flow() +
  geom_stratum() + geom_text(stat = "stratum")
weight_perm <- optimize_strata(
  vaccinations,
  id = "subject", key = "survey", value = "response",
  weight = "freq"
)
weight_perm
vaccinations_weight <- permute_strata(
  vaccinations,
  id = "subject", key = "survey", value = "response",
  perm = weight_perm$perm
)
ggplot(vaccinations_weight,
       aes(x = survey, stratum = response, alluvium = subject,
           weight = freq, fill = response,
           label = response)) +
  geom_flow() +
  geom_stratum() + geom_text(stat = "stratum")

# many axes with different subsets of strata
data(majors)
ggplot(majors,
       aes(x = semester, stratum = curriculum, alluvium = student,
           fill = curriculum, label = curriculum)) +
  geom_flow(color = "darkgray") +
  geom_stratum()
count_perm <- optimize_strata(
  majors,
  id = "student", key = "semester", value = "curriculum",
  method = "heuristic", niter = 12
)
count_perm
majors_perm <- permute_strata(
  majors,
  id = "student", key = "semester", value = "curriculum",
  perm = count_perm$perm
)
ggplot(majors_perm,
       aes(x = semester, stratum = curriculum, alluvium = student,
           fill = curriculum, label = curriculum)) +
  geom_flow(color = "darkgray") +
  geom_stratum()

# many axes and strata subsets, tracking individuals/cohorts
ggplot(majors,
       aes(x = semester, stratum = curriculum, alluvium = student,
           fill = curriculum, label = curriculum)) +
  geom_flow(stat = "alluvium", lode.guidance = "rightleft",
            color = "darkgray") +
  geom_stratum()
count_perm <- optimize_strata(
  majors,
  id = "student", key = "semester", value = "curriculum",
  method = "heuristic", niter = 12
)
count_perm
majors_perm <- permute_strata(
  majors,
  id = "student", key = "semester", value = "curriculum",
  perm = count_perm$perm
)
ggplot(majors_perm,
       aes(x = semester, stratum = curriculum, alluvium = student,
           fill = curriculum, label = curriculum)) +
  geom_flow(stat = "alluvium", lode.guidance = "rightleft",
            color = "darkgray") +
  geom_stratum()
