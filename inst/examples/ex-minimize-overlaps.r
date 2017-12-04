data <- to_lodes(as.data.frame(Titanic), axes = 1:4)
# overlap count, heuristic search
count_perms <- optimize_strata(
  data,
  key = "x", value = "stratum", id = "alluvium",
  objective = "count", method = "heuristic"
)
count_perms
# overlap count, exhaustive search
count_perms <- optimize_strata(
  data,
  key = "x", value = "stratum", id = "alluvium",
  objective = "count", method = "exhaustive"
)
count_perms
# overlap weight, heuristic search
weight_perms <- optimize_strata(
  data,
  key = "x", value = "stratum", id = "alluvium",
  weight = "Freq",
  objective = "weight", method = "heuristic"
)
weight_perms
# overlap weight, exhaustive search
weight_perms <- optimize_strata(
  data,
  key = "x", value = "stratum", id = "alluvium",
  weight = "Freq",
  objective = "weight", method = "exhaustive"
)
weight_perms
# plot and re-plot the alluvial diagram
ggplot(data,
       aes(x = x, stratum = stratum, alluvium = alluvium, weight = Freq)) +
  geom_alluvium() + geom_stratum() +
  geom_text(stat = "stratum", aes(label = stratum))
\dontrun{
data2 <- permute_axis_strata(
  data,
  key = "x", value = "stratum", id = "alluvium",
  perms = count_perms
)
ggplot(data2,
       aes(x = x, stratum = stratum, alluvium = alluvium, weight = Freq)) +
  geom_alluvium() + geom_stratum() +
  geom_text(stat = "stratum", aes(label = stratum))
}

# multiple axes with the same strata
data(vaccinations)
ggplot(vaccinations,
       aes(x = survey, stratum = response, alluvium = subject,
           weight = freq, fill = response,
           label = response)) +
  geom_flow() +
  geom_stratum() +
  geom_text(stat = "stratum")
objective_fun(
  vaccinations,
  id = "subject", key = "survey", value = "response",
  weight = "freq",
  perm = lapply(sapply(sort(unique(vaccinations$survey)), function(x) {
    length(unique(vaccinations[vaccinations$survey == x, ]$response))
  }), seq_len)
)
weight_perms1 <- optimize_strata(
  vaccinations,
  id = "subject", key = "survey", value = "response",
  weight = "freq",
  objective = "weight"
)
weight_perms1
vaccinations1 <- permute_strata(
  vaccinations,
  id = "subject", key = "survey", value = "response",
  perm = weight_perms1$perm
)
objective_fun(
  vaccinations,
  id = "subject", key = "survey", value = "response",
  weight = "freq",
  perm = weight_perms1$perms
)
objective_fun(
  vaccinations1,
  id = "subject", key = "survey", value = "response",
  weight = "freq",
  perm = lapply(sapply(sort(unique(vaccinations1$survey)), function(x) {
    length(unique(vaccinations1[vaccinations1$survey == x, ]$response))
  }), seq_len)
)
ggplot(vaccinations1,
       aes(x = survey, stratum = response, alluvium = subject,
           weight = freq, fill = response,
           label = response)) +
  geom_flow() +
  geom_stratum() +
  geom_text(stat = "stratum")
weight_perms2 <- optimize_strata(
  vaccinations,
  id = "subject", key = "survey", value = "response",
  weight = "freq",
  objective = "weight"
)
weight_perms2
vaccinations2 <- permute_strata(
  vaccinations,
  id = "subject", key = "survey", value = "response",
  perm = weight_perms2$perm
)
ggplot(vaccinations2,
       aes(x = survey, stratum = response, alluvium = subject,
           weight = freq, fill = response,
           label = response)) +
  geom_flow() +
  geom_stratum() +
  geom_text(stat = "stratum")

# many axes with variation in the strata included
data(majors)
ggplot(majors,
       aes(x = semester, stratum = curriculum, alluvium = student,
           fill = curriculum, label = curriculum)) +
  geom_flow(stat = "alluvium", lode.guidance = "rightleft",
            color = "darkgray") +
  geom_stratum()
objective_fun(
  transform(majors, weight = 1),
  id = "student", key = "semester", value = "curriculum",
  weight = "weight",
  perm = lapply(sapply(sort(unique(majors$semester)), function(x) {
    length(unique(majors[majors$semester == x, ]$curriculum))
  }), seq_len)
)
count_perms <- optimize_strata(
  majors,
  id = "student", key = "semester", value = "curriculum",
  objective = "count", method = "heuristic"
)
objective_fun(
  transform(majors, weight = 1),
  id = "student", key = "semester", value = "curriculum",
  weight = "weight",
  perm = count_perms$perms
)
majors2 <- permute_strata(
  majors,
  id = "student", key = "semester", value = "curriculum",
  perm = count_perms$perm
)
objective_fun(
  transform(majors2, weight = 1),
  id = "student", key = "semester", value = "curriculum",
  weight = "weight",
  perm = lapply(sapply(sort(unique(majors$semester)), function(x) {
    length(unique(majors[majors$semester == x, ]$curriculum))
  }), seq_len)
)
ggplot(majors2,
       aes(x = semester, stratum = curriculum, alluvium = student,
           fill = curriculum, label = curriculum)) +
  geom_flow(stat = "alluvium", lode.guidance = "rightleft",
            color = "darkgray") +
  geom_stratum()
