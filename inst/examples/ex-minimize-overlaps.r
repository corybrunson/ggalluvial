data <- to_lodes(as.data.frame(Titanic), axes = 1:4)
ggplot(data,
       aes(x = x, stratum = stratum, alluvium = alluvium, weight = Freq)) +
  geom_alluvium() + geom_stratum()

count_perms <- optimize_strata(
  data,
  key = "x", value = "stratum", id = "alluvium",
  objective = "count", method = "exhaustive"
)

count_perms <- optimize_strata(
  data,
  key = "x", value = "stratum", id = "alluvium",
  objective = "count", method = "heuristic"
)

weight_perms <- optimize_strata(
  data,
  key = "x", value = "stratum", id = "alluvium",
  weight = "Freq",
  objective = "weight", method = "exhaustive"
)

weight_perms <- optimize_strata(
  data,
  key = "x", value = "stratum", id = "alluvium",
  weight = "Freq",
  objective = "weight", method = "heuristic"
)

data <- to_lodes(as.data.frame(Titanic), axes = 1:4)
ggplot(data,
       aes(x = x, stratum = stratum, alluvium = alluvium, weight = Freq)) +
  geom_alluvium() + geom_stratum() +
  geom_text(stat = "stratum", aes(label = stratum))
data2 <- permute_strata(data,
                        key = "x", value = "stratum", id = "alluvium",
                        permutations = count_perms)
ggplot(data2,
       aes(x = x, stratum = stratum, alluvium = alluvium, weight = Freq)) +
  geom_alluvium() + geom_stratum() +
  geom_text(stat = "stratum", aes(label = stratum))

# require strata to be consistently ordered at each axis
data(majors)
ggplot(majors,
       aes(x = semester, stratum = curriculum, alluvium = student,
           fill = curriculum, label = curriculum)) +
  geom_flow(stat = "alluvium", lode.guidance = "rightleft",
            color = "darkgray") +
  geom_stratum()
ggalluvial:::objective_fun(
  transform(majors, weight = 1),
  id = "student", key = "semester", value = "curriculum",
  weight = "weight",
  perms = lapply(sapply(sort(unique(majors$semester)), function(x) {
    length(unique(majors[majors$semester == x, ]$curriculum))
  }), seq_len)
)
count_perms <- optimize_strata(
  majors,
  id = "student", key = "semester", value = "curriculum",
  free.strata = TRUE,
  objective = "count", method = "heuristic"
)
majors2 <- permute_strata(
  majors,
  id = "student", key = "semester", value = "curriculum",
  permutations = count_perms
)
ggplot(majors2,
       aes(x = semester, stratum = curriculum, alluvium = student,
           fill = curriculum, label = curriculum)) +
  geom_flow(stat = "alluvium", lode.guidance = "rightleft",
            color = "darkgray") +
  geom_stratum()
ggalluvial:::objective_fun(
  transform(majors2, weight = 1),
  id = "student", key = "semester", value = "curriculum",
  weight = "weight",
  perms = lapply(sapply(sort(unique(majors$semester)), function(x) {
    length(unique(majors[majors$semester == x, ]$curriculum))
  }), seq_len)
)

# allow inconsistent orderings of strata across axes
data(vaccinations)
ggplot(vaccinations,
       aes(x = survey, stratum = response, alluvium = subject,
           weight = freq, fill = response,
           label = response)) +
  geom_flow() +
  geom_stratum() +
  geom_text(stat = "stratum")
ggalluvial:::objective_fun(
  vaccinations,
  id = "subject", key = "survey", value = "response",
  weight = "freq",
  perms = lapply(sapply(sort(unique(vaccinations$survey)), function(x) {
    length(unique(vaccinations[vaccinations$survey == x, ]$response))
  }), seq_len)
)
weight_perms1 <- optimize_strata(
  vaccinations,
  id = "subject", key = "survey", value = "response",
  free.strata = TRUE,
  weight = "freq",
  objective = "weight"
)
vaccinations1 <- permute_strata(
  vaccinations,
  id = "subject", key = "survey", value = "response",
  permutations = weight_perms1
)
ggplot(vaccinations1,
       aes(x = survey, stratum = response, alluvium = subject,
           weight = freq, fill = .stratum,
           label = .stratum)) +
  geom_flow() +
  geom_stratum() +
  geom_text(stat = "stratum")
ggalluvial:::objective_fun(
  vaccinations1,
  id = "subject", key = "survey", value = "response",
  weight = "freq",
  perms = lapply(sapply(sort(unique(vaccinations1$survey)), function(x) {
    length(unique(vaccinations1[vaccinations1$survey == x, ]$response))
  }), seq_len)
)
weight_perms2 <- optimize_strata(
  vaccinations,
  id = "subject", key = "survey", value = "response",
  free.strata = FALSE,
  weight = "freq",
  objective = "weight"
)
vaccinations2 <- permute_strata(
  vaccinations,
  id = "subject", key = "survey", value = "response",
  permutations = weight_perms2
)
ggplot(vaccinations2,
       aes(x = survey, stratum = response, alluvium = subject,
           weight = freq, fill = .stratum,
           label = .stratum)) +
  geom_flow() +
  geom_stratum() +
  geom_text(stat = "stratum")
ggalluvial:::objective_fun(
  vaccinations2,
  id = "subject", key = "survey", value = "response",
  weight = "freq",
  perms = lapply(sapply(sort(unique(vaccinations2$survey)), function(x) {
    length(unique(vaccinations2[vaccinations2$survey == x, ]$response))
  }), seq_len)
)
