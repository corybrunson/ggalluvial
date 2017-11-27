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
