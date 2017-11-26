data <- to_lodes(as.data.frame(Titanic), axes = 1:4)
ggplot(data,
       aes(x = x, stratum = stratum, alluvium = alluvium, weight = Freq)) +
  geom_alluvium() + geom_stratum()

count_perms <- optimize_strata(
  data,
  key = "x", value = "stratum", id = "alluvium",
  free.strata = TRUE,
  objective = "count", method = "exhaustive"
)

\dontrun{
count_perms <- optimize_strata(
  data,
  key = "x", value = "stratum", id = "alluvium",
  free.strata = TRUE,
  objective = "count", method = "heuristic"
)
}

weight_perms <- optimize_strata(
  data,
  key = "x", value = "stratum", id = "alluvium",
  weight = "Freq",
  free.strata = TRUE,
  objective = "weight", method = "exhaustive"
)

\dontrun{
weight_perms <- optimize_strata(
  data,
  key = "x", value = "stratum", id = "alluvium",
  weight = "Freq",
  free.strata = TRUE,
  objective = "weight", method = "heuristic"
)
}

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
