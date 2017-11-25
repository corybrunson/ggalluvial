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

count_perms <- optimize_strata(
  data,
  key = "x", value = "stratum", id = "alluvium",
  free.strata = TRUE,
  objective = "count", method = "heuristic"
)

weight_perms <- optimize_strata(
  data,
  key = "x", value = "stratum", id = "alluvium",
  weight = "Freq",
  free.strata = TRUE,
  objective = "weight", method = "exhaustive"
)

weight_perms <- optimize_strata(
  data,
  key = "x", value = "stratum", id = "alluvium",
  weight = "Freq",
  free.strata = TRUE,
  objective = "weight", method = "heuristic"
)



# NOTE:
# temporarily store stratum factors as a new variable,
# then use the new variable to
# (a) add new levels if any strata appear at multiple axes
# (b) reorder levels

permute_strata <- function(data, key, value, id, perms) {
  stopifnot(is_alluvial_lodes(data, key, value, id))
  
  # if any strata span multiple axes, replace them with multiple levels
  # SHOULD ONLY NEED TO DO THIS IF 'free.strata' IS 'TRUE'
  if (length(unique(data[[value]])) < nrow(unique(data[, c(key, value)]))) {
    warning("Some strata appear at multiple axes ",
            "and will be replaced by new levels with adjusted names.")
  }
  # replace any duplicates with adjusted names
  uniq <- unique(data[, c(key, value)])
  dupe_values <- unique(uniq[[value]][duplicated(uniq[[value]])])
  dupe_no <- cumsum(!duplicated(data[, c(value)])) - 1
  
  # introduce a key-value interaction variable in axis-stratum order
  data <- data[do.call(order, data[, c(key, value)]), ]
  data$.keyvalue <- interaction(data[[key]], data[[value]],
                                lex.order = TRUE, drop = TRUE, sep = "_._")
  # permute the order of 'keyvalue' at each axis
  perm_cums <- c(0, cumsum(sapply(perms, length)))
  perm_levs <- unlist(lapply(seq_along(perms),
                             function(i) perms[[i]] + perm_cums[i]))
  data$.keyvalue <- factor(data$.keyvalue,
                           levels = levels(data$.keyvalue)[perm_levs])
  # reorder value variable according to permutations
  data[[value]] <- factor(
    data[[value]],
    levels = gsub("^.*_\\._(.*$)", "\\1", levels(data$.keyvalue))
  )
}
