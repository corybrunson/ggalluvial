# Titanic data in alluvium form
titanic_alluvia <- as.data.frame(Titanic)
is_alluvial(titanic_alluvia,
            weight = "Freq")

# Titanic data in lode form
titanic_lodes <- suppressWarnings(tidyr::gather(
  dplyr::mutate(titanic_alluvia, Index = 1:nrow(titanic_alluvia)),
  "Variable", "Value", axes = 1:4, factor_key = TRUE
))
titanic_lodes$Value <- factor(titanic_lodes$Value,
                              levels = do.call(c, lapply(titanic_alluvia[, 1:4],
                                                         levels)))
is_alluvial(titanic_lodes,
            key = "Variable", value = "Value", id = "Index",
            weight = "Freq",
            logical = FALSE)
