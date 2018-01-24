library(ggalluvial)
context("is_alluvial")

titanic_alluvia <- as.data.frame(Titanic)

test_that("is_alluvial recognizes alluvia-format Titanic data", {
  expect_message(is_alluvial(titanic_alluvia),
                 "[Mm]issing")
  expect_true(is_alluvial(titanic_alluvia, axes = 1:4))
  expect_true(is_alluvial(titanic_alluvia, axes = c("Class", "Sex")))
  expect_warning(is_alluvial(titanic_alluvia,
                             key = "Class", value = "Freq", id = "Age"),
                 "[Dd]uplicate")
})

titanic_lodes <- suppressWarnings(tidyr::gather(
  dplyr::mutate(titanic_alluvia, Index = 1:nrow(titanic_alluvia)),
  "Variable", "Value", axes = 1:4, factor_key = TRUE
))
titanic_lodes$Value <- factor(titanic_lodes$Value,
                              levels = do.call(c, lapply(titanic_alluvia[, 1:4],
                                                         levels)))

test_that("is_alluvial recognizes lodes-format Titanic data", {
  expect_message(is_alluvial(titanic_lodes),
                 "[Mm]issing")
  expect_true(is_alluvial(titanic_lodes,
                          key = "Variable", value = "Value", id = "Index"))
  expect_true(is_alluvial(titanic_lodes,
                          key = "Variable", value = "Value", id = "Index",
                          weight = "Freq"))
  expect_message(is_alluvial(titanic_lodes, axes = 3:4),
                 "[Mm]issing")
})
