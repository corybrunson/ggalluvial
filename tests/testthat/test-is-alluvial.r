library(ggalluvial)
context("is_alluvial")

titanic_alluvia <- as.data.frame(Titanic)

test_that("is_alluvial recognizes alluvium-form Titanic data", {
  expect_error(is_alluvial(titanic_alluvia), "required")
  expect_true(is_alluvial(titanic_alluvia, axes = 1:4))
  expect_true(is_alluvial(titanic_alluvia, axes = c("Class", "Sex")))
  expect_false(is_alluvial(titanic_alluvia,
                           key = Class, value = Freq, id = Age))
})

titanic_lodes <- suppressWarnings(tidyr::gather(
  dplyr::mutate(titanic_alluvia, Index = 1:nrow(titanic_alluvia)),
  "Variable", "Value", axes = 1:4, factor_key = TRUE
))

test_that("is_alluvial recognizes lode-form Titanic data", {
  expect_error(is_alluvial(titanic_lodes), "required")
  expect_warning(is_alluvial(titanic_lodes,
                             key = Variable, value = Value, id = Index),
                 "weight")
  expect_true(is_alluvial(titanic_lodes,
                          key = Variable, value = Value, id = Index,
                          weight = Freq))
  #expect_false(is_alluvial(titanic_lodes, axes = 3:4))
})
