library(ggalluvial)
context("alluvial-data")

titanic_alluvia <- as.data.frame(Titanic)

# updated `is_alluvia_form()` tests
test_that("`is_alluvia_form()` recognizes alluvia-format Titanic data", {
  expect_message(is_alluvia_form(titanic_alluvia), "[Mm]issing")
  expect_true(is_alluvia_form(titanic_alluvia, axes = 1:4))
  expect_true(is_alluvia_form(titanic_alluvia, axes = c("Class", "Sex")))
})

titanic_lodes <- suppressWarnings(tidyr::gather(
  dplyr::mutate(titanic_alluvia, Index = 1:nrow(titanic_alluvia)),
  "Variable", "Value", axes = 1:4, factor_key = TRUE
))
titanic_lodes$Value <- factor(titanic_lodes$Value,
                              levels = do.call(c, lapply(titanic_alluvia[, 1:4],
                                                         levels)))

# updated `is_alluvia_form()` tests
test_that("`is_lodes_form()` recognizes lodes-format Titanic data", {
  expect_error(is_lodes_form(titanic_lodes))
  expect_true(is_lodes_form(titanic_lodes,
                            key = Variable, value = Value, id = Index))
  expect_true(is_lodes_form(titanic_lodes,
                            key = "Variable", value = "Value", id = "Index",
                            weight = "Freq"))
})
