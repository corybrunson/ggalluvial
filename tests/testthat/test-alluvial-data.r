context("alluvial-data")

titanic_alluvia <- as.data.frame(Titanic)
null_wt <- NULL

# `is_alluvia_form()` tests

test_that("`is_alluvia_form` recognizes alluvia-format Titanic data", {
  expect_message(is_alluvia_form(titanic_alluvia), "[Mm]issing")
  expect_true(is_alluvia_form(titanic_alluvia, axes = c("Class", "Sex")))
  expect_true(is_alluvia_form(titanic_alluvia, axes = 1:4))
  expect_true(is_alluvia_form(titanic_alluvia, Class, Sex, Age))
  expect_true(is_alluvia_form(titanic_alluvia, axes = c("Class", "Sex"),
                              weight = "Freq"))
  expect_true(is_alluvia_form(titanic_alluvia, axes = 1:4, weight = 5))
  expect_true(is_alluvia_form(titanic_alluvia, Class, Sex, Age, weight = Freq))
  expect_true(is_alluvia_form(titanic_alluvia, Class, Sex, weight = !!null_wt))
})

# `to_lodes_form()` tests

test_that("`to_lodes_form` consistently formats Titanic data", {
  expect_equivalent(to_lodes_form(titanic_alluvia, axes = c("Class", "Sex")),
                    to_lodes_form(titanic_alluvia, axes = 1:2))
  expect_equivalent(to_lodes_form(titanic_alluvia, axes = c("Class", "Sex")),
                    to_lodes_form(titanic_alluvia, Class, Sex))
  expect_equivalent(to_lodes_form(titanic_alluvia, axes = c("Class", "Sex"),
                                  diffuse = "Class"),
                    to_lodes_form(titanic_alluvia, axes = 1:2,
                                  diffuse = 1))
  expect_equivalent(to_lodes_form(titanic_alluvia, axes = c("Class", "Sex"),
                                  diffuse = "Class"),
                    to_lodes_form(titanic_alluvia, Class, Sex,
                                  diffuse = Class))
})

# preparation for next tests
titanic_lodes <- suppressWarnings(to_lodes_form(
  transform(titanic_alluvia, Index = 1:nrow(titanic_alluvia)),
  key = "Variable", value = "Value", id = "Index", axes = 1:4,
  factor_key = TRUE
))
titanic_lodes$Value <-
  factor(titanic_lodes$Value,
         levels = do.call(c, lapply(titanic_alluvia[, 1:4], levels)))

# `is_lodes_form()` tests

test_that("`is_lodes_form` recognizes lodes-format Titanic data", {
  expect_error(is_lodes_form(titanic_lodes))
  expect_true(is_lodes_form(titanic_lodes,
                            key = "Variable", value = "Value", id = "Index"))
  expect_true(is_lodes_form(titanic_lodes,
                            key = Variable, value = Value, id = Index))
  expect_true(is_lodes_form(titanic_lodes,
                            key = 3, value = 4, id = 2))
  expect_true(is_lodes_form(titanic_lodes,
                            key = "Variable", value = "Value", id = "Index",
                            weight = "Freq"))
  expect_true(is_lodes_form(titanic_lodes,
                            key = 3, value = 4, id = 2,
                            weight = 1))
  expect_true(is_lodes_form(titanic_lodes,
                            key = Variable, value = Value, id = Index,
                            weight = Freq))
  expect_true(is_lodes_form(titanic_lodes,
                            key = Variable, value = Value, id = Index,
                            weight = !!null_wt))
})

# `to_alluvia_form()` tests

test_that("`to_alluvia_form` consistently formats Titanic data", {
  expect_equivalent(to_alluvia_form(titanic_lodes,
                                    key = "Variable", value = "Value",
                                    id = "Index"),
                    to_alluvia_form(titanic_lodes,
                                    key = 3, value = 4, id = 2))
  expect_equivalent(to_alluvia_form(titanic_lodes,
                                    key = "Variable", value = "Value",
                                    id = "Index"),
                    to_alluvia_form(titanic_lodes,
                                    key = Variable, value = Value, id = Index))
})
