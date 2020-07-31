context("alluvial-data")

titanic_alluvia <- as.data.frame(Titanic)
null_wt <- NULL

# `is_alluvia_form()` tests

test_that("`is_alluvia_form` recognizes alluvia-format Titanic data", {
  expect_message(is_alluvia_form(titanic_alluvia), "[Mm]issing")
  expect_true(is_alluvia_form(titanic_alluvia, axes = c("Class", "Sex")))
  expect_true(is_alluvia_form(titanic_alluvia, axes = 1:4))
  expect_true(is_alluvia_form(titanic_alluvia,
                              axes = c("Class", "Sex"), y = "Freq"))
  expect_true(is_alluvia_form(titanic_alluvia, axes = 1:4, y = 5))
  expect_true(is_alluvia_form(titanic_alluvia,
                              axes = c(Class, Sex, Age), y = Freq))
  expect_true(is_alluvia_form(titanic_alluvia,
                              axes = c(Class, Sex), y = !! null_wt))
})

# `to_lodes_form()` tests

test_that("`to_lodes_form` consistently formats Titanic data", {
  expect_equivalent(to_lodes_form(titanic_alluvia, axes = c("Class", "Sex")),
                    to_lodes_form(titanic_alluvia, axes = 1:2))
  expect_equivalent(to_lodes_form(titanic_alluvia, axes = c("Class", "Sex")),
                    to_lodes_form(titanic_alluvia, axes = c(Class, Sex)))
  expect_equivalent(to_lodes_form(titanic_alluvia, axes = c("Class", "Sex"),
                                  diffuse = "Class"),
                    to_lodes_form(titanic_alluvia, axes = 1:2,
                                  diffuse = 1))
  expect_equivalent(to_lodes_form(titanic_alluvia, axes = c("Class", "Sex"),
                                  diffuse = "Class"),
                    to_lodes_form(titanic_alluvia, axes = c(Class, Sex),
                                  diffuse = Class))
})

# preparation for next tests
titanic_lodes <- to_lodes_form(
  titanic_alluvia,
  alluvia_to = "Index", axes_to = "Variable", strata_to = "Value", axes = 1:4
)
titanic_lodes$Value <-
  factor(titanic_lodes$Value,
         levels = do.call(c, lapply(titanic_alluvia[, 1:4], levels)))

# `is_lodes_form()` tests

test_that("`is_lodes_form` recognizes lodes-format Titanic data", {
  expect_error(is_lodes_form(titanic_lodes))
  expect_true(is_lodes_form(titanic_lodes,
                            alluvia_from = "Index", axes_from = "Variable",
                            strata_from = "Value"))
  expect_true(is_lodes_form(titanic_lodes,
                            alluvia_from = Index, axes_from = Variable,
                            strata_from = Value))
  expect_true(is_lodes_form(titanic_lodes,
                            alluvia_from = 2, axes_from = 3, strata_from = 4))
  expect_true(is_lodes_form(titanic_lodes,
                            alluvia_from = "Index", axes_from = "Variable",
                            strata_from = "Value", y = "Freq"))
  expect_true(is_lodes_form(titanic_lodes,
                            alluvia_from = 2, axes_from = 3, strata_from = 4,
                            y = 1))
  expect_true(is_lodes_form(titanic_lodes,
                            alluvia_from = Index, axes_from = Variable,
                            strata_from = Value, y = Freq))
  expect_true(is_lodes_form(titanic_lodes,
                            alluvia_from = Index, axes_from = Variable,
                            strata_from = Value, y = !! null_wt))
})

# `to_alluvia_form()` tests

test_that("`to_alluvia_form` consistently formats Titanic data", {
  expect_equivalent(to_alluvia_form(titanic_lodes,
                                    alluvia_from = "Index",
                                    axes_from = "Variable",
                                    strata_from = "Value"),
                    to_alluvia_form(titanic_lodes,
                                    alluvia_from = 2,
                                    axes_from = 3,
                                    strata_from = 4))
  expect_equivalent(to_alluvia_form(titanic_lodes,
                                    alluvia_from = "Index",
                                    axes_from = "Variable",
                                    strata_from = "Value"),
                    to_alluvia_form(titanic_lodes,
                                    alluvia_from = Index,
                                    axes_from = Variable,
                                    strata_from = Value))
})
