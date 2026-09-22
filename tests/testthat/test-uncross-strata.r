# `uncross_strata_alluvia()` and `uncross_strata_lodes()` tests

data(majors)
data(vaccinations)

# `uncross_strata_alluvia()` tests

majors_wide <- to_alluvia_form(
  majors[c("student", "semester", "curriculum")],
  key = "semester", value = "curriculum", id = "student"
)

test_that("`uncross_strata_alluvia()` reorders axis variables", {
  set.seed(1)
  # FIXME: Keep non-axis columns by default.
  maj_sort <- uncross_strata_alluvia(
    majors_wide, axes = c("CURR1", "CURR7", "CURR13")
  )
  # returns data in alluvia form
  expect_true(is_alluvia_form(maj_sort,
                              axes = c("CURR1", "CURR7", "CURR13"),
                              silent = TRUE))
  # returns factors even for characters
  expect_all_false(vapply(
    majors_wide[, c("CURR1", "CURR7", "CURR13")],
    is.factor, FALSE
  ))
  expect_all_true(vapply(
    maj_sort[, c("CURR1", "CURR7", "CURR13")],
    is.factor, FALSE
  ))
  # keeps the `student` column and adds a `value` column
  expect_contains(names(maj_sort),
                  c("student", "CURR1", "CURR7", "CURR13", "value"))
  # missing values get `"Missing"` factor level
  # FIXME: Allow user to specify character level for missing values.
  for (ax in c("CURR1", "CURR7", "CURR13")) {
    levs <- levels(factor(majors_wide[[ax]]))
    if (anyNA(majors_wide[[ax]])) levs <- c(levs, "Missing")
    expect_setequal(levels(maj_sort[[ax]]), levs)
  }
})

test_that("`uncross_strata_alluvia()` handles weights", {
  vac_axes <- levels(factor(vaccinations$survey))
  vac_wide <- to_alluvia_form(
    vaccinations[c("subject", "survey", "response", "freq")],
    key = "survey", value = "response", id = "subject"
  )
  set.seed(1)
  vac_sort <- uncross_strata_alluvia(vac_wide, axes = vac_axes,
                                   weight = freq)
  expect_true(is_alluvia_form(vac_sort, axes = vac_axes, silent = TRUE))
  expect_contains(colnames(vac_sort), c(vac_axes, "freq"))
  for (ax in vac_axes) {
    expect_setequal(levels(vac_sort[[ax]]),
                    levels(factor(vac_wide[[ax]])))
  }
})

# `uncross_strata_lodes()` tests

test_that("`uncross_strata_lodes()` reorders stratum variable", {
  set.seed(1)
  maj_bf <- uncross_strata_lodes(majors,
                              key = semester, value = curriculum,
                              id = student,
                              stratum.guidance = "backfront")
  # original columns, rows, and id/key values are retained
  expect_identical(colnames(maj_bf), colnames(majors))
  expect_identical(maj_bf[c("student", "semester")],
                   majors[c("student", "semester")])
  # returns factors even for characters
  expect_false(is.factor(majors$curriculum))
  expect_true(is.factor(maj_bf$curriculum))
  # missing values get `"Missing"` factor level
  # FIXME: Allow user to specify character level for missing values.
  expect_setequal(levels(maj_bf$curriculum),
                  c(levels(factor(majors$curriculum)), "Missing"))
  # different `start`s yield different orders
  set.seed(1)
  maj_bf3 <- uncross_strata_lodes(majors,
                                  key = semester, value = curriculum,
                                  id = student,
                                  stratum.guidance = "backfront", start = 3)
  expect_false(identical(levels(maj_bf$curriculum), levels(maj_bf3$curriculum)))
})

test_that("`uncross_strata_lodes()` handles weights", {
  set.seed(1)
  vac_zig <- uncross_strata_lodes(vaccinations, method = "tsp",
                                  key = survey, value = response,
                                  id = subject, weight = freq,
                                  stratum.guidance = "zigzag")
  set.seed(1)
  vac_rank <- uncross_strata_lodes(vaccinations, method = "tsp",
                                   key = survey, value = response,
                                   id = subject, weight = freq,
                                   stratum.guidance = "mean_rank")
  expect_true(is_lodes_form(vac_zig,
                            key = "survey", value = "response", id = "subject",
                            weight = "freq", silent = TRUE))
  expect_setequal(levels(vac_zig$response),
                  levels(factor(vaccinations$response)))
  # different guidance functions yield different orders
  expect_false(identical(levels(vac_zig$response),
                         levels(vac_rank$response)))
})
