context("stat-stratum")

# weights are used but not returned

test_that("`stat_stratum` weights computed variables but drops weight", {
  data <- data.frame(x = rep(1:2, c(2, 3)), stratum = LETTERS[c(1, 2, 1, 2, 2)])
  data$y <- c(1, 1, 1, 1, 2)
  data$weight <- c(.5, 1, .5, 1, 1.5)
  comp <- StatStratum$compute_panel(data)
  comp <- comp[with(comp, order(x, stratum)), ]
  expect_equivalent(comp$n, c(0.5, 1, 0.5, 2.5))
  expect_equivalent(comp$count, c(0.5, 1, 0.5, 4))
  expect_equivalent(comp$prop, c(c(1, 2) / 3, c(1, 8) / 9))
  expect_null(comp$lode)
  expect_null(comp$weight)
})

# reverse and absolute parameters, negative values

test_that("`stat_stratum` orders strata correctly with negative values", {
  data <- expand.grid(stratum = LETTERS[1:2], x = 1:2)
  data$y <- c(1, 1, -1, -1)
  # order by stratum, `reverse = TRUE`
  #ggplot(data, aes(x = x, stratum = stratum, y = y)) +
  #  geom_stratum() +
  #  geom_text(stat = "stratum", aes(label = stratum))
  comp <- StatStratum$compute_panel(data)
  expect_identical(comp[with(comp, order(x, stratum)), ]$y,
                   c(1.5, 0.5, -1.5, -0.5))
  # order by stratum, `reverse = FALSE`
  #ggplot(data, aes(x = x, stratum = stratum, y = y)) +
  #  geom_stratum(reverse = FALSE) +
  #  geom_text(stat = "stratum", aes(label = stratum), reverse = FALSE)
  comp <- StatStratum$compute_panel(data, reverse = FALSE)
  expect_identical(comp[with(comp, order(x, stratum)), ]$y,
                   c(0.5, 1.5, -0.5, -1.5))
  # order by stratum, `absolute = FALSE`
  #ggplot(data, aes(x = x, stratum = stratum, y = y)) +
  #  geom_stratum(absolute = FALSE) +
  #  geom_text(stat = "stratum", aes(label = stratum), absolute = FALSE)
  comp <- StatStratum$compute_panel(data, absolute = FALSE)
  expect_identical(comp[with(comp, order(x, stratum)), ]$y,
                   c(1.5, 0.5, -0.5, -1.5))
  # order by stratum, `reverse = FALSE, absolute = FALSE`
  #ggplot(data, aes(x = x, stratum = stratum, y = y)) +
  #  geom_stratum(reverse = FALSE, absolute = FALSE) +
  #  geom_text(stat = "stratum", aes(label = stratum),
  #            reverse = FALSE, absolute = FALSE)
  comp <- StatStratum$compute_panel(data, reverse = FALSE, absolute = FALSE)
  expect_identical(comp[with(comp, order(x, stratum)), ]$y,
                   c(0.5, 1.5, -1.5, -0.5))
})
