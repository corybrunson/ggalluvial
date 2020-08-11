context("stat-flow")

# weights are used but not returned

test_that("`stat_flow` weights computed variables but drops weight", {
  data <- expand.grid(alluvium = letters[1:3], x = 1:2)
  data$stratum <- LETTERS[c(1, 1, 2, 1, 2, 2)]
  data$y <- c(1, 1, 1, 1, 1, 2)
  data$weight <- c(.5, 1, 1, .5, 1, 1)
  comp <- as.data.frame(StatFlow$compute_panel(data))
  comp <- comp[with(comp, order(x, alluvium)), ]
  expect_equivalent(comp$n, c(1, 1, 0.5, 1, 1, 0.5))
  expect_equivalent(comp$count, c(1, 1, 0.5, 2, 1, 0.5))
  expect_equivalent(comp$prop, c(c(2, 2, 1) / 5, c(4, 2, 1) / 7))
  expect_equal(comp$lode, rep(factor(letters[3:1]), times = 2))
  expect_null(comp$weight)
})

# reverse and absolute, negative values

test_that("`stat_flow` orders flows correctly with negative values", {
  data <- expand.grid(alluvium = letters[1:3], x = 1:2)
  data$stratum <- LETTERS[c(1, 1, 2)]
  data$y <- c(1, 1, 1, -1, 1, -1)
  # order by stratum, `reverse = TRUE`
  #ggplot(data, aes(x = x, stratum = stratum, alluvium = alluvium, y = y)) +
  #  geom_flow()
  comp <- StatFlow$compute_panel(data)
  expect_identical(comp[with(comp, order(x, alluvium)), ]$y,
                   c(1.5, 0.5, 2.5, -1.5, -0.5, 0.5))
  # order by stratum, `reverse = FALSE`
  #ggplot(data, aes(x = x, stratum = stratum, alluvium = alluvium, y = y)) +
  #  geom_flow(reverse = FALSE)
  comp <- StatFlow$compute_panel(data, reverse = FALSE)
  expect_identical(comp[with(comp, order(x, alluvium)), ]$y,
                   c(2.5, 0.5, 1.5, -1.5, -0.5, 0.5))
  # order by stratum, `reverse = FALSE, absolute = FALSE`
  #ggplot(data, aes(x = x, stratum = stratum, alluvium = alluvium, y = y)) +
  #  geom_flow(reverse = FALSE, absolute = FALSE)
  comp <- StatFlow$compute_panel(data, reverse = FALSE, absolute = FALSE)
  expect_identical(comp[with(comp, order(x, alluvium)), ]$y,
                   c(0.5, 2.5, 1.5, -1.5, -0.5, 0.5))
})

# aesthetic binding

test_that("`stat_flow` orders alluvia correctly according to `aes.bind`", {
  data <- expand.grid(alluvium = letters[1:4], x = 1:2)
  data$stratum <- LETTERS[c(1, 1, 1, 2, 2, 1, 3, 1)]
  data$y <- 1
  data$fill <- c("red", "blue", "blue", "blue",
                 "red", "blue", "blue", "blue")
  # order by index strata and linked strata (flows)
  #ggplot(data, aes(x = x, stratum = stratum, alluvium = alluvium, y = y)) +
  #  geom_flow(aes(fill = fill))
  comp <- StatFlow$compute_panel(data)
  expect_identical(comp[with(comp, order(x, alluvium)), ]$y,
                   c(2.5, 1.5, 0.5, 3.5, 1.5, 0.5, 2.5, 3.5))
  # order by index strata, linked strata (flows), and aesthetics
  #ggplot(data, aes(x = x, stratum = stratum, alluvium = alluvium, y = y)) +
  #  geom_flow(aes(fill = fill), aes.bind = "flows")
  comp <- StatFlow$compute_panel(data, aes.bind = "flows")
  expect_identical(comp[with(comp, order(x, alluvium)), ]$y,
                   c(1.5, 2.5, 0.5, 3.5, 1.5, 0.5, 2.5, 3.5))
  # cannot order by aesthetics before by linked strata (flows)
  expect_warning(StatFlow$compute_panel(data, aes.bind = "alluvia"), "flows")
})

# missing values

test_that("`stat_flow` preserves missingness to position flows", {
  data <- data.frame(x = c(1, 2, 2, 3),
                     stratum = factor(LETTERS[c(1L, 2L, 1L, 2L)]),
                     alluvium = c(1L, 2L, 1L, 2L),
                     PANEL = factor(1L),
                     group = seq(4L),
                     y = 1)
  comp <- StatFlow$compute_panel(data)
  expect_identical(sort(complete.cases(comp)), rep(c(FALSE, TRUE), c(2L, 4L)))
})

# exceptional data

test_that("`stat_flow` handles exceptional data with out errors", {
  wph <- as.data.frame(as.table(WorldPhones))
  names(wph) <- c("Year", "Region", "Telephones")
  wph$Year <- as.integer(as.character(wph$Year))
  gg <- ggplot(wph, aes(x = Year, alluvium = Region, y = Telephones)) +
    geom_flow(aes(fill = Region, colour = Region))
  expect_silent(ggplot_build(gg))
})
