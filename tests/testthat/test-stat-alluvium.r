context("stat-alluvium")

# weights are used but not returned

test_that("`stat_alluvium` weights computed variables but drops weight", {
  # not cementing alluvia
  data <- expand.grid(alluvium = letters[1:3], x = 1:2)
  data$stratum <- LETTERS[c(1, 1, 2, 1, 2, 2)]
  data$y <- c(1, 1, 1, 1, 1, 2)
  data$weight <- c(.5, 1, 1, .5, 1, 1)
  comp <- StatAlluvium$compute_panel(data)
  comp <- comp[with(comp, order(x, alluvium)), ]
  expect_equivalent(comp$n, c(0.5, 1, 1, 0.5, 1, 1))
  expect_equivalent(comp$count, c(0.5, 1, 1, 0.5, 1, 2))
  expect_equivalent(comp$prop, c(c(1, 2, 2) / 5, c(1, 2, 4) / 7))
  expect_equal(comp$lode, factor(rep(letters[1:3], times = 2)))
  expect_null(comp$weight)
  # cementing alluvia
  data$stratum <- LETTERS[c(1, 1, 2)]
  comp <- StatAlluvium$compute_panel(data, cement.alluvia = TRUE)
  comp <- comp[with(comp, order(x, alluvium)), ]
  expect_equivalent(comp$n, c(1.5, 1, 1.5, 1))
  expect_equivalent(comp$count, c(1.5, 1, 1.5, 2))
  expect_equivalent(comp$prop, c(c(3, 2) / 5, c(3, 4) / 7))
  expect_equal(comp$lode, rep(factor(letters[1:3])[c(1, 3)], times = 2))
  expect_null(comp$weight)
})

# negative values

test_that("`stat_alluvium` orders alluvia without regard to negative values", {
  data <- expand.grid(alluvium = letters[1:2], x = 1:2)
  data$stratum <- LETTERS[1]
  data$y <- c(-1, -1)
  # order by alluvium, `reverse = TRUE`
  #ggplot(data, aes(x = x, stratum = stratum, alluvium = alluvium, y = y)) +
  #  geom_alluvium() +
  #  geom_text(stat = "alluvium", aes(label = alluvium))
  comp <- StatAlluvium$compute_panel(data)
  expect_identical(comp[with(comp, order(x, alluvium)), ]$y,
                   c(-0.5, -1.5, -0.5, -1.5))
  # order by alluvium, `reverse = FALSE`
  #ggplot(data, aes(x = x, stratum = stratum, alluvium = alluvium, y = y)) +
  #  geom_alluvium(absolute = FALSE) +
  #  geom_text(stat = "alluvium", aes(label = alluvium), absolute = FALSE)
  comp <- StatAlluvium$compute_panel(data, absolute = FALSE)
  expect_identical(comp[with(comp, order(x, alluvium)), ]$y,
                   c(-0.5, -1.5, -0.5, -1.5))
})

# aesthetic binding

test_that("`stat_alluvium` orders alluvia correctly according to `aes.bind`", {
  data <- expand.grid(alluvium = letters[1:4], x = 1:2)
  data$stratum <- LETTERS[1:2][c(1, 1, 2, 2, 2, 2, 2, 1)]
  data$y <- 1
  data$fill <- c("red", "blue", "blue", "blue")
  # order by index strata, linked strata (flows), and alluvia
  #ggplot(data, aes(x = x, stratum = stratum, alluvium = alluvium, y = y)) +
  #  geom_alluvium(aes(fill = fill)) +
  #  geom_text(stat = "alluvium", aes(fill = fill, label = alluvium))
  comp <- StatAlluvium$compute_panel(data)
  expect_identical(comp[with(comp, order(x, alluvium)), ]$y,
                   c(3.5, 2.5, 0.5, 1.5, 2.5, 1.5, 0.5, 3.5))
  # order by index strata, linked strata (flows), aesthetics, and alluvia
  #ggplot(data, aes(x = x, stratum = stratum, alluvium = alluvium, y = y)) +
  #  geom_alluvium(aes(fill = fill), aes.bind = "flows") +
  #  geom_text(stat = "alluvium", aes(fill = fill, label = alluvium),
  #            aes.bind = "flows")
  comp <- StatAlluvium$compute_panel(data, aes.bind = "flows")
  expect_identical(comp[with(comp, order(x, alluvium)), ]$y,
                   c(2.5, 3.5, 0.5, 1.5, 1.5, 2.5, 0.5, 3.5))
  # order by index strata, aesthetics, linked strata (flows), and alluvia
  #ggplot(data, aes(x = x, stratum = stratum, alluvium = alluvium, y = y)) +
  #  geom_alluvium(aes(fill = fill), aes.bind = "alluvia") +
  #  geom_text(stat = "alluvium", aes(fill = fill, label = alluvium),
  #            aes.bind = "alluvia")
  comp <- StatAlluvium$compute_panel(data, aes.bind = "alluvia")
  expect_identical(comp[with(comp, order(x, alluvium)), ]$y,
                   c(2.5, 3.5, 0.5, 1.5, 0.5, 2.5, 1.5, 3.5))
})

# exceptional data

test_that("`stat_flow` handles exceptional data with out errors", {
  data(Refugees, package = "alluvial")
  refugees_sub <- subset(Refugees, year %in% c(2003, 2005, 2010, 2013))
  gg <- ggplot(refugees_sub, aes(x = year, y = refugees, alluvium = country)) +
    geom_alluvium(aes(fill = country))
  expect_silent(ggplot_build(gg))
})
