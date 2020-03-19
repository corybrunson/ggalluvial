context("stat-alluvium")

# weights are used but not returned

test_that("`stat_alluvium` weights computed variables but drops weight", {
  # not cementing alluvia
  data <- expand.grid(alluvium = letters[1:3], x = 1:2)
  data$stratum <- LETTERS[c(1, 1, 2, 1, 2, 2)]
  data$y <- c(1, 2, 1, 3, 1, 2)
  data$weight <- c(.5, 1, 2.5, 2, 1.5, 1)
  comp <- StatAlluvium$compute_panel(data)
  expect_equivalent(comp[with(comp, order(x, alluvium)), ]$n,
                    c(0.5, 1, 2.5, 2, 1.5, 1))
  expect_equivalent(comp[with(comp, order(x, alluvium)), ]$count,
                    c(0.5, 2, 2.5, 6, 1.5, 2))
  expect_equivalent(comp[with(comp, order(x, alluvium)), ]$prop,
                    c(1.9, 7.6, 9.5, 12, 3, 4) / 19)
  expect_null(comp$weight)
  # cementing alluvia
  data$stratum <- LETTERS[c(1, 1, 2)]
  comp <- StatAlluvium$compute_panel(data, cement.alluvia = TRUE)
  expect_equivalent(comp[with(comp, order(x, alluvium)), ]$n,
                    c(1.5, 2.5, 3.5, 1))
  expect_equivalent(comp[with(comp, order(x, alluvium)), ]$count,
                    c(2.5, 2.5, 7.5, 2))
  expect_equivalent(comp[with(comp, order(x, alluvium)), ]$prop,
                    c(9.5, 9.5, 15, 4) / 19)
  expect_null(comp$weight)
})

# negative values

test_that("`stat_alluvium` orders alluvia correctly with negative values", {
  data <- expand.grid(alluvium = letters[1:2], x = 1:2)
  data$stratum <- LETTERS[1]
  data$y <- c(-1, -1)
  # order by alluvium, `reverse = TRUE`
  #ggplot(data, aes(x = x, stratum = stratum, alluvium = alluvium, y = y)) +
  #  geom_alluvium() +
  #  geom_text(stat = "alluvium", aes(label = alluvium))
  comp <- StatAlluvium$compute_panel(data)
  expect_identical(comp[with(comp, order(x, alluvium)), ]$y,
                   c(-1.5, -0.5, -1.5, -0.5))
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
