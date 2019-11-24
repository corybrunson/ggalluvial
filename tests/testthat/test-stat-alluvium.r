context("stat-alluvium")

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
  # order by index strata, linked strata, and alluvium
  #ggplot(data, aes(x = x, stratum = stratum, alluvium = alluvium, y = y)) +
  #  geom_alluvium(aes(fill = fill)) +
  #  geom_text(stat = "alluvium", aes(fill = fill, label = alluvium))
  comp <- StatAlluvium$compute_panel(data)
  expect_identical(comp[with(comp, order(x, alluvium)), ]$y,
                   c(3.5, 2.5, 0.5, 1.5, 2.5, 1.5, 0.5, 3.5))
  # order by index strata, linked strata, aesthetics, and alluvium
  #ggplot(data, aes(x = x, stratum = stratum, alluvium = alluvium, y = y)) +
  #  geom_alluvium(aes(fill = fill), aes.bind = "linked") +
  #  geom_text(stat = "alluvium", aes(fill = fill, label = alluvium),
  #            aes.bind = "linked")
  comp <- StatAlluvium$compute_panel(data, aes.bind = "linked")
  expect_identical(comp[with(comp, order(x, alluvium)), ]$y,
                   c(2.5, 3.5, 0.5, 1.5, 1.5, 2.5, 0.5, 3.5))
  # order by index strata, aesthetics, linked strata, and alluvium
  #ggplot(data, aes(x = x, stratum = stratum, alluvium = alluvium, y = y)) +
  #  geom_alluvium(aes(fill = fill), aes.bind = "index") +
  #  geom_text(stat = "alluvium", aes(fill = fill, label = alluvium),
  #            aes.bind = "index")
  comp <- StatAlluvium$compute_panel(data, aes.bind = "index")
  expect_identical(comp[with(comp, order(x, alluvium)), ]$y,
                   c(2.5, 3.5, 0.5, 1.5, 0.5, 2.5, 1.5, 3.5))
})
