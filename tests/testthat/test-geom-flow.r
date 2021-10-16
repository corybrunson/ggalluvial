context("geom-flow")

# curve tests

test_that("`positions_to_flow` computes as expected", {
  # spline curve
  spline_curve <-
    positions_to_flow(1, 2, 0, 1, 1, 2, 1.3, 1.7, FALSE, "spline", NULL, NULL)
  expect_equal(nrow(spline_curve), 8L)
  expect_equal(spline_curve$x, c(1, 2.3, 0.3, 2, 2, 0.3, 2.3, 1))
  expect_equal(unique(spline_curve$y), c(0, 1, 2))
  expect_equal(unique(spline_curve$shape), c(0, 1))
  # cubic curve
  cubic_curve <-
    positions_to_flow(1, 2, 0, 1, 1, 2, 1.3, 1.7, FALSE, "cubic", NULL, 8L)
  expect_equal(nrow(cubic_curve), 2L * 8L + 2L)
  expect_equal(unique(cubic_curve$x), seq(1, 2, .125))
  expect_equal(unique(cubic_curve$shape), 0)
})

# visual tests

test_that("`geom_flow` draws correctly", {
  d <- as.data.frame(Titanic)
  a1 <- aes(y = Freq, axis1 = Class, axis2 = Sex, axis3 = Age)
  a2 <- aes(y = Freq, axis1 = Class, axis2 = Sex)
  
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(
    "`geom_flow`: basic",
    ggplot(d, a1) + geom_flow()
  )
  vdiffr::expect_doppelganger(
    "`geom_flow`: aesthetic",
    ggplot(d, a1) + geom_flow(aes(fill = Survived))
  )
  vdiffr::expect_doppelganger(
    "`geom_flow`: facets",
    ggplot(d, a2) +
      geom_flow(aes(fill = Age), width = .4) +
      facet_wrap(~ Survived, scales = "fixed")
  )
})

data(vaccinations)

test_that("`geom_flow` orients flows correctly", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(
    "`geom_flow`: forward orientation",
    ggplot(vaccinations,
           aes(x = survey, stratum = response, alluvium = subject,
               y = freq, fill = response)) +
      geom_lode() + geom_flow()
  )
  vdiffr::expect_doppelganger(
    "`geom_flow`: backward orientation",
    ggplot(vaccinations,
           aes(x = survey, stratum = response, alluvium = subject,
               y = freq, fill = response)) +
      geom_lode() + geom_flow(aes.flow = "backward")
  )
})

test_that("`geom_flow()` recognizes alternative curves", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(
    "`geom_flow`: unscaled knot positions",
    ggplot(vaccinations,
           aes(x = survey, stratum = response, alluvium = subject,
               y = freq, fill = response)) +
      geom_stratum() + geom_flow(knot.prop = FALSE)
  )
  vdiffr::expect_doppelganger(
    "`geom_flow`: 'linear' curve",
    ggplot(vaccinations,
           aes(x = survey, stratum = response, alluvium = subject,
               y = freq, fill = response)) +
      geom_stratum() + geom_flow(curve_type = "linear")
  )
  vdiffr::expect_doppelganger(
    "`geom_flow`: 'cubic' curve",
    ggplot(vaccinations,
           aes(x = survey, stratum = response, alluvium = subject,
               y = freq, fill = response)) +
      geom_stratum() + geom_flow(curve_type = "cubic")
  )
  vdiffr::expect_doppelganger(
    "`geom_flow`: 'quintic' curve",
    ggplot(vaccinations,
           aes(x = survey, stratum = response, alluvium = subject,
               y = freq, fill = response)) +
      geom_stratum() + geom_flow(curve_type = "quintic")
  )
  vdiffr::expect_doppelganger(
    "`geom_flow`: 'sine' curve",
    ggplot(vaccinations,
           aes(x = survey, stratum = response, alluvium = subject,
               y = freq, fill = response)) +
      geom_stratum() + geom_flow(curve_type = "sine")
  )
  vdiffr::expect_doppelganger(
    "`geom_flow`: 'arctangent' curve",
    ggplot(vaccinations,
           aes(x = survey, stratum = response, alluvium = subject,
               y = freq, fill = response)) +
      geom_stratum() + geom_flow(curve_type = "arctan")
  )
  vdiffr::expect_doppelganger(
    "`geom_flow`: 'arctangent' curve with custom range",
    ggplot(vaccinations,
           aes(x = survey, stratum = response, alluvium = subject,
               y = freq, fill = response)) +
      geom_stratum() + geom_flow(curve_type = "arctan", curve_range = 1)
  )
  vdiffr::expect_doppelganger(
    "`geom_flow`: 'sigmoid' curve",
    ggplot(vaccinations,
           aes(x = survey, stratum = response, alluvium = subject,
               y = freq, fill = response)) +
      geom_stratum() + geom_flow(curve_type = "sigmoid")
  )
  vdiffr::expect_doppelganger(
    "`geom_flow`: 'sigmoid' curve with custom range",
    ggplot(vaccinations,
           aes(x = survey, stratum = response, alluvium = subject,
               y = freq, fill = response)) +
      geom_stratum() + geom_flow(curve_type = "sigmoid", curve_range = 3)
  )
})
