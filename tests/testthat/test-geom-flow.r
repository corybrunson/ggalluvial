context("geom-flow")

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
