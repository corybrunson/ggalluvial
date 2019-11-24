context("geom-flow")

# visual tests

test_that("`geom_flow` draws correctly", {
  d <- as.data.frame(Titanic)
  a1 <- aes(y = Freq, axis1 = Class, axis2 = Sex, axis3 = Age)
  a2 <- aes(y = Freq, axis1 = Class, axis2 = Sex)
  
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

test_that("`geom_flow` orients flows correctly", {
  data(vaccinations)
  
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
