context("geom-alluvium")

# visual tests

test_that("`geom_alluvium` draws correctly", {
  d1 <- as.data.frame(Titanic)
  a1 <- aes(y = Freq, axis1 = Class, axis2 = Sex, axis3 = Age, fill = Survived)
  a2 <- aes(y = Freq, axis1 = Class, axis2 = Sex)
  d2 <- alluvial::Refugees
  a3 <- aes(y = refugees, x = year, alluvium = country)
  
  vdiffr::expect_doppelganger(
    "`geom_alluvium`: basic",
    ggplot(d1, a1) + geom_alluvium()
  )
  vdiffr::expect_doppelganger(
    "`geom_alluvium`: facets",
    ggplot(d1, a2) +
      geom_alluvium(aes(fill = Age), width = .4) +
      facet_wrap(~ Survived, scales = "fixed")
  )
  vdiffr::expect_doppelganger(
    "`geom_alluvium`: bump plot",
    ggplot(d2, a3) +
      geom_alluvium(aes(fill = country), width = 1/4, decreasing = FALSE)
  )
  vdiffr::expect_doppelganger(
    "`geom_alluvium`: line plot",
    ggplot(d2, a3) +
      geom_alluvium(aes(fill = country),  width = 0, knot.pos = 0)
  )
})
