context("geom-lode")

# visual tests

test_that("`geom_lode` draws correctly", {
  d <- as.data.frame(Titanic)
  a1 <- aes(y = Freq, axis = Class)
  a2 <- aes(y = Freq, axis1 = Class, axis2 = Sex, axis3 = Age, fill = Survived)
  
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  suppressWarnings(vdiffr::expect_doppelganger(
    "`geom_lode`: one axis",
    ggplot(d, a1) + geom_lode(aes(fill = Class, alpha = Survived)) +
      scale_x_discrete(limits = c("Class"))
  ))
  vdiffr::expect_doppelganger(
    "`geom_lode`: lodes and alluvia",
    ggplot(d, a2) + geom_alluvium() + geom_lode()
  )
  vdiffr::expect_doppelganger(
    "`geom_lode`: lodes as strata",
    ggplot(d, a2) + geom_alluvium() + geom_stratum(stat = "alluvium")
  )
})
