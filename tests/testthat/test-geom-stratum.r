context("geom-stratum")

# visual tests

test_that("`geom_stratum` draws correctly", {
  d <- as.data.frame(Titanic)
  a1 <- aes(y = Freq, axis1 = Class, axis2 = Sex, axis3 = Age, axis4 = Survived)
  a2 <- aes(y = Freq, axis1 = Class, axis2 = Sex)

  skip_on_cran()
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(
    "`geom_stratum`: basic",
    ggplot(d, a1) + geom_stratum()
  )
  vdiffr::expect_doppelganger(
    "`geom_stratum`: extended width",
    ggplot(d, a1) + geom_stratum(width = 1)
  )
  vdiffr::expect_doppelganger(
    "`geom_stratum`: inferred text labels",
    ggplot(d, a1) +
      geom_text(stat = "stratum", aes(label = after_stat(stratum)))
  )
  vdiffr::expect_doppelganger(
    "`geom_stratum`: axis labels",
    ggplot(d, a1) + geom_stratum() +
      scale_x_discrete(limits = c("Class", "Sex", "Age", "Survived"))
  )
  vdiffr::expect_doppelganger(
    "`geom_stratum`: facets",
    ggplot(d, a2) + geom_stratum() +
      facet_wrap(~ Age, scales = "free_y")
  )
  vdiffr::expect_doppelganger(
    "`geom_stratum`: facets and axis labels",
    ggplot(d, a2) + geom_stratum() +
      scale_x_discrete(limits = c("Class", "Sex")) +
      facet_wrap(~ Age, scales = "free_y")
  )
})
