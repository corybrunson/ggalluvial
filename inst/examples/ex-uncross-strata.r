if (requireNamespace("wompwomp", quietly = TRUE)) { # begin conditional exaples

# reorder axes of alluvia-form data
data(majors)
majors_alluvia <- to_alluvia_form(majors,
                                  key = "semester", value = "curriculum",
                                  id = "student")
set.seed(1)
majors_sorted <- uncross_strata_alluvia(majors_alluvia,
                                        axes = c("CURR1", "CURR7", "CURR13"))
# from characters to factors
is.character(majors_alluvia$CURR1)
levels(majors_sorted$CURR1)

# reorder strata of lodes-form data
set.seed(1)
majors_forward <- uncross_strata_lodes(majors,
                                       key = semester, value = curriculum,
                                       id = student,
                                       stratum.guidance = "forward")
levels(majors_forward$curriculum)
# sensitivity to guidance function
set.seed(1)
majors_mean_rank <- uncross_strata_lodes(majors,
                                         key = semester, value = curriculum,
                                         id = student,
                                         stratum.guidance = "mean_rank")
levels(majors_mean_rank$curriculum)

data(vaccinations)
vaccinations_zigzag <- uncross_strata_lodes(vaccinations,
                                            key = survey, value = response,
                                            id = subject, weight = freq,
                                            stratum.guidance = "zigzag")
vaccinations_zagzig <- uncross_strata_lodes(vaccinations,
                                            key = survey, value = response,
                                            id = subject, weight = freq,
                                            stratum.guidance = "zagzig")
ggplot(vaccinations,
       aes(x = survey, stratum = response, alluvium = subject, y = freq)) +
  stat_alluvium(alpha = .8) +
  stat_stratum(aes(fill = response)) +
  stat_stratum(geom = "text", aes(label = after_stat(stratum)), size = 3)
ggplot(vaccinations_zigzag,
       aes(x = survey, stratum = response, alluvium = subject, y = freq)) +
  stat_alluvium(alpha = .8) +
  stat_stratum(aes(fill = response)) +
  stat_stratum(geom = "text", aes(label = after_stat(stratum)), size = 3)
ggplot(vaccinations_zagzig,
       aes(x = survey, stratum = response, alluvium = subject, y = freq)) +
  stat_alluvium(alpha = .8) +
  stat_stratum(aes(fill = response)) +
  stat_stratum(geom = "text", aes(label = after_stat(stratum)), size = 3)

} # end conditional exaples
