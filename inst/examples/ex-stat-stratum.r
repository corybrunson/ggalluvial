data(vaccinations)
# only `stratum` assignment is necessary to generate strata
ggplot(vaccinations,
       aes(y = freq,
           x = survey, stratum = response,
           fill = response)) +
  stat_stratum(width = .5)

# lode data, positioning with y labels
ggplot(vaccinations,
       aes(y = freq,
           x = survey, stratum = response, alluvium = subject,
           label = after_stat(count))) +
  stat_stratum(geom = "errorbar") +
  geom_text(stat = "stratum")
# alluvium data, positioning with stratum labels
ggplot(as.data.frame(Titanic),
       aes(y = Freq,
           axis1 = Class, axis2 = Sex, axis3 = Age, axis4 = Survived)) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  stat_stratum(geom = "errorbar") +
  scale_x_discrete(limits = c("Class", "Sex", "Age", "Survived"))

# omit labels for strata outside a y range
ggplot(vaccinations,
       aes(y = freq,
           x = survey, stratum = response,
           fill = response, label = response)) +
  stat_stratum(width = .5) +
  geom_text(stat = "stratum", min.y = 100)

# date-valued axis variables
ggplot(vaccinations,
       aes(x = end_date, y = freq, stratum = response, alluvium = subject,
           fill = response)) +
  stat_alluvium(geom = "flow", lode.guidance = "forward",
                width = 30) +
  stat_stratum(width = 30) +
  labs(x = "Survey date", y = "Number of respondents")

admissions <- as.data.frame(UCBAdmissions)
admissions <- transform(admissions, Count = Freq * (-1) ^ (Admit == "Rejected"))
# use negative y values to encode rejection versus acceptance
ggplot(admissions,
       aes(y = Count, axis1 = Dept, axis2 = Gender)) +
  geom_alluvium(aes(fill = Dept), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum)), min.y = 200) +
  scale_x_discrete(limits = c("Department", "Gender"), expand = c(.05, .05))
# computed variable 'deposit' indicates order of each signed stratum
ggplot(admissions,
       aes(y = Count, axis1 = Dept, axis2 = Gender)) +
  geom_alluvium(aes(fill = Dept), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_text(stat = "stratum", aes(label = after_stat(deposit)),
            color = "white") +
  scale_x_discrete(limits = c("Department", "Gender"), expand = c(.05, .05))
# fixed-width strata with acceptance and rejection totals
ggplot(admissions,
       aes(y = sign(Count), weight = Count, axis1 = Dept, axis2 = Gender)) +
  geom_alluvium(aes(fill = Dept), width = 1/8) +
  geom_stratum(width = 1/8, fill = "black", color = "grey") +
  geom_text(stat = "stratum",
            aes(label = paste0(stratum,
                               ifelse(nchar(as.character(stratum)) == 1L,
                                      ": ", "\n"),
                               after_stat(n))),
            color = "white", size = 3) +
  scale_x_discrete(limits = c("Department", "Gender"), expand = c(.05, .05))
