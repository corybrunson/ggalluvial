## ----setup---------------------------------------------------------------
knitr::opts_chunk$set(fig.width = 6, fig.height = 4, fig.align = "center")
library(ggalluvial)
data(vaccinations)
levels(vaccinations$response) <- rev(levels(vaccinations$response))

## ----raw-----------------------------------------------------------------
ggplot(vaccinations,
       aes(x = survey, stratum = response, alluvium = subject, y = freq,
           fill = response, label = response)) +
  scale_x_discrete(expand = c(.1, 0)) +
  geom_flow(width = 1/4) +
  geom_stratum(alpha = .5, width = 1/4) +
  geom_text(stat = "stratum", size = 4) +
  theme(legend.position = "none") +
  ggtitle("vaccination survey responses", "labeled using `geom_text()`")

## ----aesthetics----------------------------------------------------------
print(ggrepel::GeomTextRepel$required_aes)
print(ggfittext:::GeomFitText$required_aes)
print(ggfittext:::GeomFitText$setup_data)
print(StatStratum$compute_panel)

## ----ggrepel-------------------------------------------------------------
ggplot(vaccinations,
       aes(x = survey, stratum = response, alluvium = subject, y = freq,
           fill = response)) +
  scale_x_discrete(expand = c(.4, 0)) +
  geom_flow(width = 1/4) +
  geom_stratum(alpha = .5, width = 1/4) +
  scale_linetype_manual(values = c("blank", "solid")) +
  ggrepel::geom_text_repel(
    aes(label = ifelse(as.numeric(survey) == 1, as.character(response), NA)),
    stat = "stratum", size = 4, direction = "y", nudge_x = -.5
  ) +
  ggrepel::geom_text_repel(
    aes(label = ifelse(as.numeric(survey) == 3, as.character(response), NA)),
    stat = "stratum", size = 4, direction = "y", nudge_x = .5
  ) +
  theme(legend.position = "none") +
  ggtitle("vaccination survey responses", "labeled using `geom_text_repel()`")

## ----ggfittext-----------------------------------------------------------
ggplot(vaccinations,
       aes(x = survey, stratum = response, alluvium = subject, y = freq,
           fill = response, label = response)) +
  scale_x_discrete(expand = c(.1, 0)) +
  geom_flow(width = 1/4) +
  geom_stratum(alpha = .5, width = 1/4) +
  ggfittext::geom_fit_text(stat = "stratum", width = 1/4, min.size = 3) +
  theme(legend.position = "none") +
  ggtitle("vaccination survey responses", "labeled using `geom_fit_text()`")

## ----session info--------------------------------------------------------
sessioninfo::session_info()

