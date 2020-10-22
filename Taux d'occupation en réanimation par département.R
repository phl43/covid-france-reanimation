library(tidyverse)
library(utils)
library(httr)

# j'ai trouvé l'url du jeu de données, qui n'est pas listé sur data.gouv.fr, dans le code source du tableau
# de bord du gouvernement : https://github.com/etalab/covid19-dashboard/blob/master/prepare-data/index.js
url_données <- "https://www.data.gouv.fr/fr/datasets/r/4acad602-d8b1-4516-bc71-7d5574d5f33e"

GET(
  url_données,
  write_disk(tmp <- tempfile(fileext = ".csv"))
)

# le jeu de données contient une ligne par département, mais la donnée sur le taux d'occupation des unités de
# réanimation n'est en réalité disponible que par région, car le chiffre est systématiquement le même pour tous
# d'une même région
données <- read_csv(
  tmp
  ) %>%
  rename(
    date = extract_date,
    région = libelle_reg,
    taux = taux_occupation_sae
  ) %>%
  group_by(date, région) %>%
  summarize(taux = mean(taux)) %>%
  na.omit()

ggplot(données %>% slice_head(n = 9), aes(x = date, y = taux)) +
  geom_line(size = 1, color = "steelblue") +
  theme_bw() +
  ggtitle("Évolution du taux d'occupation des lits en réanimation par des patients atteint du COVID-19") +
  xlab("Date") +
  ylab("Taux d'occupation des lits en réanimation par des patients atteint du COVID-19") +
  scale_x_date(
    labels = scales::date_format("%d/%m/%Y"),
    date_breaks = "15 day"
  ) +
  scale_y_continuous(
    labels = function (x) paste0(x, "%")
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~ région, ncol = 3) +
  labs(caption = "Source : Santé Publique France - Graphique par Philippe Lemoine (@phl43)") +
  ggsave(
    "Évolution du taux d'occupation des lits en réanimation par des patients atteint du COVID-19 - 1.png",
    width = 18,
    height = 12,
    limitsize = FALSE
    )

ggplot(données %>% slice_tail(n = 9), aes(x = date, y = taux)) +
  geom_line(size = 1, color = "steelblue") +
  theme_bw() +
  ggtitle("Évolution du taux d'occupation des lits en réanimation par des patients atteint du COVID-19") +
  xlab("Date") +
  ylab("Taux d'occupation des lits en réanimation par des patients atteint du COVID-19") +
  scale_x_date(
    labels = scales::date_format("%d/%m/%Y"),
    date_breaks = "15 day"
  ) +
  scale_y_continuous(
    labels = function (x) paste0(x, "%")
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~ région, ncol = 3) +
  labs(caption = "Source : Santé Publique France - Graphique par Philippe Lemoine (@phl43)") +
  ggsave(
    "Évolution du taux d'occupation des lits en réanimation par des patients atteint du COVID-19 - 2.png",
    width = 18,
    height = 12,
    limitsize = FALSE
  )
