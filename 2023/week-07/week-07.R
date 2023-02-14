library(ggplot2)
library(ggstatsplot)
library(dplyr)

age_gaps <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-14/age_gaps.csv'
  )

age_gaps <- age_gaps |>
  mutate("differential" = if_else(
    character_1_gender == "woman",
    age_difference * -1,
    age_difference
  )) |>
  mutate("group" = if_else(release_year < 1965, 1, if_else(release_year < 1995, 2, 3))) |>
  mutate("decade" = paste(substr(release_year, 1, 3), "0s", sep = "")) |>
  filter(character_1_gender == "woman" |
           character_2_gender == "woman")


plt <- ggbetweenstats(
  data = age_gaps,
  x = decade,
  y = differential,
  palette = "Set3",
  pairwise.comparisons = FALSE
)

plt <- plt +
  labs(
    x = "Decade",
    y = "Age Difference",
    title = "Hollywood Age Gaps",
    subtitle = "Age difference between male and female love interests (years)",
    caption = "Source: https://hollywoodagegap.com/"
  )

plt