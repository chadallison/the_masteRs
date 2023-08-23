the masteRs
================

Data:
<a href="https://datagolf.com/raw-data-archive" target="_blank">Raw Data
Archive</a> via
<a href="https://datagolf.com/" target="_blank">datagolf.com</a>

------------------------------------------------------------------------

### Setup

<details>
<summary>
View Code
</summary>

``` r
library(tidyverse)
library(janitor)
library(tvthemes)
library(plotly)

theme_custom = theme_avatar() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 9, vjust = 2.5, face = "italic"),
        plot.caption = element_text(face = "italic"),
        panel.grid.major = element_line(linewidth = 0.5, colour = "#DFDAD1"),
        panel.grid.minor = element_line(linewidth = 0.5, colour = "#DFDAD1"))

theme_set(theme_custom)
```

</details>

### Data Import

<details>
<summary>
View Code
</summary>

``` r
df = read_csv("data/masters_data_2021.csv", show_col_types = F)
paste0("Data dimensions: ", nrow(df), " rows, ", ncol(df), " columns")
```

</details>

    ## [1] "Data dimensions: 284 rows, 29 columns"

### Scoring Gaps

Which players had the largest difference between their best and worst
scores for the tournament?

<details>
<summary>
View Code
</summary>

``` r
min_max_rounds = df |>
  group_by(player_name) |>
  summarise(min_round = min(round_score),
            max_round = max(round_score)) |>
  mutate(min_max_diff = max_round - min_round)

min_max_rounds |>
  slice_max(min_max_diff, n = 5) |>
  pivot_longer(c(min_round, max_round), names_to = "min_max", values_to = "score") |>
  ggplot(aes(reorder(player_name, -min_max_diff), score)) +
  geom_col(aes(fill = min_max), position = "dodge", show.legend = F) +
  geom_text(aes(label = score, group = min_max), position = position_dodge2(width = 0.9), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("#D17373", "#769773")) +
  labs(x = NULL, y = "Score",
       title = "Who had the biggest difference in their best and worst rounds?",
       subtitle = "2021 Masters Tournament") +
  theme(axis.text.y = element_blank())
```

</details>

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

**Plot Notes**

- Francesco Molinari shot 81 in round 4 and 69 in round 3
- Bernd Wiesberger shot 78 in round 4 and 66 in round 2
- Carlos Ortiz shot 82 in round 1 and 71 in round 2 (missed cut)
- Joe Long shot 82 in round 1 and 72 in round 2 (missed cut)
- Hudson Swafford shot 83 in round 2 and 73 in round 1 (missed cut)

------------------------------------------------------------------------

### Great Shots

Who hit the most great shots?

<details>
<summary>
View Code
</summary>

``` r
# cor_df = df |>
#   filter(!is.na(great_shots) & !is.na(round_score))
# 
# round(cor(cor_df$great_shots, cor_df$round_score), 3)

df |>
  filter(!is.na(great_shots)) |>
  group_by(player_name) |>
  summarise(great_shots = sum(great_shots)) |>
  slice_max(great_shots, n = 10) |>
  ggplot(aes(reorder(player_name, great_shots), great_shots)) +
  geom_col(fill = "#769773") +
  geom_text(aes(label = great_shots), size = 3.5, hjust = -0.4) +
  coord_flip() +
  labs(x = NULL, y = "Great Shots",
       title = "Who hit the most great shots?",
       subtitle = "2021 Masters Tournament") +
  theme(axis.text.x = element_blank())
```

</details>

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

**Plot Note**: The number of great shots a player hit had a -0.367
correlation coefficient with that player’s round score, meaning that as
players hit more great shots, their score decreased - just as we’d
expect!

------------------------------------------------------------------------

### Poor Shots

Who hit the most poor shots?

<details>
<summary>
View Code
</summary>

``` r
# cor_df = df |>
#   filter(!is.na(poor_shots) & !is.na(round_score))
# 
# round(cor(cor_df$poor_shots, cor_df$round_score), 3)

df |>
  filter(!is.na(poor_shots)) |>
  group_by(player_name) |>
  summarise(poor_shots = sum(poor_shots)) |>
  slice_max(poor_shots, n = 10) |>
  ggplot(aes(reorder(player_name, poor_shots), poor_shots)) +
  geom_col(fill = "#D17373") +
  geom_text(aes(label = poor_shots), size = 3.5, hjust = -0.4) +
  coord_flip() +
  labs(x = NULL, y = "Poor Shots",
       title = "Who hit the most poor shots?",
       subtitle = "2021 Masters Tournament") +
  theme(axis.text.x = element_blank())
```

</details>

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

**Plot Note**: The number of poor shots a player hit had a 0.592
correlation coefficient with that player’s round score, meaning that as
players hit more poor shots, their score increased - this is compared to
a -0.367 correlation coefficient between great shots and round score, so
the number of poor shots a player hit in a round was more impactful to
their score than the number of great shots was.
