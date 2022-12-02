nfl model
================
chad allison \| 1 december 2022

predictive model for nfl games

------------------------------------------------------------------------

### loading required packages

``` r
library(tidyverse)
library(nflreadr)

knitr::opts_chunk$set(message = F, warning = F)
options(nflreadr.verbose = F)
```

``` r
load_pbp(seasons = 2020:2022) |>
  filter(!is.na(yards_gained) & !is.na(posteam)) |>
  # filter(between(yards_gained, -10, 50)) |>
  ggplot(aes(reorder(posteam, yards_gained), yards_gained)) +
  geom_boxplot(aes(fill = posteam)) +
  coord_flip() +
  theme_classic()
```

![](nfl_model_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->
