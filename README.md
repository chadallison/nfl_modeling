nfl model
================
chad allison

predictive model for nfl games (work in progress)

*current model accuracy: 77.93%*

------------------------------------------------------------------------

### loading required packages

<details>
<summary>
view code
</summary>

``` r
library(tidyverse)
library(nflreadr)

knitr::opts_chunk$set(message = F, warning = F)
options(nflreadr.verbose = F)
theme_set(theme_classic())
```

</details>
<details>
<summary>
view code
</summary>

``` r
start_season = 2022
write_csv(load_pbp(seasons = start_season:2022), "pbp_data.csv")
df = read_csv("pbp_data.csv", col_types = cols()) # this prevents message popup
head(df)
```

</details>

    ## # A tibble: 6 × 372
    ##   play_id game_id  old_g…¹ home_…² away_…³ seaso…⁴  week posteam poste…⁵ defteam
    ##     <dbl> <chr>      <dbl> <chr>   <chr>   <chr>   <dbl> <chr>   <chr>   <chr>  
    ## 1       1 2022_01…  2.02e9 NYJ     BAL     REG         1 <NA>    <NA>    <NA>   
    ## 2      43 2022_01…  2.02e9 NYJ     BAL     REG         1 NYJ     home    BAL    
    ## 3      68 2022_01…  2.02e9 NYJ     BAL     REG         1 NYJ     home    BAL    
    ## 4      89 2022_01…  2.02e9 NYJ     BAL     REG         1 NYJ     home    BAL    
    ## 5     115 2022_01…  2.02e9 NYJ     BAL     REG         1 NYJ     home    BAL    
    ## 6     136 2022_01…  2.02e9 NYJ     BAL     REG         1 NYJ     home    BAL    
    ## # … with 362 more variables: side_of_field <chr>, yardline_100 <dbl>,
    ## #   game_date <date>, quarter_seconds_remaining <dbl>,
    ## #   half_seconds_remaining <dbl>, game_seconds_remaining <dbl>,
    ## #   game_half <chr>, quarter_end <dbl>, drive <dbl>, sp <dbl>, qtr <dbl>,
    ## #   down <dbl>, goal_to_go <dbl>, time <time>, yrdln <chr>, ydstogo <dbl>,
    ## #   ydsnet <dbl>, desc <chr>, play_type <chr>, yards_gained <dbl>,
    ## #   shotgun <dbl>, no_huddle <dbl>, qb_dropback <dbl>, qb_kneel <dbl>, …

<details>
<summary>
view code
</summary>

``` r
unique_games = df |>
  filter(season_type == "REG") |>
  count(game_id)

paste("number of regular season games in data:", nrow(unique_games))
```

</details>

    ## [1] "number of regular season games in data: 224"

<details>
<summary>
view code
</summary>

``` r
team_stats = df |>
  filter(season_type == "REG") |>
  filter(!is.na(posteam) & !is.na(yards_gained)) |>
  group_by(game_id, posteam) |>
  summarise(n = n(),
            yds = sum(yards_gained),
            .groups = "drop") |>
  mutate(season = factor(substr(game_id, 1, 4))) |>
  group_by(posteam, season) |>
  summarise(ypg = round(mean(yds), 0),
            .groups = "drop")
```

</details>
<details>
<summary>
view code
</summary>

``` r
team_stats |>
  ggplot(aes(reorder(posteam, ypg), ypg)) +
  geom_col(position = "stack", width = 0.7, aes(fill = posteam)) +
  coord_flip() +
  scale_fill_manual(values = c(
    "#DD0000", "#B80000", "#6E3390", "#6D9BFF", "#79CAFF", "#000D5F", "#FF8A22",
    "#FF7800", "#002AAF", "#FF9803", "#26A6FF", "#076C00", "#001F93", "#001DA0",
    "#00B0B8", "#FF2121", "#0042FF", "#6CC5FF", "#838383", "#00CE61", "#AC34FF",
    "#001371", "#D6B458", "#0800FF", "#045B00", "#0A7200", "#F7FF00", "#53D200",
    "#BB0000", "#DA0000", "#003472", "#690A00")) +
  labs(x = NULL, y = "yards per game",
       title = paste("yards per game by team in", start_season)) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))
```

</details>

![](nfl_model_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

<details>
<summary>
view code
</summary>

``` r
df |>
  filter(season_type == "REG") |>
  filter(!is.na(posteam) & !is.na(yards_gained)) |>
  group_by(game_id, posteam) |>
  summarise(n = n(),
            yds = sum(yards_gained),
            .groups = "drop") |>
  # mutate(season = factor(substr(game_id, 1, 4))) |>
  ggplot(aes(reorder(posteam, yds), yds)) +
  geom_boxplot(aes(fill = posteam)) +
  coord_flip() +
  theme_classic() +
  scale_fill_manual(values = c(
    "#DD0000", "#B80000", "#6E3390", "#6D9BFF", "#79CAFF", "#000D5F", "#FF8A22",
    "#FF7800", "#002AAF", "#FF9803", "#26A6FF", "#076C00", "#001F93", "#001DA0",
    "#00B0B8", "#FF2121", "#0042FF", "#6CC5FF", "#838383", "#00CE61", "#AC34FF",
    "#001371", "#D6B458", "#0800FF", "#045B00", "#0A7200", "#F7FF00", "#53D200",
    "#BB0000", "#DA0000", "#003472", "#690A00")) +
  labs(x = NULL, y = "total yards", title = paste("yards per game by team in", start_season)) +
  theme(plot.title = element_text(hjust = 0.5))
```

</details>

![](nfl_model_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

<details>
<summary>
view code
</summary>

``` r
game_results = df |>
  filter(season_type == "REG" & desc == "END GAME") |>
  select(home_team, away_team, total_home_score, total_away_score) |>
  mutate(win_team = case_when(total_home_score > total_away_score ~ "home",
                              total_away_score > total_home_score ~ "away",
                              total_home_score == total_away_score ~ "tie"))

wl_df = data.frame(team = NULL, wins = NULL, losses = NULL, ties = NULL)

all_teams = game_results |>
  distinct(home_team) |>
  pull(home_team)

for (i in 1:length(all_teams)) {
  
  home_data = game_results |>
    filter(home_team == all_teams[i])
  
  away_data = game_results |>
    filter(away_team == all_teams[i])
  
  home_wins = home_data |>
    filter(win_team == "home") |>
    nrow()
  
  away_wins = away_data |>
    filter(win_team == "away") |>
    nrow()
  
  home_losses = home_data |>
    filter(win_team == "away") |>
    nrow()
  
  away_losses = away_data |>
    filter(win_team == "home") |>
    nrow()
  
  home_ties = home_data |>
    filter(win_team == "tie") |>
    nrow()
  
  away_ties = away_data |>
    filter(win_team == "tie") |>
    nrow()
  
  new_data = data.frame(team = all_teams[i],
                        wins = home_wins + away_wins,
                        losses = home_losses + away_losses,
                        ties = home_ties + away_ties)
  
  wl_df = rbind(wl_df, new_data)
  
}

wl_df |>
  mutate(win_prop = round(wins / (wins + losses + ties), 3)) |>
  ggplot(aes(reorder(team, win_prop), win_prop)) +
  geom_col(aes(fill = team)) +
  scale_fill_manual(values = c(
    "#DD0000", "#B80000", "#6E3390", "#6D9BFF", "#79CAFF", "#000D5F", "#FF8A22",
    "#FF7800", "#002AAF", "#FF9803", "#26A6FF", "#076C00", "#001F93", "#001DA0",
    "#00B0B8", "#FF2121", "#0042FF", "#6CC5FF", "#838383", "#00CE61", "#AC34FF",
    "#001371", "#D6B458", "#0800FF", "#045B00", "#0A7200", "#F7FF00", "#53D200",
    "#BB0000", "#DA0000", "#003472", "#690A00")) +
  coord_flip() +
  labs(x = NULL, y = "win proportion",
       title = paste("win proportions for each team in", start_season)) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")
```

</details>

![](nfl_model_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

<details>
<summary>
view code
</summary>

``` r
wl_df = wl_df |>
  mutate(win_prop = round(wins / (wins + losses + ties), 3))
```

</details>
<details>
<summary>
view code
</summary>

``` r
team_ypg = df |>
  filter(season_type == "REG") |>
  filter(!is.na(posteam) & !is.na(yards_gained)) |>
  group_by(game_id, posteam) |>
  summarise(n = n(),
            yds = sum(yards_gained),
            .groups = "drop") |>
  group_by(posteam) |>
  summarise(ypg = mean(yds)) |>
  rename(team = posteam)

wl_df |>
  mutate(win_prop = round(wins / (wins + losses + ties), 3)) |>
  left_join(team_ypg, by = "team") |>
  ggplot(aes(ypg, win_prop)) +
  geom_point(aes(col = team), size = 3) +
  geom_line(stat = "smooth", method = "lm", formula = y ~ x, linewidth = 1, linetype = "dashed", alpha = 0.25) +
  ggrepel::geom_text_repel(aes(x = ypg, 
                      y = win_prop, 
                      label = team),
                      size = 3) +
  scale_color_manual(values = c(
    "#DD0000", "#B80000", "#6E3390", "#6D9BFF", "#79CAFF", "#000D5F", "#FF8A22",
    "#FF7800", "#002AAF", "#FF9803", "#26A6FF", "#076C00", "#001F93", "#001DA0",
    "#00B0B8", "#FF2121", "#0042FF", "#6CC5FF", "#838383", "#00CE61", "#AC34FF",
    "#001371", "#D6B458", "#0800FF", "#045B00", "#0A7200", "#F7FF00", "#53D200",
    "#BB0000", "#DA0000", "#003472", "#690A00")) +
  labs(x = "yards per game", y = "win percentage", col = NULL,
       title = paste("relationship between yards per game and win percentage in", start_season)) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")
```

</details>

![](nfl_model_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

<details>
<summary>
view code
</summary>

``` r
# adding win_prop to team_stats
team_stats = wl_df |>
  select(team, win_prop) |>
  right_join(team_stats, by = c("team" = "posteam")) |>
  select(-season)

team_stats = df |>
  filter(season_type == "REG") |>
  filter(!is.na(defteam) & !is.na(yards_gained)) |>
  group_by(game_id, defteam) |>
  summarise(n = n(),
            yds = sum(yards_gained),
            .groups = "drop") |>
  mutate(season = factor(substr(game_id, 1, 4))) |>
  group_by(defteam, season) |>
  summarise(ypg = round(mean(yds), 0),
            .groups = "drop") |>
  select(defteam, ypg) |>
  right_join(team_stats, by = c("defteam" = "team")) |>
  rename(off_ypg = ypg.y,
         def_ypg = ypg.x,
         team = defteam) |>
  select(team, win_prop, off_ypg, def_ypg)
```

</details>
<details>
<summary>
view code
</summary>

``` r
new_gr = game_results |>
  left_join(wl_df, by = c("home_team" = "team")) |>
  rename(home_win_prop = win_prop) |>
  select(-c(wins, losses, ties)) |>
  left_join(wl_df, by = c("away_team" = "team")) |>
  rename(away_win_prop = win_prop) |>
  select(-c(wins, losses, ties))

head(new_gr) # use this to make the record above .500 column
```

</details>

    ## # A tibble: 6 × 7
    ##   home_team away_team total_home_score total_away_score win_team home_…¹ away_…²
    ##   <chr>     <chr>                <dbl>            <dbl> <chr>      <dbl>   <dbl>
    ## 1 NYJ       BAL                      9               24 away       0.5     0.643
    ## 2 LA        BUF                     10               31 away       0.286   0.786
    ## 3 CAR       CLE                     24               26 away       0.357   0.429
    ## 4 SEA       DEN                     17               16 home       0.5     0.286
    ## 5 MIN       GB                      23                7 home       0.786   0.429
    ## 6 HOU       IND                     20               20 tie        0.071   0.286
    ## # … with abbreviated variable names ¹​home_win_prop, ²​away_win_prop

<details>
<summary>
view code
</summary>

``` r
margins_df = data.frame(team = all_teams, margin = NA)

for (i in 1:nrow(margins_df)) {
  
  team = margins_df$team[i]
  
  home_margin = new_gr |>
    filter(home_team == team) |>
    mutate(margin = total_home_score - total_away_score) |>
    pull(margin) |>
    sum()
  
  away_margin = new_gr |>
    filter(away_team == team) |>
    mutate(margin = total_away_score - total_home_score) |>
    pull(margin) |>
    sum()
  
  margins_df$margin[i] = home_margin + away_margin
  
}

wl_df |>
  left_join(margins_df, by = "team") |>
  ggplot(aes(win_prop, margin)) +
  geom_point(aes(col = team)) +
  geom_line(stat = "smooth", method = "lm", formula = y ~ x, linewidth = 1, linetype = "dashed", alpha = 0.25) +
  ggrepel::geom_text_repel(aes(x = win_prop, 
                      y = margin, 
                      label = team),
                      size = 3) +
  scale_color_manual(values = c(
    "#DD0000", "#B80000", "#6E3390", "#6D9BFF", "#79CAFF", "#000D5F", "#FF8A22",
    "#FF7800", "#002AAF", "#FF9803", "#26A6FF", "#076C00", "#001F93", "#001DA0",
    "#00B0B8", "#FF2121", "#0042FF", "#6CC5FF", "#838383", "#00CE61", "#AC34FF",
    "#001371", "#D6B458", "#0800FF", "#045B00", "#0A7200", "#F7FF00", "#53D200",
    "#BB0000", "#DA0000", "#003472", "#690A00")) +
  labs(x = "win percentage", y = "overall point differential",
       title = "point differentials by team") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")
```

</details>

![](nfl_model_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

<details>
<summary>
view code
</summary>

``` r
# updating team_stats with margins
team_stats = team_stats |>
  left_join(margins_df, by = "team")
```

</details>
<details>
<summary>
view code
</summary>

``` r
all_teams = sort(all_teams)

df500 = data.frame(team = all_teams, wp500 = NA)

for (i in 1:nrow(df500)) {
  
  x = new_gr |>
    filter(home_team == df500$team[i] & away_win_prop >= 0.5) |>
    mutate(home_win = ifelse(win_team == "home", 1, 0)) |>
    summarise(x = sum(home_win),
              n = n())
  
  h500w = x |> pull(x)
  h500n = x |> pull(n)
  
  x = new_gr |>
    filter(away_team == df500$team[i] & home_win_prop >= 0.5) |>
    mutate(away_win = ifelse(win_team == "away", 1, 0)) |>
    summarise(x = sum(away_win),
              n = n())
  
  a500w = x |> pull(x)
  a500n = x |> pull(n)
  
  df500$wp500[i] = round((h500w + a500w) / (h500n + a500n), 3)
  
  
}

# adding win percentage v. .500 or better teams to team_stats
team_stats = team_stats |>
  left_join(df500, by = "team")
```

</details>
<details>
<summary>
view code
</summary>

``` r
# create home and away win percentages
home_away_wp = data.frame(team = all_teams, home_wp = NA, away_wp = NA)
for (i in 1:nrow(home_away_wp)) {
  
  home_wp = new_gr |>
    filter(home_team == home_away_wp$team[i]) |>
    mutate(home_win = ifelse(win_team == "home", 1, 0)) |>
    summarise(x = sum(home_win),
              n = n()) |>
    transmute(p = x / n) |>
    pull(p) |>
    round(3)
  
  away_wp = new_gr |>
    filter(away_team == home_away_wp$team[i]) |>
    mutate(away_win = ifelse(win_team == "away", 1, 0)) |>
    summarise(x = sum(away_win),
              n = n()) |>
    transmute(p = x / n) |>
    pull(p) |>
    round(3)
  
  home_away_wp$home_wp[i] = home_wp
  home_away_wp$away_wp[i] = away_wp
  
}

# updating team_stats with home and away win percentages
team_stats = team_stats |>
  left_join(home_away_wp, by = "team")
```

</details>
<details>
<summary>
view code
</summary>

``` r
# add third and fourth down rates
# and ytg on third down
# penalties

off_ytg3 = df |>
  filter(down == 3 & !play_type %in% c("no_play", "punt", "field_goal", "qb_kneel") & !is.na(play_type)) |>
  group_by(posteam) |>
  summarise(ytg3 = round(mean(ydstogo), 3))

off_conv3 = df |>
  filter(down == 3 & !play_type %in% c("no_play", "punt", "field_goal", "qb_kneel") & !is.na(play_type)) |>
  transmute(posteam, ydstogo, yards_gained,
            converted = ifelse(yards_gained >= ydstogo, 1, 0))  |>
  group_by(posteam) |>
  summarise(conv3 = round(sum(converted) / n(), 3))

def_ytg3 = df |>
  filter(down == 3 & !play_type %in% c("no_play", "punt", "field_goal", "qb_kneel") & !is.na(play_type)) |>
  group_by(defteam) |>
  summarise(ytg3 = round(mean(ydstogo), 3))

def_conv3 = df |>
  filter(down == 3 & !play_type %in% c("no_play", "punt", "field_goal", "qb_kneel") & !is.na(play_type)) |>
  transmute(defteam, ydstogo, yards_gained,
            converted = ifelse(yards_gained >= ydstogo, 1, 0))  |>
  group_by(defteam) |>
  summarise(conv3 = round(sum(converted) / n(), 3))

df_3rd_down = off_ytg3 |>
  left_join(off_conv3, by = "posteam") |>
  rename(off_ytg3 = ytg3,
         off_conv3 = conv3) |>
  left_join(def_ytg3, by = c("posteam" = "defteam")) |>
  rename(def_ytg3 = ytg3) |>
  left_join(def_conv3, by = c("posteam" = "defteam")) |>
  rename(def_conv3 = conv3,
         team = posteam)

rm(off_ytg3, off_conv3, def_ytg3, def_conv3)
    
df_3rd_down |>
  ggplot(aes(reorder(team, off_conv3), off_conv3)) +
  geom_col(aes(fill = team)) +
  coord_flip() +
  labs(x = NULL, y = "conversion rate", fill = NULL,
       title = "third down offensive conversion rates") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c(
    "#DD0000", "#B80000", "#6E3390", "#6D9BFF", "#79CAFF", "#000D5F", "#FF8A22",
    "#FF7800", "#002AAF", "#FF9803", "#26A6FF", "#076C00", "#001F93", "#001DA0",
    "#00B0B8", "#FF2121", "#0042FF", "#6CC5FF", "#838383", "#00CE61", "#AC34FF",
    "#001371", "#D6B458", "#0800FF", "#045B00", "#0A7200", "#F7FF00", "#53D200",
    "#BB0000", "#DA0000", "#003472", "#690A00"))
```

</details>

![](nfl_model_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

<details>
<summary>
view code
</summary>

``` r
df_3rd_down |>
  ggplot(aes(reorder(team, -def_conv3), def_conv3)) +
  geom_col(aes(fill = team)) +
  coord_flip() +
  labs(x = NULL, y = "conversion rate", fill = NULL,
       title = "third down defensive conversion rates") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c(
    "#DD0000", "#B80000", "#6E3390", "#6D9BFF", "#79CAFF", "#000D5F", "#FF8A22",
    "#FF7800", "#002AAF", "#FF9803", "#26A6FF", "#076C00", "#001F93", "#001DA0",
    "#00B0B8", "#FF2121", "#0042FF", "#6CC5FF", "#838383", "#00CE61", "#AC34FF",
    "#001371", "#D6B458", "#0800FF", "#045B00", "#0A7200", "#F7FF00", "#53D200",
    "#BB0000", "#DA0000", "#003472", "#690A00"))
```

</details>

![](nfl_model_files/figure-gfm/unnamed-chunk-31-2.png)<!-- -->

<details>
<summary>
view code
</summary>

``` r
df_3rd_down |>
  transmute(team, conv = off_conv3 - def_conv3) |>
  ggplot(aes(reorder(team, conv), conv)) +
  geom_col(aes(fill = team)) +
  coord_flip() +
  labs(x = NULL, y = "difference in conversion rates", fill = NULL,
       title = "difference in offensive and defensive third down conversion rates") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c(
    "#DD0000", "#B80000", "#6E3390", "#6D9BFF", "#79CAFF", "#000D5F", "#FF8A22",
    "#FF7800", "#002AAF", "#FF9803", "#26A6FF", "#076C00", "#001F93", "#001DA0",
    "#00B0B8", "#FF2121", "#0042FF", "#6CC5FF", "#838383", "#00CE61", "#AC34FF",
    "#001371", "#D6B458", "#0800FF", "#045B00", "#0A7200", "#F7FF00", "#53D200",
    "#BB0000", "#DA0000", "#003472", "#690A00"))
```

</details>

![](nfl_model_files/figure-gfm/unnamed-chunk-31-3.png)<!-- -->

<details>
<summary>
view code
</summary>

``` r
off_conv4 = df |>
  filter(down == 4 & !play_type %in% c("no_play", "punt", "field_goal", "qb_kneel") & !is.na(play_type)) |>
  select(posteam, ydstogo, yards_gained) |>
  mutate(converted = ifelse(yards_gained >= ydstogo, 1, 0)) |>
  group_by(posteam) |>
  summarise(off_conv4 = round(sum(converted) / n(), 3))

def_conv4 = df |>
  filter(down == 4 & !play_type %in% c("no_play", "punt", "field_goal", "qb_kneel") & !is.na(play_type)) |>
  select(defteam, ydstogo, yards_gained) |>
  mutate(converted = ifelse(yards_gained >= ydstogo, 1, 0)) |>
  group_by(defteam) |>
  summarise(def_conv4 = round(sum(converted) / n(), 3))

df_4th_down = off_conv4 |>
  left_join(def_conv4, by = c("posteam" = "defteam")) |>
  rename(team = posteam)

rm(off_conv4, def_conv4)

df_4th_down |>
  ggplot(aes(reorder(team, off_conv4), off_conv4)) +
  geom_col(aes(fill = team)) +
  coord_flip() +
  scale_fill_manual(values = c(
    "#DD0000", "#B80000", "#6E3390", "#6D9BFF", "#79CAFF", "#000D5F", "#FF8A22",
    "#FF7800", "#002AAF", "#FF9803", "#26A6FF", "#076C00", "#001F93", "#001DA0",
    "#00B0B8", "#FF2121", "#0042FF", "#6CC5FF", "#838383", "#00CE61", "#AC34FF",
    "#001371", "#D6B458", "#0800FF", "#045B00", "#0A7200", "#F7FF00", "#53D200",
    "#BB0000", "#DA0000", "#003472", "#690A00")) +
      labs(x = NULL, y = "conversion rate", fill = NULL,
           title = "fourth down offensive conversion rates") +
  theme(plot.title = element_text(hjust = 0.5))
```

</details>

![](nfl_model_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

<details>
<summary>
view code
</summary>

``` r
df_4th_down |>
  ggplot(aes(reorder(team, -def_conv4), def_conv4)) +
  geom_col(aes(fill = team)) +
  coord_flip() +
  scale_fill_manual(values = c(
    "#DD0000", "#B80000", "#6E3390", "#6D9BFF", "#79CAFF", "#000D5F", "#FF8A22",
    "#FF7800", "#002AAF", "#FF9803", "#26A6FF", "#076C00", "#001F93", "#001DA0",
    "#00B0B8", "#FF2121", "#0042FF", "#6CC5FF", "#838383", "#00CE61", "#AC34FF",
    "#001371", "#D6B458", "#0800FF", "#045B00", "#0A7200", "#F7FF00", "#53D200",
    "#BB0000", "#DA0000", "#003472", "#690A00")) +
      labs(x = NULL, y = "conversion rate", fill = NULL,
           title = "fourth down defensive conversion rates") +
  theme(plot.title = element_text(hjust = 0.5))
```

</details>

![](nfl_model_files/figure-gfm/unnamed-chunk-32-2.png)<!-- -->

<details>
<summary>
view code
</summary>

``` r
df_4th_down |>
  mutate(conv = off_conv4 - def_conv4) |>
  ggplot(aes(reorder(team, conv), conv)) +
  geom_col(aes(fill = team)) +
  coord_flip() +
  scale_fill_manual(values = c(
    "#DD0000", "#B80000", "#6E3390", "#6D9BFF", "#79CAFF", "#000D5F", "#FF8A22",
    "#FF7800", "#002AAF", "#FF9803", "#26A6FF", "#076C00", "#001F93", "#001DA0",
    "#00B0B8", "#FF2121", "#0042FF", "#6CC5FF", "#838383", "#00CE61", "#AC34FF",
    "#001371", "#D6B458", "#0800FF", "#045B00", "#0A7200", "#F7FF00", "#53D200",
    "#BB0000", "#DA0000", "#003472", "#690A00")) +
      labs(x = NULL, y = "difference in conversion rates", fill = NULL,
           title = "difference in offensive and defensive fourth down conversion rates") +
  theme(plot.title = element_text(hjust = 0.5))
```

</details>

![](nfl_model_files/figure-gfm/unnamed-chunk-32-3.png)<!-- -->

<details>
<summary>
view code
</summary>

``` r
df_3and4 = df_3rd_down |>
  left_join(df_4th_down, by = "team")

team_stats = team_stats |>
  left_join(df_3and4, by = "team")

names(team_stats)
```

</details>

    ##  [1] "team"      "win_prop"  "off_ypg"   "def_ypg"   "margin"    "wp500"    
    ##  [7] "home_wp"   "away_wp"   "off_ytg3"  "off_conv3" "def_ytg3"  "def_conv3"
    ## [13] "off_conv4" "def_conv4"

<details>
<summary>
view code
</summary>

``` r
# updating model
x = game_results |>
  left_join(team_stats, by = c("home_team" = "team")) |>
  rename(home_win_prop = win_prop, home_off_ypg = off_ypg, home_def_ypg = def_ypg,
         home_margin = margin, home_wp500 = wp500, home_home_wp = home_wp,
         home_off_ytg3 = off_ytg3, home_off_conv3 = off_conv3,
         home_def_ytg3 = def_ytg3, home_def_conv3 = def_conv3,
         home_off_conv4 = off_conv4, home_def_conv4 = def_conv4) |>
  select(-away_wp) |>
  left_join(team_stats, by = c("away_team" = "team")) |>
  rename(away_win_prop = win_prop, away_off_ypg = off_ypg, away_def_ypg = def_ypg,
         away_margin = margin, away_wp500 = wp500, away_away_wp = away_wp,
         away_off_ytg3 = off_ytg3, away_off_conv3 = off_conv3,
         away_def_ytg3 = def_ytg3, away_def_conv3 = def_conv3,
         away_off_conv4 = off_conv4, away_def_conv4 = def_conv4) |>
  select(-home_wp) |>
  filter(win_team != "tie") |>
  mutate(home_win = ifelse(win_team == "home", 1, 0))

win_mod = glm(home_win ~ home_win_prop + home_off_ypg + home_def_ypg + home_margin + home_wp500 + home_home_wp +
                         home_off_ytg3 + home_off_conv3 + home_def_ytg3 + home_def_conv3 +
                         home_off_conv4 + home_def_conv4 +
                         away_win_prop + away_off_ypg + away_def_ypg + away_margin + away_wp500 + away_away_wp +
                         away_off_ytg3 + away_off_conv3 + away_def_ytg3 + away_def_conv3 +
                         away_off_conv4 + away_def_conv4,
              data = x, family = "binomial") # current accuracy is higher without wp500 variables
```

</details>
<details>
<summary>
view code
</summary>

``` r
pick_winner = function(home, away) {
  
  matchup = data.frame(home = home, away = away)
  
  matchup = matchup |>
    left_join(team_stats, by = c("home" = "team")) |>
    rename(home_win_prop = win_prop, home_off_ypg = off_ypg, home_def_ypg = def_ypg,
         home_margin = margin, home_wp500 = wp500, home_home_wp = home_wp,
         home_off_ytg3 = off_ytg3, home_off_conv3 = off_conv3,
         home_def_ytg3 = def_ytg3, home_def_conv3 = def_conv3,
         home_off_conv4 = off_conv4, home_def_conv4 = def_conv4) |>
    select(-away_wp) |>
    left_join(team_stats, by = c("away" = "team")) |>
    rename(away_win_prop = win_prop, away_off_ypg = off_ypg, away_def_ypg = def_ypg,
         away_margin = margin, away_wp500 = wp500, away_away_wp = away_wp,
         away_off_ytg3 = off_ytg3, away_off_conv3 = off_conv3,
         away_def_ytg3 = def_ytg3, away_def_conv3 = def_conv3,
         away_off_conv4 = off_conv4, away_def_conv4 = def_conv4) |>
    select(-home_wp)
  
  prob = predict(win_mod, matchup, type = "response")
  winner = ifelse(prob >= 0.5, home, away)
  loser = ifelse(prob >= 0.5, away, home)
  location = ifelse(prob >= 0.5, "v.", "@")
  conf = ifelse(prob >= 0.5, prob, 1 - prob)
  return(paste0(winner, " will win ", location, " ", loser, " (", round(conf, 3), ")"))
  
}

pick_winner_return_team = function(home, away) {
  
  matchup = data.frame(home = home, away = away)
  
  matchup = matchup |>
    left_join(team_stats, by = c("home" = "team")) |>
    rename(home_win_prop = win_prop, home_off_ypg = off_ypg, home_def_ypg = def_ypg,
         home_margin = margin, home_wp500 = wp500, home_home_wp = home_wp,
         home_off_ytg3 = off_ytg3, home_off_conv3 = off_conv3,
         home_def_ytg3 = def_ytg3, home_def_conv3 = def_conv3,
         home_off_conv4 = off_conv4, home_def_conv4 = def_conv4) |>
    select(-away_wp) |>
    left_join(team_stats, by = c("away" = "team")) |>
    rename(away_win_prop = win_prop, away_off_ypg = off_ypg, away_def_ypg = def_ypg,
         away_margin = margin, away_wp500 = wp500, away_away_wp = away_wp,
         away_off_ytg3 = off_ytg3, away_off_conv3 = off_conv3,
         away_def_ytg3 = def_ytg3, away_def_conv3 = def_conv3,
         away_off_conv4 = off_conv4, away_def_conv4 = def_conv4) |>
    select(-home_wp)
  
  prob = predict(win_mod, matchup, type = "response")
  winner = ifelse(prob >= 0.5, home, away)
  loser = ifelse(prob >= 0.5, away, home)
  location = ifelse(prob >= 0.5, "v.", "@")
  conf = ifelse(prob >= 0.5, prob, 1 - prob)
  return(winner)
  
}
```

</details>
<details>
<summary>
view code
</summary>

``` r
acc = game_results |>
  filter(win_team != "tie") |>
  mutate(winner = ifelse(win_team == "home", home_team, away_team),
         pred = pick_winner_return_team(home_team, away_team),
         correct = ifelse(winner == pred, 1, 0)) |>
  summarise(acc = round(sum(correct) / n(), 4) * 100) |>
  pull(acc)

paste0("current model accuracy: ", acc, "%")
```

</details>

    ## [1] "current model accuracy: 77.93%"

<details>
<summary>
view code
</summary>

``` r
df |>
  select(game_id, home_team, away_team) |>
  right_join(game_results, by = c("home_team", "away_team")) |>
  distinct() |>
  mutate(winner = ifelse(win_team == "home", home_team, away_team),
         pred = pick_winner_return_team(home_team, away_team),
         correct = ifelse(winner == pred, "correct prediction", "wrong prediction"),
         bar = 1) |>
  ggplot(aes(game_id, bar)) +
  geom_col(aes(fill = correct), width = 1) +
  scale_fill_manual(values = c("#B6E0B8", "#FFC4C4")) +
  labs(x = NULL, y = NULL, fill = NULL,
       title = "game predictions over course of 2022 season",
       subtitle = "← beginning of season | today →") +
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 9))
```

</details>

![](nfl_model_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->

<details>
<summary>
view code
</summary>

``` r
week16 = data.frame(home = c("NYJ", "BAL", "CAR", "KC", "CLE", "TEN", "NE", "MIN", "CHI", "SF", "DAL", "PIT", "MIA", "LA", "ATL", "IND"),
                    away = c("JAX", "ATL", "DET", "SEA", "NO", "HOU", "CIN", "NYG", "BUF", "WAS", "PHI", "LV", "GB", "DEN", "TB", "LAC"))

preds = week16 |>
  mutate(pred = pick_winner(home, away)) |>
  pull(pred)

for (i in 1:length(preds)) {
  print(preds[i])
}
```

</details>

    ## [1] "NYJ will win v. JAX (0.691)"
    ## [1] "BAL will win v. ATL (0.944)"
    ## [1] "DET will win @ CAR (0.693)"
    ## [1] "KC will win v. SEA (0.859)"
    ## [1] "CLE will win v. NO (0.93)"
    ## [1] "TEN will win v. HOU (0.861)"
    ## [1] "CIN will win @ NE (0.707)"
    ## [1] "MIN will win v. NYG (0.642)"
    ## [1] "BUF will win @ CHI (0.944)"
    ## [1] "SF will win v. WAS (0.727)"
    ## [1] "PHI will win @ DAL (0.675)"
    ## [1] "PIT will win v. LV (0.563)"
    ## [1] "MIA will win v. GB (0.944)"
    ## [1] "LA will win v. DEN (0.673)"
    ## [1] "ATL will win v. TB (0.631)"
    ## [1] "LAC will win @ IND (0.901)"

**at this point in time these are the model predictors**

- home team win percentage
- home team offensive ypg
- home team defensive ypg
- home team point differential
- home team win percentage v. teams with .500 or better win percentage
- home team home win percentage
- home team offensive yards to go on third down
- home team offensive third down conversion rate
- home team defensive yards to go on third down
- home team defensive third down conversion rate
- home team offensive fourth down conversion rate
- home team defensive fourth down conversion rate
- away team win percentage
- away team offensive ypg
- away team defensive ypg
- away team point differential
- away team win percentage v. teams with .500 or better win percentage
- away team away win percentage
- away team offensive yards to go on third down
- away team offensive third down conversion rate
- away team defensive yards to go on third down
- away team defensive third down conversion rate
- away team offensive fourth down conversion rate
- away team defensive fourth down conversion rate
