nfl model
================
chad allison \| 1 december 2022

predictive model for nfl games - *work in progress, not even close to
done*

------------------------------------------------------------------------

### loading required packages

``` r
library(tidyverse)
library(nflreadr)

knitr::opts_chunk$set(message = F, warning = F)
options(nflreadr.verbose = F)
theme_set(theme_classic())
```

``` r
start_season = 2022
# write_csv(load_pbp(seasons = start_season:2022), "pbp_data.csv")
df = read_csv("pbp_data.csv", col_types = cols())
```

``` r
unique_games = df |>
  filter(season_type == "REG") |>
  count(game_id)

paste("number of regular season games in data:", nrow(unique_games))
```

    ## [1] "number of regular season games in data: 209"

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
       title = paste("yards per game by team each season since", start_season)) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))
```

![](nfl_model_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

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
  labs(x = NULL, y = "total yards", title = paste("yards per game by team since", start_season)) +
  theme(plot.title = element_text(hjust = 0.5))
```

![](nfl_model_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

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
       title = paste("win proportions for each team since", start_season)) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")
```

![](nfl_model_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
wl_df = wl_df |>
  mutate(win_prop = round(wins / (wins + losses + ties), 3))
```

``` r
df |>
  filter(season_type == "REG") |>
  filter(!is.na(posteam) & !is.na(yards_gained)) |>
  group_by(game_id, posteam) |>
  summarise(n = n(),
            yds = sum(yards_gained),
            .groups = "drop") |>
  ggplot(aes(n, yds)) +
  geom_point(aes(col = posteam), alpha = 0.5) +
  scale_color_manual(values = c(
    "#DD0000", "#B80000", "#6E3390", "#6D9BFF", "#79CAFF", "#000D5F", "#FF8A22",
    "#FF7800", "#002AAF", "#FF9803", "#26A6FF", "#076C00", "#001F93", "#001DA0",
    "#00B0B8", "#FF2121", "#0042FF", "#6CC5FF", "#838383", "#00CE61", "#AC34FF",
    "#001371", "#D6B458", "#0800FF", "#045B00", "#0A7200", "#F7FF00", "#53D200",
    "#BB0000", "#DA0000", "#003472", "#690A00")) +
  annotate("text", x = 105, y = 100, label = "i promise this is\ncooler with plotly")
```

![](nfl_model_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

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
       title = paste("relationship between yards per game and win percentage since", start_season)) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")
```

![](nfl_model_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
xyz = game_results |>
  left_join(team_stats, by = c("home_team" = "posteam")) |>
  select(-season) |>
  rename(home_ypg = ypg) |>
  left_join(team_stats, by = c("away_team" = "posteam")) |>
  select(-season) |>
  rename(away_ypg = ypg) |>
  filter(win_team != "tie") |>
  mutate(home_win = ifelse(win_team == "home", 1, 0))

win_mod = glm(home_win ~ home_ypg + away_ypg, data = xyz, family = "binomial")

predict_winner = function(home, away) {
  
  matchup = data.frame(home_team = home, away_team = away)
  
  matchup = matchup |>
    left_join(team_stats, by = c("home_team" = "posteam")) |>
    select(-season) |>
    rename(home_ypg = ypg) |>
    left_join(team_stats, by = c("away_team" = "posteam")) |>
    select(-season) |>
    rename(away_ypg = ypg)
  
  home_win_prob = predict(win_mod, matchup, type = "response")
  winner = ifelse(home_win_prob >= 0.5, home, away)
  loser = ifelse(home_win_prob >= 0.5, away, home)
  paste(winner, "will defeat", loser)
  
}

predict_winner("CHI", "PHI")
```

    ## [1] "PHI will defeat CHI"

``` r
predict_winner("MIN", "IND")
```

    ## [1] "MIN will defeat IND"

``` r
predict_winner("CLE", "BAL")
```

    ## [1] "CLE will defeat BAL"

``` r
predict_winner("BUF", "MIA")
```

    ## [1] "BUF will defeat MIA"

``` r
predict_winner("NYJ", "DET")
```

    ## [1] "DET will defeat NYJ"

``` r
predict_winner("CAR", "PIT")
```

    ## [1] "PIT will defeat CAR"

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

``` r
x = game_results |>
  left_join(team_stats, by = c("home_team" = "team")) |>
  rename(home_win_prop = win_prop,
         home_off_ypg = off_ypg,
         home_def_ypg = def_ypg) |>
  left_join(team_stats, by = c("away_team" = "team")) |>
  rename(away_win_prop = win_prop,
         away_off_ypg = off_ypg,
         away_def_ypg = def_ypg) |>
  filter(win_team != "tie") |>
  mutate(home_win = ifelse(win_team == "home", 1, 0))

win_mod = glm(home_win ~ home_win_prop + home_off_ypg + home_def_ypg +
                         away_win_prop + away_off_ypg + away_def_ypg,
              data = x, family = "binomial")

# need df with matchup stats

matchup = data.frame(home = "CHI", away = "PHI")

matchup = matchup |>
  left_join(team_stats, by = c("home" = "team")) |>
  rename(home_win_prop = win_prop,
         home_off_ypg = off_ypg,
         home_def_ypg = def_ypg) |>
  left_join(team_stats, by = c("away" = "team")) |>
  rename(away_win_prop = win_prop,
         away_off_ypg = off_ypg,
         away_def_ypg = def_ypg)

pick_winner = function(home, away) {
  
  matchup = data.frame(home = home, away = away)
  
  matchup = matchup |>
    left_join(team_stats, by = c("home" = "team")) |>
    rename(home_win_prop = win_prop,
           home_off_ypg = off_ypg,
           home_def_ypg = def_ypg) |>
    left_join(team_stats, by = c("away" = "team")) |>
    rename(away_win_prop = win_prop,
           away_off_ypg = off_ypg,
           away_def_ypg = def_ypg)
  
  prob = predict(win_mod, matchup, type = "response")
  winner = ifelse(prob >= 0.5, home, away)
  loser = ifelse(prob >= 0.5, away, home)
  location = ifelse(prob >= 0.5, "v.", "@")
  conf = ifelse(prob >= 0.5, prob, 1 - prob)
  return(paste0(winner, " will win ", location, " ", loser, " (", round(conf, 3), ")"))
  
}

pick_winner("CHI", "PHI")
```

    ## [1] "PHI will win @ CHI (0.96)"

``` r
pick_winner_return_team = function(home, away) {
  
  matchup = data.frame(home = home, away = away)
  
  matchup = matchup |>
    left_join(team_stats, by = c("home" = "team")) |>
    rename(home_win_prop = win_prop,
           home_off_ypg = off_ypg,
           home_def_ypg = def_ypg) |>
    left_join(team_stats, by = c("away" = "team")) |>
    rename(away_win_prop = win_prop,
           away_off_ypg = off_ypg,
           away_def_ypg = def_ypg)
  
  prob = predict(win_mod, matchup, type = "response")
  winner = ifelse(prob >= 0.5, home, away)
  loser = ifelse(prob >= 0.5, away, home)
  location = ifelse(prob >= 0.5, "v.", "@")
  conf = ifelse(prob >= 0.5, prob, 1 - prob)
  return(winner)
  
}
```

``` r
week15 = data.frame(home = c("SEA", "MIN", "CLE", "BUF", "CHI", "NYJ", "CAR", "HOU",
                    "NO", "JAX", "DEN", "LV", "LAC", "TB", "WAS", "GB"),
           away = c("SF", "IND", "BAL", "MIA", "PHI", "DET", "PIT", "KC",
                    "ATL", "DAL", "ARI", "NE", "TEN", "CIN", "NYG", "LA"))

week15 |>
  mutate(winner = pick_winner_return_team(home, away),
         msg = pick_winner(home, away))
```

    ##    home away winner                         msg
    ## 1   SEA   SF     SF   SF will win @ SEA (0.678)
    ## 2   MIN  IND    MIN MIN will win v. IND (0.918)
    ## 3   CLE  BAL    BAL  BAL will win @ CLE (0.765)
    ## 4   BUF  MIA    BUF BUF will win v. MIA (0.799)
    ## 5   CHI  PHI    PHI   PHI will win @ CHI (0.96)
    ## 6   NYJ  DET    NYJ  NYJ will win v. DET (0.66)
    ## 7   CAR  PIT    PIT  PIT will win @ CAR (0.529)
    ## 8   HOU   KC     KC   KC will win @ HOU (0.971)
    ## 9    NO  ATL    ATL   ATL will win @ NO (0.553)
    ## 10  JAX  DAL    DAL   DAL will win @ JAX (0.84)
    ## 11  DEN  ARI    DEN DEN will win v. ARI (0.501)
    ## 12   LV   NE     NE    NE will win @ LV (0.607)
    ## 13  LAC  TEN    TEN  TEN will win @ LAC (0.525)
    ## 14   TB  CIN    CIN   CIN will win @ TB (0.688)
    ## 15  WAS  NYG    WAS WAS will win v. NYG (0.525)
    ## 16   GB   LA     GB   GB will win v. LA (0.655)

``` r
res = x |>
  mutate(pred_winner = pick_winner_return_team(home_team, away_team),
         correct = case_when(home_win == 0 & pred_winner == away_team ~ 1,
                             home_win == 0 & pred_winner == home_team ~ 0,
                             home_win == 1 & pred_winner == home_team ~ 1,
                             home_win == 1 & pred_winner == away_team ~ 0)) |>
  count(correct) |>
  pull(n)

acc = round(res[2] / sum(res), 3)
paste("current model accuracy:", acc)
```

    ## [1] "current model accuracy: 0.739"
