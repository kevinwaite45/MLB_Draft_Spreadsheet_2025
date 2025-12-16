library(readr)
library(sqldf)
library(conflicted)
conflict_prefer("mutate", "dplyr")

batting_stats <- read_csv('batting_stats.csv')
pitching_stats <- read_csv('pitching_stats.csv')
adp <- read_csv('MLB_ADP_2025.csv')

colnames(batting_stats)[1] <- c('player_name')
colnames(pitching_stats)[1] <- c('player_name')

bat_vars <- c('player_name','year','player_age','b_game','pa','woba','xwoba',
              'exit_velocity_avg','barrel_batted_rate','hard_hit_percent',
              'sweet_spot_percent','oz_swing_percent','whiff_percent','k_percent',
              'bb_percent','avg_swing_speed','avg_swing_length',
              'r_total_stolen_base','r_total_caught_stealing')
batting_stats <- batting_stats[bat_vars]
batting_stats<-subset(batting_stats,year>=2022)

bat_percentile_vars <- c('xwoba','exit_velocity_avg','barrel_batted_rate',
                         'hard_hit_percent','oz_swing_percent',
                         'whiff_percent','k_percent','bb_percent',
                         'avg_swing_speed','avg_swing_length')
pitch_percentile_vars <- c('xwoba','exit_velocity_avg','barrel_batted_rate',
                           'hard_hit_percent','oz_swing_percent',
                           'whiff_percent','k_percent','bb_percent',
                           'fastball_avg_speed','breaking_avg_spin')

batting_stats_w_percentiles <- batting_stats

for (var in bat_percentile_vars) {
  batting_stats_w_percentiles <- batting_stats_w_percentiles %>%
    group_by(year) %>%
    mutate(!!paste0("percentile_",var) := ifelse(pa > 50, percent_rank(!!sym(var)), NA))
}

pitching_stats_w_percentiles <- pitching_stats

for (var in pitch_percentile_vars) {
  pitching_stats_w_percentiles <- pitching_stats_w_percentiles %>%
    group_by(year) %>%
    mutate(!!paste0("percentile_",var) := ifelse(p_formatted_ip > 20 | p_game >= 10, percent_rank(!!sym(var)), NA))
}

df_final_1 <- bind_rows(batting_stats_w_percentiles,pitching_stats_w_percentiles)
df_final_1$player_name <- stringi::stri_trans_general(df_final_1$player_name, "Latin-ASCII")

df_final <- left_join(adp,df_final_1,by = "player_name",relationship = "many-to-many")

print(df_final %>%
  filter(is.na(year)),n=100)


write.csv(df_final, file = "MLB_Draft_Stats_2025.csv", row.names = FALSE)






