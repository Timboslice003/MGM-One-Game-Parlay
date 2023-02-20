library(tidyverse)

#Load NBA schedule and clean
nba_pbp_loaded <- hoopR::load_nba_pbp(2023)
nba_23 <- nba_pbp_loaded %>%
  group_by(game_id, home_team_name, away_team_name, home_team_spread) %>%
  summarise(home_final = max(home_score),
            away_final = max(away_score),
            quarters = max(period)) %>%
  ungroup()

#Create loop to grab betting data
#It takes a long time to run this for loop
my_df <- c()
for (i in nba_23$game_id) {
  temp_game <- hoopR::espn_nba_betting(i)
  temp_ou <- temp_game$pickcenter$over_under[1]
  temp_book <- temp_game$pickcenter$provider_name[1]
  my_df <- rbind(my_df, c(i,temp_ou,temp_book))
  
}

#Organize data frame
my_df_complete <- as.tibble(my_df) %>%
  select(game_id = V1, total_line = V2) %>%
  mutate(game_id = as.integer(game_id),
         total_line = as.numeric(total_line))

#Join with original data frame and check OGP results
final_df <- nba_23 %>%
  left_join(my_df_complete, by = 'game_id') %>%
  filter(!is.na(total_line)) %>%
  mutate(total = home_final + away_final,
         result = home_final - away_final,
         sgp_hit = ifelse(abs(total_line - total) < 9.5 & abs(result - home_team_spread) < 9.5
                          ,1,0),
         sgp_ins = ifelse((abs(total_line - total) < 9.5 | abs(result - home_team_spread) < 9.5) & sgp_hit == 0
                          ,1,0))
#write.csv(final_df, file = '2023_1_18.csv', row.names = F)