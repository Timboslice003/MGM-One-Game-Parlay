library(tidyverse)

#Load NHL data
nhl_loaded <- read.csv('nhl_game_data.csv')

#Manipulate data for analysis
nhl_clean <- nhl_loaded %>%
  filter(home_ml != away_ml) %>%
  mutate(total_score = home_score + away_score,
         favorite_score = ifelse(away_ml < home_ml, away_score, home_score),
         underdog_score = ifelse(away_ml > home_ml, away_score, home_score),
         min_total = ifelse(total_line <= 6, 3.5, 4.5),
         max_total = ifelse(total_line <= 6, 7.5, 8.5)) %>%
  select(favorite_score, underdog_score, min_total, max_total, total_score, total_line) %>%
  mutate(sgp_hit = ifelse(favorite_score - underdog_score < 4 & underdog_score - favorite_score < 3
                          & total_score >= min_total & total_score <= max_total, 1, 0),
         sgp_ins = ifelse(((favorite_score - underdog_score < 4 & underdog_score - favorite_score < 3)
                          | total_score >= min_total & total_score <= max_total) & sgp_hit != 1,1,0),
         sgp_miss = ifelse(sgp_hit != 1 & sgp_ins != 1, 1, 0),
         category = case_when(sgp_hit == 1 ~ 'Hit',
                              sgp_ins == 1 ~ 'Free Bet',
                              sgp_miss == 1 ~ 'Miss'))
  

#Organization to help with plotting
nhl_clean$category <- factor(nhl_clean$category, levels = c('Hit','Free Bet','Miss'))
my_colors <- c('green4','yellow2','red2')

#Plot the results
p_bar <- nhl_clean %>% ggplot(aes(x = category)) +
  geom_bar(aes(y = (after_stat(count))/sum(after_stat(count))),
           fill = my_colors) +
  scale_y_continuous(breaks = seq(0,1,.1),
                     limits = c(0,1),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = '',
       y = '',
       title = 'Historical Results of One Game Parlay | NHL',
       subtitle = '2022-2023 NHL games prior to 2/1/23 | n = 776') +
  ggthemes::theme_clean() +
  theme(axis.text.x = element_text(face = 'bold', size = 14),
        plot.title = element_text(face = 'bold', size = 18, hjust = .5),
        plot.subtitle = element_text(size = 14, hjust = .5))


#Split up the results by total
total_check <- nhl_clean %>%
  group_by(total_line) %>%
  summarise(total_hit = mean(sgp_hit),
            total_ins = mean(sgp_ins),
            total_miss = mean(sgp_miss),
            tot = n())

#Plot the results by total
p_total <- total_check %>% ggplot(aes(x = total_line)) +
  geom_line(aes(y = total_hit, color = 'Hit'), linewidth = 1.5) +
  geom_line(aes(y = total_ins,  color = 'Free Bet'), linewidth = 1.5) +
  geom_line(aes(y = total_miss,  color = 'Miss'), linewidth = 1.5) +
  labs(x = 'Game Total',
       y = '',
       color = 'Result',
       title = 'Historical Results of One Game Parlay | NHL',
       subtitle = '2022-2023 NHL games prior to 2/1/23 | n = 776') +
  scale_color_manual(values = my_colors, breaks = c("Hit",'Free Bet','Miss')) +
  ggthemes::theme_clean() +
  scale_x_continuous(breaks = c(seq(5.5,7,.5))) +
  scale_y_continuous(breaks = seq(0,1,.1),
                     limits = c(0,1),
                     labels = scales::percent_format(accuracy = 1)) +
  theme(plot.title = element_text(face = 'bold', size = 18, hjust = .5),
        plot.subtitle = element_text(size = 14, hjust = .5),
        legend.position = c(.5,.85),
        plot.background = element_rect(fill = 'white', color = 'white'))

#Save Plots
ggsave(plot = p_total, "NHL_total.png")
ggsave(plot = p_bar, 'NHL_hit_pct.png')

#Save NHL results for plotting all leagues
nhl_write_results <- data.frame(
  league = 'NHL',
  sgp_hit = mean(nhl_clean$sgp_hit),
  sgp_ins = mean(nhl_clean$sgp_ins),
  sgp_miss = mean(nhl_clean$sgp_miss))

write.csv(nhl_write_results, 'nhl_results.csv', row.names = F)

