library(tidyverse)

#Load schedules
schedule_loaded <- nflreadr::load_schedules(2020:2022)

#Clean df down to only needed data
schedule_df <- schedule_loaded %>%
  #filter(game_type == 'REG') %>% didnt change results significantly
  select(season,spread_line, total_line, result, total)

#Add in whether parlay hits, insurance kicks in, or a miss
parlay_check <- schedule_df %>%
  mutate(sgp_hit = ifelse(abs(result - spread_line) < 10 & abs(total - total_line) < 10,
                          1,0),
         sgp_ins = ifelse((abs(result - spread_line) < 10 | abs(total - total_line < 10)) & sgp_hit == 0,
                          1,0),
         sgp_miss = ifelse(sgp_hit == 0 & sgp_ins == 0,1,0),
         category = case_when(sgp_hit == 1 ~ 'Hit',
                              sgp_ins == 1 ~ 'Free Bet',
                              sgp_miss == 1 ~ 'Miss',
                              TRUE ~ 'ERROR')) %>%
  filter(category != 'ERROR') #Remove the canceled 2022 week 17 game

#Organization to help with plotting
parlay_check$category <- factor(parlay_check$category, levels = c('Hit','Free Bet','Miss'))
my_colors <- c('green4','yellow2','red2')

#Plot the results
p_bar <- parlay_check %>% ggplot(aes(x = category)) +
  geom_bar(aes(y = (after_stat(count))/sum(after_stat(count))),
           fill = my_colors) +
  scale_y_continuous(breaks = seq(0,1,.1),
                     limits = c(0,1),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = '',
       y = '',
       title = 'Historical Results of One Game Parlay | NFL',
       subtitle = 'All NFL Games 2020-2022 | n = 833') +
  ggthemes::theme_clean() +
  theme(axis.text.x = element_text(face = 'bold', size = 14),
        plot.title = element_text(face = 'bold', size = 18, hjust = .5),
        plot.subtitle = element_text(size = 14, hjust = .5))

#Split up results by spread
spread_check <- parlay_check %>%
  mutate(abs_spread = abs(spread_line)) %>%
  mutate(abs_spread = ifelse(abs_spread > 14.0, 14.0, abs_spread)) %>%
  group_by(abs_spread) %>%
  summarise(spread_hit = mean(sgp_hit),
            spread_ins = mean(sgp_ins),
            spread_miss = mean(sgp_miss),
            tot = n())

#Plot the results by spread
p_spread <- spread_check %>% ggplot(aes(x = abs_spread)) +
  geom_line(aes(y = spread_hit, color = 'Hit'), linewidth = 1.5) +
  geom_line(aes(y = spread_ins,  color = 'Free Bet'), linewidth = 1.5) +
  geom_line(aes(y = spread_miss,  color = 'Miss'), linewidth = 1.5) +
  labs(x = 'Value of Point Spread',
       y = '',
       color = 'Result') +
  scale_color_manual(values = my_colors, breaks = c("Hit",'Free Bet','Miss')) +
  ggthemes::theme_clean() +
  scale_x_continuous(breaks = c(1,seq(2.5,12.5,2),14)) +
  scale_y_continuous(breaks = seq(0,1,.1),
                     limits = c(0,1),
                     labels = scales::percent_format(accuracy = 1)) +
  theme(plot.title = element_text(face = 'bold', size = 18, hjust = .5),
        plot.subtitle = element_text(size = 14, hjust = .5),
        legend.position = c(.44,.79),
        legend.key.size = unit(.5,'cm'),
        plot.background = element_rect(fill = 'white', color = 'white'))

#Split up the results by total
total_check <- parlay_check %>%
  mutate(total_line = ifelse(total_line < 36, 36, total_line),
         total_line = ifelse(total_line > 55.5, 55.5, total_line)) %>%
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
       color = 'Result') +
  scale_color_manual(values = my_colors, breaks = c("Hit",'Free Bet','Miss')) +
  ggthemes::theme_clean() +
  scale_x_continuous(breaks = c(seq(36,54,2),55.5)) +
  scale_y_continuous(breaks = seq(0,1,.1),
                     limits = c(0,1),
                     labels = scales::percent_format(accuracy = 1)) +
  theme(plot.title = element_text(face = 'bold', size = 18, hjust = .5),
        plot.subtitle = element_text(size = 14, hjust = .5),
        legend.position = 'none',
        plot.background = element_rect(fill = 'white', color = 'white'))

#Combine plots
combo <- ggpubr::ggarrange(p_spread, p_total, nrow = 2)
cowplot::ggdraw(combo) +
  theme(plot.title = element_text(size = 24, face = 'bold', hjust = .5),
        plot.caption = element_text(size = 14, face = 'bold', hjust = .5),
        plot.background = element_rect(fill = 'white', color = 'white')) +
  labs(title = 'Historical Results of One Game Parlay by Spread and Game Total| NFL', 
       caption = 'All NFL Games 2020-2022 | n = 833')

#Save Plots
ggsave('NFL_total_spread.png')
ggsave(plot = p_total, filename = 'nfl_by_total.png')

#Save NFL results for plotting all leagues
nfl_write_results <- data.frame(
  league = 'NFL',
  sgp_hit = mean(parlay_check$sgp_hit),
  sgp_ins = mean(parlay_check$sgp_ins),
  sgp_miss = mean(parlay_check$sgp_miss))

write.csv(nfl_write_results, 'nfl_results.csv', row.names = F)

#Calculate expected value
pct_hit <- mean(parlay_check$sgp_hit)
pct_ins <- mean(parlay_check$sgp_ins)
ev <- pct_hit*2.75*25 + 25*pct_ins*pct_hit*1.75
#ROI percentage
(ev - 25)/25*100

#Number of game days
game_days <- schedule_loaded %>%
  filter(season == 2022) %>%
  group_by(gameday) %>%
  summarise(total = n())
