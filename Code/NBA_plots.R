library(tidyverse)

#Load schedules
nba_loaded <- read.csv('2023_1_18.csv')

#Clean df down to only needed data
nba_clean <- nba_loaded %>%
  select(home_team_spread, total_line, result, total, sgp_hit, sgp_ins) %>%
  mutate(sgp_miss = ifelse(sgp_ins == 0 & sgp_hit == 0, 1, 0),
         category = case_when(sgp_hit == 1 ~ 'Hit',
                              sgp_ins == 1 ~ 'Free Bet',
                              sgp_miss == 1 ~ 'Miss',
                              TRUE ~ 'ERROR'))

#Organization to help with plotting
nba_clean$category <- factor(nba_clean$category, levels = c('Hit','Free Bet','Miss'))
my_colors <- c('green4','yellow2','red2')

#Plot the results
p_bar <- nba_clean %>% ggplot(aes(x = category)) +
  geom_bar(aes(y = (after_stat(count))/sum(after_stat(count))),
           fill = my_colors) +
  scale_y_continuous(breaks = seq(0,1,.1),
                     limits = c(0,1),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = '',
       y = '',
       title = 'Historical Results of One Game Parlay | NBA',
       subtitle = '2022-2023 NBA games prior to 2/1/23 | n = 668') +
  ggthemes::theme_clean() +
  theme(axis.text.x = element_text(face = 'bold', size = 14),
        plot.title = element_text(face = 'bold', size = 18, hjust = .5),
        plot.subtitle = element_text(size = 14, hjust = .5))

#Split up results by spread
spread_check <- nba_clean %>%
  mutate(abs_spread = abs(home_team_spread),
         abs_spread = ifelse(abs_spread > 12.5, 12.5, abs_spread),
         abs_spread = ifelse(abs_spread < 1, 1, abs_spread)) %>%
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
        plot.background = element_rect(color = 'white'),
        legend.position = 'none')

#Split up the results by total
total_check <- nba_clean %>%
  group_by(total_line) %>%
  summarise(total_hit = mean(sgp_hit),
            total_ins = mean(sgp_ins),
            total_miss = mean(sgp_miss),
            tot = n())


#Plot the results by total
p_total <- total_check %>% ggplot(aes(x = total_line)) +
  geom_line(aes(y = total_hit, color = 'Hit'), 
            stat = "summary_bin", binwidth = 3, linewidth = 1.5) +
  geom_line(aes(y = total_ins,  color = 'Free Bet'),
            stat = "summary_bin", binwidth = 3, linewidth = 1.5) +
  geom_line(aes(y = total_miss,  color = 'Miss'), 
            stat = "summary_bin", binwidth = 3, linewidth = 1.5) +
  labs(x = 'Game Total',
       y = '',
       color = 'Result') +
  scale_color_manual(values = my_colors, breaks = c("Hit",'Free Bet','Miss')) +
  ggthemes::theme_clean() +
  scale_x_continuous(breaks = c(seq(208.5,247.5,3))) +
  scale_y_continuous(breaks = seq(0,1,.1),
                     limits = c(0,1),
                     labels = scales::percent_format(accuracy = 1)) +
  theme(plot.title = element_text(face = 'bold', size = 18, hjust = .5),
        plot.subtitle = element_text(size = 14, hjust = .5),
        legend.position = c(.3,.85),
        plot.background  = element_rect(color = 'white'))

#Combine plots
combo <- ggpubr::ggarrange(p_spread, p_total, nrow = 2)
cowplot::ggdraw(combo) +
  theme(plot.title = element_text(size = 24, face = 'bold', hjust = .5),
        plot.caption = element_text(size = 14, face = 'bold', hjust = .5),
        plot.background = element_rect(fill = 'white', color = 'white')) +
  labs(title = 'Historical Results of One Game Parlay by Spread and Game Total| NBA',
       caption  = '2022-2023 NBA games prior to 2/1/23 | n = 668')

#Save plots
ggsave('NBA_total_spread.png', width = 14)
ggsave(plot = p_bar, 'NBA_hit_pct.png', width = 14)

#Save NBA results for plotting all leagues
nba_write_results <- data.frame(
  league = 'NBA',
  sgp_hit = mean(nba_clean$sgp_hit),
  sgp_ins = mean(nba_clean$sgp_ins),
  sgp_miss = mean(nba_clean$sgp_miss))

write.csv(nba_write_results, 'nba_results.csv', row.names = F)

#Calculate expected value
pct_hit <- mean(nba_clean$sgp_hit)
pct_ins <- mean(nba_clean$sgp_ins)
ev <- pct_hit*3.3*25 + 25*pct_ins*pct_hit*2.3
#ROI percent
(ev - 25)/25*100

#Number of game days
game_days <- hoopR::load_nba_schedule() %>%
  select(date) %>%
  mutate(date_only = str_sub(date,1,10)) %>%
  group_by(date_only) %>%
  summarise(total = n())
