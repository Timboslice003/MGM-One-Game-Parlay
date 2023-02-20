library(tidyverse)

#Load data
nfl_loaded <- read.csv('nfl_results.csv')
nba_loaded <- read.csv('nba_results.csv')
nhl_loaded <- read.csv('nhl_results.csv')

#Combine Data and reformat
combo_loaded <- rbind(nfl_loaded, nba_loaded, nhl_loaded)
combo_df <- combo_loaded %>% rename('Hit' = sgp_hit,
                                    'Free Bet' = sgp_ins,
                                    'Miss' = sgp_miss) %>%
  pivot_longer(!league, names_to = 'result', values_to = 'rate')

#Organization to help with plotting
combo_df$result <- factor(combo_df$result, levels = c('Hit','Free Bet','Miss'))
combo_df$league <- factor(combo_df$league, levels = c('NFL','NBA','NHL'))
my_colors <- c('green4','yellow2','red2')
my_labels <- c('https://www.freepnglogos.com/uploads/nfl-logo-png/nfl-nike-logo-logodownload-download-logotipos-21.png',
               'https://cdn.freebiesupply.com/logos/thumbs/2x/nba-logo.png',
               'https://1000logos.net/wp-content/uploads/2017/05/NHL-Logo.png')

#Plot results
combo_df %>% ggplot(aes(x = league, y = rate, fill = result)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values = my_colors) +
  scale_y_continuous(breaks = seq(0,1,.1),
                     limits = c(0,1),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = '',
       y = '',
       title = 'Historical Results of One Game Parlay',
       subtitle = '',
       fill = 'Result') +
  ggthemes::theme_clean() +
  scale_x_discrete(labels = my_labels) +
  theme(axis.text.x = nflplotR::element_path(size = 2),
        legend.position = c(.35,.85),
        plot.title = element_text(face = 'bold', size = 18, hjust = .5),
        plot.background = element_rect(fill = 'white', color = 'black')) +
  annotate('text', label = 'Odds: +175', x = 1, y = .63, size = 5) +
  annotate('text', label = 'Odds: +230', x = 2, y = .63, size = 5) +
  annotate('text', label = 'Odds: -110', x = 3, y = .63, size = 5)

#Save plot
ggsave('combo_bar.png', width = 14)

#Final Table
final_table <- combo_loaded %>%
  mutate(league = my_labels,
         sgp_hit = paste0(round(sgp_hit,3)*100,'%'),
         sgp_ins = paste0(round(sgp_ins,3)*100,'%'),
         sgp_miss = paste0(round(sgp_miss,3)*100,'%'),
         ogp_ev = c('18.3%', '8.1%','6.7%'),
         no_games = c('61','168','178'),
         exp_p = c('$279.08','$340.20','$298.15'))

gt::gt(final_table) %>%
  gtExtras::gt_img_rows(league) %>%
  gt::cols_align(align = 'center') %>%
  gtExtras::gt_theme_538() %>%
  gt::cols_label(sgp_hit = 'Hit',
                 sgp_ins = 'Free Bet',
                 sgp_miss = 'Miss',
                 league = '',
                 no_games = '# of gamedays',
                 exp_p = 'Season-long\nexpected profit') %>%
  gt::tab_spanner(label = 'OGP Historical Percentages',
                  columns = sgp_hit:sgp_miss) %>%
  gt::tab_header('Final Results for the MGM OGP Promotion') %>%
  gt::opt_align_table_header(align = 'center')
  
#gt::gtsave('final_table.png')

