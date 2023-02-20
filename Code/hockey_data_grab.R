library(tidyverse)
library(rvest)

#NHL testing
nhl_loaded <- fastRhockey::nhl_schedule(2023)
nhl_clean <- nhl_loaded %>%
  select(away_team_id,home_team_id,
         away_score, home_score, game_date_time) %>%
  mutate(game_date_time = str_replace(game_date_time,'T',' '),
         game_date_time = str_replace(game_date_time,'Z',''),
         game_date_time = as.POSIXct(game_date_time),
         game_date_time = game_date_time - 5*60*60,
         game_date_time = as.character(game_date_time),
         ymd = substr(game_date_time, 1,10),
         cbs_date = str_replace_all(ymd,'-','')) %>%
  select(-ymd,-game_date_time)

#Number of game days
length(unique(nhl_clean$cbs_date))

#Fix abbreviations for NHL teams
teams_loaded <- fastRhockey::nhl_teams()
teams <- teams_loaded %>%
  select(team_id, abbreviation) %>%
  mutate(team_abbr = case_when(abbreviation == 'CBJ' ~ 'CLB',
                               abbreviation == 'LAK' ~ 'LA',
                               abbreviation == 'MTL' ~ 'MON',
                               abbreviation == 'SJS' ~ 'SJ',
                               abbreviation == 'TBL' ~ 'TB',
                               abbreviation == 'VGK' ~ 'LV',
                               abbreviation == 'NJD' ~ 'NJ',
                               abbreviation == 'WSH' ~ 'WAS',
                               TRUE ~ abbreviation)) %>%
  select(-abbreviation)

#Create DF for scraping
cbs_begin <- "https://www.cbssports.com/nhl/gametracker/recap/NHL_"
my_df <- nhl_clean %>%
  left_join(teams, by = c('away_team_id' = 'team_id' )) %>%
  left_join(teams, by = c('home_team_id' = 'team_id' )) %>%
  select(home_team = team_abbr.y, away_team = team_abbr.x,
         home_score, away_score, cbs_date) %>%
  mutate(cbs_url = paste0(cbs_begin,cbs_date,'_',away_team,'@',home_team,'/')) %>%
  filter(cbs_date < 20230201)


#Create temporary df that will add needed info during loop
temp_df <- data.frame(cbs_url = my_df$cbs_url,
                      home_ml = NA,
                      total_line = NA,
                      away_ml = NA)

#Takes awhile to run this loop
for (i in seq(1,nrow(temp_df),1)) {
  temp_url <- temp_df$cbs_url[i]
  my_url_loaded <- read_html(temp_url)
  mytext<- html_text2(my_url_loaded)
  string_tst <- substr(mytext,100,500)
  rcs <- unlist(stringr::str_locate_all(string_tst,'ML:'))
  first_ind <- rcs[1]
  last_ind <- rcs[4]
  final_lines<-strsplit(unlist(strsplit(substr(string_tst,first_ind,last_ind+5),'\n')),' ')
  temp_df$home_ml[i] <- final_lines[[1]][2]
  temp_df$total_line[i] <- final_lines[[2]][2]
  temp_df$away_ml[i] <- final_lines[[3]][2]
  print(i)
}

#Save as .csv for future use. 
#Also helps with converting lines from characters to numeric
write.csv(temp_df, file = 'nhl_data.csv', row.names = F)

#Came back at a later time to put it all together
line_data <- read.csv('nhl_data.csv')

#Pull all together and keep only relevant data
my_final_df <- my_df %>%
  left_join(line_data, by = 'cbs_url') %>%
  select(-cbs_date, -cbs_url)

#Save for future use
write.csv(my_final_df,'nhl_game_data.csv', row.names = F)

