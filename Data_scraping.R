#install.packages("data.table")
#install.packages("corrplot")
#install.packages("GGally")
#install.packages("tidyverse")
#install.packages("PerformanceAnalytics")
#install.packages("plotly")
#install.packages("Hmisc")
#install.packages("rvest")
#install.packages("stringr")
#install.packages("magrittr")
#install.packages("leaps")
#install.packages("pander")
#install.packages("dplyr")
#install.packages("rvest")
#install.packages("XML")
#install.packages("RCurl")
#install.packages("cowplot")


library(data.table)
library(corrplot)
library(GGally)
library(tidyverse)
library(PerformanceAnalytics)
library(plotly)
library(Hmisc)
library(rvest)
library(stringr)
library(magrittr)
library(leaps)
library(pander)
library(dplyr)
library(rvest)
library(XML)
library(RCurl)
library(cowplot)


cbb_list <- list()
for(year in c("2005","2006","2007","2008", "2009","2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018","2019")){
  main_url1 <- "https://basketball.realgm.com/ncaa/stats/"
  main_url2 <-"/Advanced_Stats/Qualified/All/Season/All/per/desc/"
  urls <- paste0(main_url1,year,main_url2,c("1","2","3","4","5","6", "7", "8", "9", "10","11", "12", "13", "14", "15", "16", "17", "18", "19", "20"))
  tables <- list()
  for(i in seq_along(urls))
  {
    total <- readHTMLTable(getURL(urls[i]))
    n.rows <- unlist(lapply(total, function(t) dim(t)[1]))
    tables[[i]] <- as.data.frame(total[[which.max(n.rows)]])
  }
  cbb <- bind_rows(tables)
  cbb <- cbb[,-1]
  colnames(cbb) <- c('Player', 'team_name', 'true_shooting_pct', 'effective_field_goal_pct', 'total_shooting_pct',
                     'off_reb_rate', 'def_reb_rate', 'tot_reb_rate', 'assist_rate', 'turnover_rate','steal_rate',
                     'block_rate', 'usage_rate', 'pure_point_rating', 'points_per_shot', 'off_rating', 'def_rating',
                     'net_rating', 'floor_impact_counter', 'PER')
  cbb$Player <- gsub(', Jr.', '', cbb$Player)
  cbb$Player <- gsub(' III', '', cbb$Player)
  cbb$Player <- gsub(' IV', '', cbb$Player)
  glimpse(cbb)
  cbb_list <- c(cbb_list,list(cbb))
}

nba_list <- list()
for(nba_year in c("2006","2007","2008", "2009","2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018","2019")){
  main_url_nba1 <- "https://www.basketball-reference.com/leagues/NBA_"
  main_url_nba2 <-"_advanced.html"
  url_nba <- paste0(main_url_nba1,nba_year,main_url_nba2)
  statpage <- read_html(url_nba)
  nba_data <- statpage %>% html_table(header = FALSE) %>% extract2(1)
  names(nba_data) <- nba_data[1,]
  nba_data <- nba_data[-1,]
  glimpse(nba_data)
  nba_data <- nba_data[,c(-1,-20,-25)]
  nba_data<- na.omit(nba_data)
  colnames(nba_data) <- c('Player', 'position', 'age', 'team_name', 'games_played',
                          'minutes_played', 'PER', 'true_shooting_pct', 'three_pt_att_rate', 'free_throw_rate', 'off_reb_rate',
                          'def_reb_rate', 'tot_reb_rate', 'assist_rate', 'steal_rate', 'block_rate', 'turnover_rate', 'usage_rate',
                          'off_win_shares', 'def_win_shares', 'win_shares', 'win_shares_per_48', 'off_BPM', 'def_BPM', 'BPM','VORP')
  
  
  nba_list <- c(nba_list,list(nba_data))
  
}
glimpse(nba_list[[2]])

#store each filtered dataset into new dataframes
#analysis_stats does not contain player names
#names_analysis_stats contains player names
analysis_all_draft_data <- all_draft_data[,c(-1,-2,-21, -23)]

names_analysis_all_draft_data <- all_draft_data[,c(-2,-21, -23)]
glimpse(analysis_all_draft_data)

glimpse(cbb_list[[1]])

stats_merge_multiple_seasons <- list()

for (z in 1:12) {
  
  merge_data <- merge(merge(merge(
    cbb_list[[z]],
    nba_list[[z]], by.x = "Player", by.y = "Player"),
    nba_list[[z+1]], by.x = "Player", by.y = "Player"),
    nba_list[[z+2]], by.x = "Player", by.y = "Player")
  
  colnames(merge_data) <- c('Player', 'team_name_college', 'true_shooting_pct_college', 'effective_field_goal_pct_college', 'total_shooting_pct_college',
                            'off_reb_rate_college', 'def_reb_rate_college', 'tot_reb_rate_college', 'assist_rate_college', 'turnover_rate_college','steal_rate_college',
                            'block_rate_college', 'usage_rate_college', 'pure_point_rating_college', 'points_per_shot_college', 'off_rating_college', 'def_rating_college',
                            'net_rating_college', 'floor_impact_counter_college', 'PER_college', 'position_NBA_1', 'age_NBA_1', 'team_name_NBA_1', 'games_played_NBA_1',
                            'minutes_played_NBA_1', 'PER_NBA_1', 'true_shooting_pct_NBA_1', 'three_pt_att_rate_NBA_1', 'free_throw_rate_NBA_1', 'off_reb_rate_NBA_1',
                            'def_reb_rate_NBA_1', 'tot_reb_rate_NBA_1', 'assist_rate_NBA_1', 'steal_rate_NBA_1', 'block_rate_NBA_1', 'turnover_rate_NBA_1', 'usage_rate_NBA_1',
                            'off_win_shares_NBA_1', 'def_win_shares_NBA_1', 'win_shares_NBA_1', 'win_shares_per_48_NBA_1', 'off_BPM_NBA_1', 'def_BPM_NBA_1', 'BPM_NBA_1','VORP_NBA_1',
                            'position_NBA_2', 'age_NBA_2', 'team_name_NBA_2', 'games_played_NBA_2',
                            'minutes_played_NBA_2', 'PER_NBA_2', 'true_shooting_pct_NBA_2', 'three_pt_att_rate_NBA_2', 'free_throw_rate_NBA_2', 'off_reb_rate_NBA_2',
                            'def_reb_rate_NBA_2', 'tot_reb_rate_NBA_2', 'assist_rate_NBA_2', 'steal_rate_NBA_2', 'block_rate_NBA_2', 'turnover_rate_NBA_2', 'usage_rate_NBA_2',
                            'off_win_shares_NBA_2', 'def_win_shares_NBA_2', 'win_shares_NBA_2', 'win_shares_per_48_NBA_2', 'off_BPM_NBA_2', 'def_BPM_NBA_2', 'BPM_NBA_2','VORP_NBA_2',
                            'position_NBA_3', 'age_NBA_3', 'team_name_NBA_3', 'games_played_NBA_3',
                            'minutes_played_NBA_3', 'PER_NBA_3', 'true_shooting_pct_NBA_3', 'three_pt_att_rate_NBA_3', 'free_throw_rate_NBA_3', 'off_reb_rate_NBA_3',
                            'def_reb_rate_NBA_3', 'tot_reb_rate_NBA_3', 'assist_rate_NBA_3', 'steal_rate_NBA_3', 'block_rate_NBA_3', 'turnover_rate_NBA_3', 'usage_rate_NBA_3',
                            'off_win_shares_NBA_3', 'def_win_shares_NBA_3', 'win_shares_NBA_3', 'win_shares_per_48_NBA_3', 'off_BPM_NBA_3', 'def_BPM_NBA_3', 'BPM_NBA_3','VORP_NBA_3')
  
  merge_data$true_shooting_pct_college <- as.numeric(merge_data$true_shooting_pct_college)
  merge_data$effective_field_goal_pct_college <- as.numeric(merge_data$effective_field_goal_pct_college)
  merge_data$total_shooting_pct_college <- as.numeric(merge_data$total_shooting_pct_college)
  merge_data$off_reb_rate_college <- as.numeric(merge_data$off_reb_rate_college)/100
  merge_data$def_reb_rate_college <- as.numeric(merge_data$def_reb_rate_college)/100
  merge_data$tot_reb_rate_college <- as.numeric(merge_data$tot_reb_rate_college)/100
  merge_data$assist_rate_college <- as.numeric(merge_data$assist_rate_college)/100
  merge_data$turnover_rate_college <- as.numeric(merge_data$turnover_rate_college)/100
  merge_data$steal_rate_college <- as.numeric(merge_data$steal_rate_college)/100
  merge_data$block_rate_college <- as.numeric(merge_data$block_rate_college)/100
  merge_data$usage_rate_college <- as.numeric(merge_data$usage_rate_college)/100
  merge_data$pure_point_rating_college <- as.numeric(merge_data$pure_point_rating_college)
  merge_data$points_per_shot_college <- as.numeric(merge_data$points_per_shot_college)
  merge_data$off_rating_college <- as.numeric(merge_data$off_rating_college)
  merge_data$def_rating_college <- as.numeric(merge_data$def_rating_college)
  merge_data$net_rating_college <- as.numeric(merge_data$net_rating_college)
  merge_data$floor_impact_counter_college <- as.numeric(merge_data$floor_impact_counter_college)
  merge_data$PER_college <- as.numeric(merge_data$PER_college)
  merge_data$age_NBA_1 <- as.numeric(merge_data$age_NBA_1)
  merge_data$games_played_NBA_1 <- as.numeric(merge_data$games_played_NBA_1)
  merge_data$minutes_played_NBA_1 <- as.numeric(merge_data$minutes_played_NBA_1)
  merge_data$PER_NBA_1 <- as.numeric(merge_data$PER_NBA_1)
  merge_data$true_shooting_pct_NBA_1 <- as.numeric(merge_data$true_shooting_pct_NBA_1)
  merge_data$three_pt_att_rate_NBA_1 <- as.numeric(merge_data$three_pt_att_rate_NBA_1)
  merge_data$free_throw_rate_NBA_1 <- as.numeric(merge_data$free_throw_rate_NBA_1)
  merge_data$off_reb_rate_NBA_1 <- as.numeric(merge_data$off_reb_rate_NBA_1)/100
  merge_data$def_reb_rate_NBA_1 <- as.numeric(merge_data$def_reb_rate_NBA_1)/100
  merge_data$tot_reb_rate_NBA_1 <- as.numeric(merge_data$tot_reb_rate_NBA_1)/100
  merge_data$assist_rate_NBA_1 <- as.numeric(merge_data$assist_rate_NBA_1)/100
  merge_data$steal_rate_NBA_1 <- as.numeric(merge_data$steal_rate_NBA_1)/100
  merge_data$block_rate_NBA_1 <- as.numeric(merge_data$block_rate_NBA_1)/100
  merge_data$turnover_rate_NBA_1 <- as.numeric(merge_data$turnover_rate_NBA_1)/100
  merge_data$usage_rate_NBA_1 <- as.numeric(merge_data$usage_rate_NBA_1)/100
  merge_data$off_win_shares_NBA_1 <- as.numeric(merge_data$off_win_shares_NBA_1)
  merge_data$def_win_shares_NBA_1 <- as.numeric(merge_data$def_win_shares_NBA_1)
  merge_data$win_shares_NBA_1 <- as.numeric(merge_data$win_shares_NBA_1)
  merge_data$win_shares_per_48_NBA_1 <- as.numeric(merge_data$win_shares_per_48_NBA_1)
  merge_data$off_BPM_NBA_1 <- as.numeric(merge_data$off_BPM_NBA_1)
  merge_data$def_BPM_NBA_1 <- as.numeric(merge_data$def_BPM_NBA_1)
  merge_data$BPM_NBA_1 <- as.numeric(merge_data$BPM_NBA_1)
  merge_data$VORP_NBA_1 <- as.numeric(merge_data$VORP_NBA_1)
  merge_data$age_NBA_2 <- as.numeric(merge_data$age_NBA_2)
  merge_data$games_played_NBA_2 <- as.numeric(merge_data$games_played_NBA_2)
  merge_data$minutes_played_NBA_2 <- as.numeric(merge_data$minutes_played_NBA_2)
  merge_data$PER_NBA_2 <- as.numeric(merge_data$PER_NBA_2)
  merge_data$true_shooting_pct_NBA_2 <- as.numeric(merge_data$true_shooting_pct_NBA_2)
  merge_data$three_pt_att_rate_NBA_2 <- as.numeric(merge_data$three_pt_att_rate_NBA_2)
  merge_data$free_throw_rate_NBA_2 <- as.numeric(merge_data$free_throw_rate_NBA_2)
  merge_data$off_reb_rate_NBA_2 <- as.numeric(merge_data$off_reb_rate_NBA_2)/100
  merge_data$def_reb_rate_NBA_2 <- as.numeric(merge_data$def_reb_rate_NBA_2)/100
  merge_data$tot_reb_rate_NBA_2 <- as.numeric(merge_data$tot_reb_rate_NBA_2)/100
  merge_data$assist_rate_NBA_2 <- as.numeric(merge_data$assist_rate_NBA_2)/100
  merge_data$steal_rate_NBA_2 <- as.numeric(merge_data$steal_rate_NBA_2)/100
  merge_data$block_rate_NBA_2 <- as.numeric(merge_data$block_rate_NBA_2)/100
  merge_data$turnover_rate_NBA_2 <- as.numeric(merge_data$turnover_rate_NBA_2)/100
  merge_data$usage_rate_NBA_2 <- as.numeric(merge_data$usage_rate_NBA_2)/100
  merge_data$off_win_shares_NBA_2 <- as.numeric(merge_data$off_win_shares_NBA_2)
  merge_data$def_win_shares_NBA_2 <- as.numeric(merge_data$def_win_shares_NBA_2)
  merge_data$win_shares_NBA_2 <- as.numeric(merge_data$win_shares_NBA_2)
  merge_data$win_shares_per_48_NBA_2 <- as.numeric(merge_data$win_shares_per_48_NBA_2)
  merge_data$off_BPM_NBA_2 <- as.numeric(merge_data$off_BPM_NBA_2)
  merge_data$def_BPM_NBA_2 <- as.numeric(merge_data$def_BPM_NBA_2)
  merge_data$BPM_NBA_2 <- as.numeric(merge_data$BPM_NBA_2)
  merge_data$VORP_NBA_2 <- as.numeric(merge_data$VORP_NBA_2)
  merge_data$age_NBA_3 <- as.numeric(merge_data$age_NBA_3)
  merge_data$games_played_NBA_3 <- as.numeric(merge_data$games_played_NBA_3)
  merge_data$minutes_played_NBA_3 <- as.numeric(merge_data$minutes_played_NBA_3)
  merge_data$PER_NBA_3 <- as.numeric(merge_data$PER_NBA_3)
  merge_data$true_shooting_pct_NBA_3 <- as.numeric(merge_data$true_shooting_pct_NBA_3)
  merge_data$three_pt_att_rate_NBA_3 <- as.numeric(merge_data$three_pt_att_rate_NBA_3)
  merge_data$free_throw_rate_NBA_3 <- as.numeric(merge_data$free_throw_rate_NBA_3)
  merge_data$off_reb_rate_NBA_3 <- as.numeric(merge_data$off_reb_rate_NBA_3)/100
  merge_data$def_reb_rate_NBA_3 <- as.numeric(merge_data$def_reb_rate_NBA_3)/100
  merge_data$tot_reb_rate_NBA_3 <- as.numeric(merge_data$tot_reb_rate_NBA_3)/100
  merge_data$assist_rate_NBA_3 <- as.numeric(merge_data$assist_rate_NBA_3)/100
  merge_data$steal_rate_NBA_3 <- as.numeric(merge_data$steal_rate_NBA_3)/100
  merge_data$block_rate_NBA_3 <- as.numeric(merge_data$block_rate_NBA_3)/100
  merge_data$turnover_rate_NBA_3 <- as.numeric(merge_data$turnover_rate_NBA_3)/100
  merge_data$usage_rate_NBA_3 <- as.numeric(merge_data$usage_rate_NBA_3)/100
  merge_data$off_win_shares_NBA_3 <- as.numeric(merge_data$off_win_shares_NBA_3)
  merge_data$def_win_shares_NBA_3 <- as.numeric(merge_data$def_win_shares_NBA_3)
  merge_data$win_shares_NBA_3 <- as.numeric(merge_data$win_shares_NBA_3)
  merge_data$win_shares_per_48_NBA_3 <- as.numeric(merge_data$win_shares_per_48_NBA_3)
  merge_data$off_BPM_NBA_3 <- as.numeric(merge_data$off_BPM_NBA_3)
  merge_data$def_BPM_NBA_3 <- as.numeric(merge_data$def_BPM_NBA_3)
  merge_data$BPM_NBA_3 <- as.numeric(merge_data$BPM_NBA_3)
  merge_data$VORP_NBA_3 <- as.numeric(merge_data$VORP_NBA_3)
  
  # #remove duplicates and players greater than age 25
  
  duplicate_check<-merge_data[,1]
  duplicates <- which(duplicated(duplicate_check))
  merge_data <- merge_data[-duplicates,]
  
  improper_age <- which(merge_data$age_NBA_1 > 25)
  merge_data <- merge_data[-improper_age,]
  
  year <- toString(2005+z)
  filename <- paste0('statmerge_', year,'.csv')
  write.csv(merge_data, filename)
  stats_merge_multiple_seasons <- c(stats_merge_multiple_seasons, list(merge_data))
}
glimpse(stats_merge_multiple_seasons[[1]])
write.csv(stats_merge_multiple_seasons[[1]], "datafilestuff.csv")

all_draft_data_multiple_seasons <-bind_rows(stats_merge_multiple_seasons)
glimpse(all_draft_data_multiple_seasons)
write.csv(all_draft_data_multiple_seasons, 'alldraftdata_multiple_seasons.csv')



#Bring in predictors from 2018-19 data set to predict 2019-20 values
prediction_draft_data <- cbb_list[[15]]
write_csv(prediction_draft_data,"draftinfo.csv")
colnames(prediction_draft_data) <- c('Player', 'team_name_college', 'true_shooting_pct_college', 'effective_field_goal_pct_college', 'total_shooting_pct_college',
                                     'off_reb_rate_college', 'def_reb_rate_college', 'tot_reb_rate_college', 'assist_rate_college', 'turnover_rate_college','steal_rate_college',
                                     'block_rate_college', 'usage_rate_college', 'pure_point_rating_college', 'points_per_shot_college', 'off_rating_college', 'def_rating_college',
                                     'net_rating_college', 'floor_impact_counter_college', 'PER_college')
prediction_draft_data$Player <- as.character(prediction_draft_data$Player)
prediction_draft_data$team_name_college <- as.character(prediction_draft_data$team_name_college)
prediction_draft_data$true_shooting_pct <- as.numeric(prediction_draft_data$true_shooting_pct)
prediction_draft_data$true_shooting_pct <- as.numeric(prediction_draft_data$true_shooting_pct)
prediction_draft_data$effective_field_goal_pct <- as.numeric(prediction_draft_data$effective_field_goal_pct)
prediction_draft_data$total_shooting_pct <- as.numeric(prediction_draft_data$total_shooting_pct)
prediction_draft_data$off_reb_rate_college <- as.numeric(prediction_draft_data$off_reb_rate_college)/100
prediction_draft_data$def_reb_rate_college <- as.numeric(prediction_draft_data$def_reb_rate_college)/100
prediction_draft_data$tot_reb_rate_college <- as.numeric(prediction_draft_data$tot_reb_rate_college)/100
prediction_draft_data$assist_rate_college <- as.numeric(prediction_draft_data$assist_rate_college)/100
prediction_draft_data$turnover_rate_college <- as.numeric(prediction_draft_data$turnover_rate_college)/100
prediction_draft_data$steal_rate_college <- as.numeric(prediction_draft_data$steal_rate_college)/100
prediction_draft_data$block_rate_college <- as.numeric(prediction_draft_data$block_rate_college)/100
prediction_draft_data$usage_rate_college <- as.numeric(prediction_draft_data$usage_rate_college)/100
prediction_draft_data$pure_point_rating <- as.numeric(prediction_draft_data$pure_point_rating)
prediction_draft_data$points_per_shot <- as.numeric(prediction_draft_data$points_per_shot)
prediction_draft_data$off_rating <- as.numeric(prediction_draft_data$off_rating)
prediction_draft_data$def_rating <- as.numeric(prediction_draft_data$def_rating)
prediction_draft_data$net_rating <- as.numeric(prediction_draft_data$net_rating)
prediction_draft_data$floor_impact_counter <- as.numeric(prediction_draft_data$floor_impact_counter)
prediction_draft_data$PER <- as.numeric(prediction_draft_data$PER)

