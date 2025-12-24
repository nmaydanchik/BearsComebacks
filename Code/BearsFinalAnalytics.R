library(tidyverse)

comeback_wins <- c(4, 6, 9, 10, 11, 16) # weeks that the Bears had comeback wins (Raiders, Commanders, Bengals, Giants, Vikings--Wk11, Packers--Wk16)
one_pos_losses <- c(1, 14) # weeks that the Bears lost one-possesion games (failed comebacks: Vikings--Wk1, Packers--Wk14)

# Load data
gamefiles <- list.files("pbps", full.names = TRUE) # Play-by-play raw data for Weeks 1-16 (15 games)
gamelogs <- lapply(gamefiles, read_csv, col_select=c(qtr, fixed_drive, posteam, yrdln, down, ydstogo, play_type, yards_gained, touchdown, fumble_lost, interception, penalty, fd, complete_pass, cp, cpoe, ep, epa, success, air_yards, return_yards, passer_player_name, rusher_player_name, receiver_player_name, time, total_home_score, total_away_score, fixed_drive_result, series, series_result, series_success, vegas_home_wp, vegas_home_wpa, home_wp, desc, home_team, away_team, week)); rm(gamefiles)
seasonpbp <- bind_rows(gamelogs) %>% arrange(week); rm(gamelogs) # Data frame of every play in every Bears game this season

# split_offense <- lapply(gamelogs, filter, posteam=="CHI", !is.na(down), play_type %in% c("pass","run", "qb_kneel", "qb_spike")) # Used for reconciliation

# Create separate data frames for offense, defense, special teams
CHI_offense <- seasonpbp %>% filter(posteam=="CHI", !is.na(down), play_type %in% c("pass", "run", "qb_kneel", "qb_spike")) # !is.na(down) for two-point attempts
CHI_defense <- seasonpbp %>% filter(posteam!="CHI", !is.na(posteam), !is.na(down), play_type %in% c("pass", "run", "qb_kneel", "qb_spike"))
CHI_dropbacks <- filter(CHI_offense, play_type %in% c("pass", "qb_spike") | str_detect(desc, "scrambles")) # Includes passes, sacks, spikes, scrambles
CHI_special <- seasonpbp %>% filter(play_type %in% c("kickoff","punt","field_goal","extra_point")) %>% 
  mutate(adj_epa = if_else(posteam=="CHI", epa, -1*epa)) # adj_epa is so positive epa is good in all scenarios

# Function for finding the advanced stats for a given set of non-special teams plays
offense_stats <- function(offense_pbp) {
  c(epa=mean(offense_pbp$epa),
       epa_pass=mean(filter(offense_pbp, play_type %in% c("pass", "qb_spike"))$epa),
       epa_run=mean(filter(offense_pbp, play_type %in% c("run", "qb_kneel"))$epa),
       epa_dropback=mean(filter(offense_pbp, play_type %in% c("pass", "qb_spike") | str_detect(desc, "scrambles"))$epa),
       success_rate=mean(offense_pbp$success),
       big_plays=nrow(filter(offense_pbp, epa> 0.90)),
       bad_plays=nrow(filter(offense_pbp, epa< -0.75)),
       big_play_rate=nrow(filter(offense_pbp, epa> 0.90))/nrow(offense_pbp),
       bad_play_rate=nrow(filter(offense_pbp, epa< -0.75))/nrow(offense_pbp),
       explosive_plays=nrow(filter(offense_pbp, (play_type=="pass" & yards_gained>=20) | (play_type=="run" & yards_gained>=10))),
       explosive_play_rate=nrow(filter(offense_pbp, (play_type=="pass" & yards_gained>=20) | (play_type=="run" & yards_gained>=10)))/nrow(offense_pbp),
       fumbles=sum(offense_pbp$fumble_lost),
       interceptions=sum(offense_pbp$interception),
       turnovers=sum(offense_pbp$fumble_lost) + sum(offense_pbp$interception),
       to_rate=mean(offense_pbp$fumble_lost)+mean(offense_pbp$interception),
       completion_percent=mean(filter(offense_pbp, play_type %in% c("pass", "qb_spike"), !str_detect(desc, "sack"))$complete_pass),
       completion_percent_oe=mean(offense_pbp$cpoe, na.rm=TRUE)/100,
       yac=mean(filter(offense_pbp, complete_pass==1)$yards_gained-filter(offense_pbp, complete_pass==1)$air_yards))
}

# Function for finding advanced stats for a given set of special teams plays
special_teams_stats <- function(st_pbp, exp_epa) {
  c(epa=mean(st_pbp$adj_epa),
      big_plays=nrow(filter(st_pbp, adj_epa > exp_epa)),
      bad_plays=nrow(filter(st_pbp, adj_epa < -0.75)),
      big_play_rate=nrow(filter(st_pbp, adj_epa > exp_epa))/nrow(st_pbp),
      bad_play_rate=nrow(filter(st_pbp, adj_epa < -0.75))/nrow(st_pbp))
}

# Season stats -- whole game
offense <- offense_stats(CHI_offense)
defense <- offense_stats(CHI_defense) # Stats of opposing team offenses against CHI
dropbacks <- offense_stats(CHI_dropbacks)
special_teams <- special_teams_stats(CHI_special, 0.90)

# Season stats -- by quarter
qs_offense <- matrix(nrow=0, ncol=19); colnames(qs_offense)=c("qtr", names(offense))
for (quarter in 1:4) {
  qs_offense <- rbind(qs_offense, c(qtr=quarter, offense_stats(filter(CHI_offense, qtr==quarter))))
  # qs_offense[quarter, ] <- c(qtr=quarter, offense_stats(filter(CHI_offense, qtr==quarter)))
}

qs_defense <- matrix(nrow=0, ncol=19); colnames(qs_defense)=c("qtr", names(defense))
for (quarter in 1:4) {
  qs_defense <- rbind(qs_defense, c(qtr=quarter, offense_stats(filter(CHI_defense, qtr==quarter))))
}

qs_dropbacks <- matrix(nrow=0, ncol=19); colnames(qs_dropbacks)=c("qtr", names(dropbacks))
for (quarter in 1:4) {
  qs_dropbacks <- rbind(qs_dropbacks, c(qtr=quarter, offense_stats(filter(CHI_dropbacks, qtr==quarter))))
}

qs_special_teams <- matrix(nrow=0, ncol=6); colnames(qs_special_teams)=c("qtr", names(special_teams))
for (quarter in 1:4) {
  qs_special_teams <- rbind(qs_special_teams, c(qtr=quarter, special_teams_stats(filter(CHI_special, qtr==quarter), 0.90)))
}

# Stats for Bears comebacks fourth quarters (+ Wk 16 overtime)
comeback_offense_pbp <- CHI_offense %>% filter(week %in% comeback_wins & qtr>=4)
comeback_defense_pbp <- CHI_defense %>% filter(week %in% comeback_wins & qtr>=4)
comeback_dropbacks_pbp <- CHI_dropbacks %>% filter(week %in% comeback_wins & qtr>=4)
comeback_special_pbp <- CHI_special %>% filter(week %in% comeback_wins & qtr>=4)

comeback_offense <- offense_stats(comeback_offense_pbp)
comeback_defense <- offense_stats(comeback_defense_pbp)
comeback_dropbacks <- offense_stats(comeback_dropbacks_pbp)
comeback_special_teams <- special_teams_stats(comeback_special_pbp, exp_epa=0.90)

# Stats for Bears failed comeback fourth quarters
opl_offense_pbp <- CHI_offense %>% filter(week %in% one_pos_losses & qtr>=4)
opl_defense_pbp <- CHI_defense %>% filter(week %in% one_pos_losses & qtr>=4)
opl_dropbacks_pbp <- CHI_dropbacks %>% filter(week %in% one_pos_losses & qtr>=4)
opl_special_pbp <- CHI_special %>% filter(week %in% one_pos_losses & qtr>=4)

opl_offense <- offense_stats(opl_offense_pbp)
opl_defense <- offense_stats(opl_defense_pbp)
opl_dropbacks <- offense_stats(opl_dropbacks_pbp)
opl_special_teams <- special_teams_stats(opl_special_pbp, exp_epa = .90)

# Stats for Bears comebacks first three quarters
cbfirst3_offense_pbp <- CHI_offense %>% filter(week %in% comeback_wins & qtr<4)
cbfirst3_defense_pbp <- CHI_defense %>% filter(week %in% comeback_wins & qtr<4)
cbfirst3_dropbacks_pbp <- CHI_dropbacks %>% filter(week %in% comeback_wins & qtr<4)
cbfirst3_special_pbp <- CHI_special %>% filter(week %in% comeback_wins & qtr<4)

cbfirst3_offense <- offense_stats(cbfirst3_offense_pbp)
cbfirst3_defense <- offense_stats(cbfirst3_defense_pbp)
cbfirst3_dropbacks <- offense_stats(cbfirst3_dropbacks_pbp)
cbfirst3_special_teams <- special_teams_stats(cbfirst3_special_pbp, exp_epa=0.90)

# Stats for Bears season excluding comeback fourth quarters
control_offense_pbp <- anti_join(CHI_offense, comeback_offense_pbp)
control_defense_pbp <- anti_join(CHI_defense, comeback_defense_pbp)
control_dropbacks_pbp <- anti_join(CHI_dropbacks, comeback_dropbacks_pbp)
control_special_pbp <- anti_join(CHI_special, comeback_special_pbp)

control_offense <- offense_stats(control_offense_pbp)
control_defense <- offense_stats(control_defense_pbp)
control_dropbacks <- offense_stats(control_dropbacks_pbp)
control_special_teams <- special_teams_stats(control_special_pbp, exp_epa=0.90)

# Analysis
offense_compare <- rbind(offense, control_offense, comeback_offense, cbfirst3_offense, season_fourth=qs_offense[4, 2:19], opl_offense)
defense_compare <- rbind(defense, control_defense, comeback_defense, cbfirst3_defense, season_fourth=qs_defense[4, 2:19], opl_defense)
dropbacks_compare <- rbind(dropbacks, control_dropbacks, comeback_dropbacks, cbfirst3_dropbacks, season_fourth=qs_dropbacks[4, 2:19], opl_dropbacks)
special_teams_compare <- rbind(special_teams, control_special_teams, comeback_special_teams, cbfirst3_special_teams, season_fourth=qs_special_teams[4, 2:6], opl_special_teams)

save(offense_compare, defense_compare, dropbacks_compare, special_teams_compare, file=("BearsCompare.RData"))

# ----- 

# Stats for every quarter of every week this season (60 quarters)
qws_offense <- matrix(nrow=0, ncol=20); colnames(qws_offense)=c("wk", "qtr", names(offense))
for (i_week in c(1:4, 6:16)) {
  for (quarter in 1:4) {
    qws_offense <- rbind(qws_offense, c(wk=i_week, qtr=quarter, offense_stats(filter(CHI_offense, week==i_week, qtr==quarter))))
  }
}

qws_dropbacks <- matrix(nrow=0, ncol=20); colnames(qws_dropbacks)=c("wk", "qtr", names(dropbacks))
for (i_week in c(1:4, 6:16)) {
  for (quarter in 1:4) {
    qws_dropbacks <- rbind(qws_dropbacks, c(wk=i_week, qtr=quarter, offense_stats(filter(CHI_dropbacks, week==i_week, qtr==quarter))))
  }
}

qws_defense <- matrix(nrow=0, ncol=20); colnames(qws_defense)=c("wk", "qtr", names(defense))
for (i_week in c(1:4, 6:16)) {
  for (quarter in 1:4) {
    qws_defense <- rbind(qws_defense, c(wk=i_week, qtr=quarter, offense_stats(filter(CHI_defense, week==i_week, qtr==quarter))))
  }
}

qws_special_teams <- matrix(nrow=0, ncol=7); colnames(qws_special_teams)=c("wk", "qtr", names(special_teams))
for (i_week in c(1:4, 6:16)) {
  for (quarter in 1:4) {
    qws_special_teams <- rbind(qws_special_teams, c(wk=i_week, qtr=quarter, special_teams_stats(filter(CHI_special, week==i_week, qtr==quarter), 0.90)))
  }
}

special_epa_comp <- matrix(nrow=0, ncol=6); colnames(special_epa_comp)=c("epa_thresh", names(special_teams))
for (i_epa in (18:36)/20) {
  special_epa_comp <- rbind(special_epa_comp, special_teams_stats(CHI_special, i_epa))
}



