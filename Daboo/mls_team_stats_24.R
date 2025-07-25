library(itscalledsoccer)
library(itscalledsoccer)
library(tidyverse)
library(dplyr)
# asa<-AmericanSoccerAnalysis$new()
# mls_teams<-asa$get_teams(leagues = "mls")
# mls_players<-asa$get_players(leagues="mls")
# salaries_2024<-asa$get_player_salaries(leagues = "mls", start_date="2024-09-13",end_date="2024-09-14")
# salaries_24<-salaries_2024|> left_join(mls_players|> select(player_id, player_name), by="player_id")|> 
#   left_join(mls_teams|> 
#   select(team_id, team=team_abbreviation), by="team_id")
# ## Creating a data set with all the team stats
# 
# mls_team_salaries_2024<-asa$get_team_salaries(leagues="mls",season_name=2024) 
# mls_team_goals_added_2024<-asa$get_team_goals_added(league="mls", season_name=2024, stage_name="Regular Season")
# mls_team_xG<-asa$get_team_xgoals(leagues="mls", season_name=2024, stage_name="Regular Season")
# 
# mls_teams_24<-mls_teams|> dplyr::slice(-6, -25)
# 
# mls_team_stats_24<-mls_team_salaries_2024|> 
#   left_join(mls_teams_24|> 
#   select(team_id, team=team_abbreviation), by="team_id")|> 
#   arrange(team)|> 
#   dplyr::slice(-30)
# 
# mls_team_stats_24<-mls_team_stats_24|> left_join(mls_team_xG, by="team_id")
# 
# mls_team_stats_24<-mls_team_stats_24|> 
#   left_join(mls_team_goals_added_2024|> 
#   select(team_id, minutes, data), by="team_id")
# 
# mls_team_stats_24<-mls_team_stats_24|> select(-team_id, -competition.x, -competition.y)
# 
# mls_team_stats_24<-mls_team_stats_24|> relocate(team, .before=1)
# mls_team_stats_24<-mls_team_stats_24|> relocate(count_games, .before=2)
# write_rds(mls_team_stats_24, "mls_team_stats_24.rds")

mls_team_stats_24 <- read_rds("mls_team_stats_24.rds")