library(tidyverse)
library(dplyr)
library(ggplot2)
library(itscalledsoccer)

# asa<-AmericanSoccerAnalysis$new()
# write_rds(asa, "asa.rds")
 asa <- read_rds("asa.rds")

## constructing the main data set
# mls_teams<-asa$get_teams(leagues="mls")
# 
#  mls_teams<-mls_teams|>
#    dplyr::slice(-6, -25)
#  write_rds(mls_teams, "mls_teams.rds")
mls_teams <- read_rds("mls_teams.rds")

 # mls_players<-asa$get_players(leagues="mls")
 # 
 # write_rds(mls_players, "mls_players.rds")
mls_players <- read_rds("mls_players.rds")

 # all_salaries_2024<-asa$get_player_salaries(leagues = "mls", 
 #                                            start_date="2024-09-13", 
 #                                            end_date="2024-09-14")
 # 
 # write_rds(all_salaries_2024, "all_salaries_2024.rds")
all_salaries_2024 <- read_rds("all_salaries_2024.rds")

all_salaries_24<-all_salaries_2024|>
  left_join(mls_players|>
              select(player_id, player_name), by="player_id")|>
  left_join(mls_teams|>
              select(team_id, team=team_abbreviation), by="team_id")

all_salaries_24<-all_salaries_24|>
  mutate(year = 2024,
         team_year = paste(team, year, sep = "_")) |>
  relocate(team_year)

 # all_salaries_2023<-asa$get_player_salaries(leagues = "mls", 
 #                                            start_date="2023-09-15", 
 #                                            end_date="2023-09-16")
 # 
 # write_rds(all_salaries_2023, "all_salaries_2023.rds")
all_salaries_2023 <- read_rds("all_salaries_2023.rds")

all_salaries_23<-all_salaries_2023|>
  left_join(mls_players|>
              select(player_id, player_name), by="player_id")|>
  left_join(mls_teams|>
              select(team_id, team=team_abbreviation), by="team_id")

all_salaries_23<-all_salaries_23|>
  mutate(year = 2023,
         team_year = paste(team, year, sep = "_")) |>
  relocate(team_year)

 # all_salaries_2022<-asa$get_player_salaries(leagues = "mls",                                   season_name=2022)
 # 
 # write_rds(all_salaries_2022, "all_salaries_2022.rds")
all_salaries_2022 <- read_rds("all_salaries_2022.rds")

all_salaries_22<-all_salaries_2022|>
  left_join(mls_players|>
              select(player_id, player_name), by="player_id")|>
  left_join(mls_teams|>
              select(team_id, team=team_abbreviation), by="team_id")

all_salaries_22<-all_salaries_22|>
  filter((team == "MTL" & mlspa_release == "2022-04-15") |
           (team != "MTL" & mlspa_release == "2022-09-02"))

all_salaries_22<-all_salaries_22|>
  mutate(year = 2022,
         team_year = paste(team, year, sep = "_")) |>
  relocate(team_year)

 # all_salaries_2021<-asa$get_player_salaries(leagues = "mls", 
 #                                            start_date="2021-09-30", 
 #                                            end_date="2021-10-01")
 # 
 # write_rds(all_salaries_2021, "all_salaries_2021.rds")
all_salaries_2021 <- read_rds("all_salaries_2021.rds")

all_salaries_21<-all_salaries_2021|>
  left_join(mls_players|>
              select(player_id, player_name), by="player_id")|>
  left_join(mls_teams|>
              select(team_id, team=team_abbreviation), by="team_id")

all_salaries_21<-all_salaries_21|>
  mutate(year = 2021,
         team_year = paste(team, year, sep = "_")) |>
  relocate(team_year)