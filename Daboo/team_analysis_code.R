# asa<-AmericanSoccerAnalysis$new()
# 
# # constructing the main data set
# mls_teams<-asa$get_teams(leagues="mls")
# mls_teams<-mls_teams|>
#   dplyr::slice(-6, -25)
# 
# mls_team_salaries_24<-asa$get_team_salaries(leagues = "mls",
#                                             season_name=2024)
# mls_team_xG_24<-asa$get_team_xgoals(leagues="mls",
#                                     season_name=2024)
# 
# mls_team_goals_added_24<-asa$get_team_goals_added(leagues="mls",
#                                                   season_name=2024)
# 
# mls_teams_24<-mls_teams|>
#   left_join(mls_team_salaries_24|>
#               select(team_id, count_players, total_guaranteed_compensation,
#                      avg_guaranteed_compensation, median_guaranteed_compensation,
#                      std_dev_guaranteed_compensation), by="team_id")
# mls_teams_24<-mls_teams_24|>
#   left_join(mls_team_xG_24,
#             by="team_id")
# 
# mls_teams_24<-mls_teams_24|>
#   left_join(mls_team_goals_added_24, by="team_id")
# 
# mls_teams_24<-mls_teams_24|>
#   select(-team_id, -team_name, -team_short_name, -competition.x, -competition.y)
# 
# mls_teams_24<-mls_teams_24|>
#   mutate(year=2024)
# 
# mls_team_salaries_23<-asa$get_team_salaries(leagues = "mls",
#                                             season_name=2023)
# mls_team_xG_23<-asa$get_team_xgoals(leagues="mls",
#                                     season_name=2023)
# 
# mls_team_goals_added_23<-asa$get_team_goals_added(leagues="mls",
#                                                   season_name=2023)
# 
# mls_teams_23<-mls_teams|>
#   left_join(mls_team_salaries_23|>
#               select(team_id, count_players, total_guaranteed_compensation,
#                      avg_guaranteed_compensation, median_guaranteed_compensation,
#                      std_dev_guaranteed_compensation), by="team_id")
# mls_teams_23<-mls_teams_23|>
#   left_join(mls_team_xG_23,
#             by="team_id")
# 
# mls_teams_23<-mls_teams_23|>
#   left_join(mls_team_goals_added_23, by="team_id")
# 
# mls_teams_23<-mls_teams_23|>
#   select(-team_id, -team_name, -team_short_name, -competition.x, -competition.y)
# 
# mls_teams_23<-mls_teams_23|>
#   mutate(year=2023)
# 
# mls_team_salaries_22<-asa$get_team_salaries(leagues = "mls",
#                                             season_name=2022)
# mls_team_xG_22<-asa$get_team_xgoals(leagues="mls",
#                                     season_name=2022)
# 
# mls_team_goals_added_22<-asa$get_team_goals_added(leagues="mls",
#                                                   season_name=2022)
# 
# mls_teams_22<-mls_teams|>
#   left_join(mls_team_salaries_22|>
#               select(team_id, count_players, total_guaranteed_compensation,
#                      avg_guaranteed_compensation, median_guaranteed_compensation,
#                      std_dev_guaranteed_compensation), by="team_id")
# mls_teams_22<-mls_teams_22|>
#   left_join(mls_team_xG_22,
#             by="team_id")
# 
# mls_teams_22<-mls_teams_22|>
#   left_join(mls_team_goals_added_22, by="team_id")
# 
# mls_teams_22<-mls_teams_22|>
#   select(-team_id, -team_name, -team_short_name, -competition.x, -competition.y)
# 
# mls_teams_22<-mls_teams_22|>
#   mutate(year=2022)
# 
# mls_teams_22<-mls_teams_22|>
#   dplyr::slice(-27)
# 
# mls_team_salaries_21<-asa$get_team_salaries(leagues = "mls",
#                                             season_name=2021)
# mls_team_xG_21<-asa$get_team_xgoals(leagues="mls",
#                                     season_name=2021)
# 
# mls_team_goals_added_21<-asa$get_team_goals_added(leagues="mls",
#                                                   season_name=2021)
# 
# mls_teams_21<-mls_teams|>
#   left_join(mls_team_salaries_21|>
#               select(team_id, count_players, total_guaranteed_compensation,
#                      avg_guaranteed_compensation, median_guaranteed_compensation,
#                      std_dev_guaranteed_compensation), by="team_id")
# mls_teams_21<-mls_teams_21|>
#   left_join(mls_team_xG_21,
#             by="team_id")
# 
# mls_teams_21<-mls_teams_21|>
#   left_join(mls_team_goals_added_21, by="team_id")
# 
# mls_teams_21<-mls_teams_21|>
#   select(-team_id, -team_name, -team_short_name, -competition.x, -competition.y)
# 
# mls_teams_21<-mls_teams_21|>
#   mutate(year=2021)
# 
# mls_teams_21<-mls_teams_21|>
#   dplyr::slice(-4, -27)
# 
# mls_team_analysis<-bind_rows(mls_teams_21, mls_teams_22,
#                              mls_teams_23, mls_teams_24)
# 
# mls_team_analysis<-mls_team_analysis|>
#   rename(team=team_abbreviation)
# 
# mls_team_analysis<-mls_team_analysis|>
#   relocate(year, .before=2)
# 
# mls_team_analysis<-mls_team_analysis|>
#   mutate(
#     total_goals_added_for = map_dbl(data, ~sum(.x$goals_added_for, na.rm = TRUE)),
#     total_goals_added_against = map_dbl(data, ~sum(.x$goals_added_against, na.rm = TRUE)),
#     total_num_actions_for = map_dbl(data, ~sum(.x$num_actions_for, na.rm = TRUE)),
#     total_num_actions_against=map_dbl(data, ~sum(.x$num_actions_against, na.rm=TRUE))
#   )
# 
# expanded_team<-mls_team_analysis|>
#   select(team, year, data)|>
#   unnest(data)
# 
# wide_goals_added_team<-expanded_team|>
#   pivot_wider(names_from=action_type,
#               values_from=c(goals_added_for, goals_added_against, num_actions_for, num_actions_against),
#               names_sep="_",
#               values_fn=sum)
# mls_team_analysis<-mls_team_analysis|>
#   select(-data)|>
#   left_join(wide_goals_added_team, by=c("team", "year"))
# 
# 
# mls_players<-asa$get_players(leagues="mls")
# salaries_2024<-asa$get_player_salaries(leagues = "mls", 
#                                        start_date="2024-09-13", 
#                                        end_date="2024-09-14")
# salaries_24<-salaries_2024|>
#   left_join(mls_players|>
#               select(player_id, player_name), by="player_id")|>
#   left_join(mls_teams|>
#               select(team_id, team=team_abbreviation), by="team_id")
# 
# player_goals_added_24<-asa$get_player_goals_added(leagues="mls",
#                                                   minimum_minutes=1000,
#                                                   season_name=2024)
# 
# salaries_24<-salaries_24|>
#   left_join(player_goals_added_24|>
#               select(player_id, general_position, minutes_played, data), 
#             by=c("player_id"))|>
#   select(-player_id, -team_id, -season_name, -mlspa_release, -competition)
# 
# salaries_24<-salaries_24|>
#   mutate(year=2024,
#          team_year = paste(team, year, sep = "_"))
# 
# salaries_24<-salaries_24|>
#   relocate(team_year)
# 
# team_defender_stats_24<-salaries_24|>
#   filter(general_position %in% c("CB", "FB", "DM"))|>
#   group_by(team_year)|>
#   summarise(total_salary_def=sum(guaranteed_compensation),
#             avg_salary_def=total_salary_def/n())
# 
# team_fwd_stats_24<-salaries_24|>
#   filter(general_position %in% c("W", "ST", "CM", "AM"))|>
#   group_by(team_year)|>
#   summarise(total_salary_fwd=sum(guaranteed_compensation),
#             avg_salary_fwd=total_salary_fwd/n())
# 
# # Getting salary break downs positionally for 23
# 
# salaries_2023<-asa$get_player_salaries(leagues = "mls", 
#                                        start_date="2023-09-15", 
#                                        end_date="2023-09-16")
# salaries_23<-salaries_2023|>
#   left_join(mls_players|>
#               select(player_id, player_name), by="player_id")|>
#   left_join(mls_teams|>
#               select(team_id, team=team_abbreviation), by="team_id")
# 
# player_goals_added_23<-asa$get_player_goals_added(leagues="mls",
#                                                   minimum_minutes=1000,
#                                                   season_name=2023)
# 
# salaries_23<-salaries_23|>
#   left_join(player_goals_added_23|>
#               select(player_id, general_position, minutes_played, data), 
#             by=c("player_id"))|>
#   select(-player_id, -team_id, -mlspa_release, -competition)
# 
# salaries_23<-salaries_23|>
#   mutate(team_year = paste(team, season_name, sep = "_"))
# 
# salaries_23<-salaries_23|>
#   relocate(team_year)
# 
# team_defender_stats_23<-salaries_23|>
#   filter(general_position %in% c("CB", "FB", "DM"))|>
#   group_by(team_year)|>
#   summarise(total_salary_def=sum(guaranteed_compensation),
#             avg_salary_def=total_salary_def/n())
# 
# 
# team_fwd_stats_23<-salaries_23|>
#   filter(general_position %in% c("W", "ST", "CM", "AM"))|>
#   group_by(team_year)|>
#   summarise(total_salary_fwd=sum(guaranteed_compensation),
#             avg_salary_fwd=total_salary_fwd/n())
# 
# # Getting salary break downs positionally for 22
# 
# salaries_2022<-asa$get_player_salaries(leagues = "mls", 
#                                        season_name=2022)
# salaries_22<-salaries_2022|>
#   left_join(mls_players|>
#               select(player_id, player_name), by="player_id")|>
#   left_join(mls_teams|>
#               select(team_id, team=team_abbreviation), by="team_id")
# 
# salaries_22<-salaries_22|>
#   filter((team == "MTL" & mlspa_release == "2022-04-15") |
#            (team != "MTL" & mlspa_release == "2022-09-02"))
# 
# player_goals_added_22<-asa$get_player_goals_added(leagues="mls",
#                                                   minimum_minutes=1000,
#                                                   season_name=2022)
# 
# salaries_22<-salaries_22|>
#   left_join(player_goals_added_22|>
#               select(player_id, general_position, minutes_played, data), 
#             by=c("player_id"))|>
#   select(-player_id, -team_id, -mlspa_release, -competition)
# 
# 
# salaries_22<-salaries_22|>
#   mutate(team_year = paste(team, season_name, sep = "_"))
# 
# salaries_22<-salaries_22|>
#   relocate(team_year)
# 
# team_defender_stats_22<-salaries_22|>
#   filter(general_position %in% c("CB", "FB", "DM"))|>
#   group_by(team_year)|>
#   summarise(total_salary_def=sum(guaranteed_compensation),
#             avg_salary_def=total_salary_def/n())
# 
# team_fwd_stats_22<-salaries_22|>
#   filter(general_position %in% c("W", "ST", "CM", "AM"))|>
#   group_by(team_year)|>
#   summarise(total_salary_fwd=sum(guaranteed_compensation),
#             avg_salary_fwd=total_salary_fwd/n())
# 
# # Getting salary breaks downs positionally for 21
# 
# salaries_2021<-asa$get_player_salaries(leagues = "mls", 
#                                        start_date="2021-09-30", 
#                                        end_date="2021-10-01")
# salaries_21<-salaries_2021|>
#   left_join(mls_players|>
#               select(player_id, player_name), by="player_id")|>
#   left_join(mls_teams|>
#               select(team_id, team=team_abbreviation), by="team_id")
# 
# player_goals_added_21<-asa$get_player_goals_added(leagues="mls",
#                                                   minimum_minutes=1000,
#                                                   season_name=2021)
# 
# salaries_21<-salaries_21|>
#   left_join(player_goals_added_21|>
#               select(player_id, general_position, minutes_played, data), 
#             by=c("player_id"))|>
#   select(-player_id, -team_id, -mlspa_release, -competition)
# 
# salaries_21<-salaries_21|>
#   mutate(team_year = paste(team, season_name, sep = "_"))
# 
# salaries_21<-salaries_21|>
#   relocate(team_year)
# 
# team_defender_stats_21<-salaries_21|>
#   filter(general_position %in% c("CB", "FB", "DM"))|>
#   group_by(team_year)|>
#   summarise(total_salary_def=sum(guaranteed_compensation),
#             avg_salary_def=total_salary_def/n())
# 
# team_fwd_stats_21<-salaries_21|>
#   filter(general_position %in% c("W", "ST", "CM", "AM"))|>
#   group_by(team_year)|>
#   summarise(total_salary_fwd=sum(guaranteed_compensation),
#             avg_salary_fwd=total_salary_fwd/n())
# 
# 
# team_def_stats<-bind_rows(team_defender_stats_21, team_defender_stats_22,
#                           team_defender_stats_23, team_defender_stats_24)
# team_fwd_stats<-bind_rows(team_fwd_stats_21, team_fwd_stats_22, 
#                           team_fwd_stats_23, team_fwd_stats_24)
# 
# mls_team_analysis<-mls_team_analysis|>
#   mutate(team_year = paste(team, year, sep = "_"))
# 
# mls_team_analysis<-mls_team_analysis|>
#   left_join(team_def_stats, by="team_year")
# mls_team_analysis<-mls_team_analysis|>
#   left_join(team_fwd_stats, by="team_year")
# 
# mls_team_analysis<-mls_team_analysis|>
#   mutate(ga_per_10k=(total_goals_added_for/total_guaranteed_compensation)*10000)
# 
# mls_team_analysis<-mls_team_analysis|>
#   mutate(fwd_def_spend_ratio=avg_salary_fwd/avg_salary_def)
# 
# write_rds(mls_team_analysis, "mls_team_analysis.rds")

mls_team_analysis <- read_rds("mls_team_analysis.rds")
