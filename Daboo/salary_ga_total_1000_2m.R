library(itscalledsoccer)
# asa_client <- AmericanSoccerAnalysis$new()
# all_players <- asa_client$players
# 
# 
# ##Data set with each players goals added in 2024
# library(itscalledsoccer)
# library(dplyr)
# library(tidyverse)
# asa <- AmericanSoccerAnalysis$new()
# goals_added2024 <- asa$get_player_goals_added(leagues = "mls", season_name = "2024")
# 
# asa <- AmericanSoccerAnalysis$new()
# goals_added2023 <- asa$get_player_goals_added(leagues = "mls", season_name = "2023")
# 
# asa <- AmericanSoccerAnalysis$new()
# goals_added2022 <- asa$get_player_goals_added(leagues = "mls", season_name = "2022")
# 
# asa <- AmericanSoccerAnalysis$new()
# goals_added2021 <- asa$get_player_goals_added(leagues = "mls", season_name = "2021")
# 
# goals_added_all <- bind_rows(goals_added2023, goals_added2022, goals_added2021)
# 
# 
# 
# 
# ## un vectoring/summing goals added 
# goals_added_all<- goals_added_all |>
#   mutate(
#     total_goals_added_raw = map_dbl(data, ~sum(.x$goals_added_raw, na.rm = TRUE)),
#     total_goals_added_above_avg = map_dbl(data, ~sum(.x$goals_added_above_avg, na.rm = TRUE)),
#     total_count_actions = map_dbl(data, ~sum(.x$count_actions, na.rm = TRUE)))
# 
# 
# ##salary data sets 
# asa <- AmericanSoccerAnalysis$new()
# salary2024 <- asa$get_player_salaries(leagues = "mls",  season_name = "2024") 
# 
# asa <- AmericanSoccerAnalysis$new()
# salary2023 <- asa$get_player_salaries(leagues = "mls",  season_name = "2023")
# 
# asa <- AmericanSoccerAnalysis$new()
# salary2022 <- asa$get_player_salaries(leagues = "mls",  season_name = "2022") 
# 
# asa <- AmericanSoccerAnalysis$new()
# salary2021 <- asa$get_player_salaries(leagues = "mls",  season_name = "2021") 
# 
# salary_all <- bind_rows(salary2023, salary2022, salary2021)
# 
# 
# ##combining data sets one at a time 
# 
# salary_ga2024 <- salary2024  |>
#   left_join(goals_added2024 |> 
#               select(player_id, general_position, minutes_played, data, competition), by = "player_id")
# 
# salary_ga2023 <- salary2023  |>
#   left_join(goals_added2023 |> 
#               select(player_id, general_position, minutes_played, data, competition), by = "player_id") 
# 
# salary_ga2022 <- salary2022  |>
#   left_join(goals_added2022 |> 
#               select(player_id, general_position, minutes_played, data, competition), by = "player_id")
# 
# salary_ga2021 <- salary2021  |>
#   left_join(goals_added2021 |> 
#               select(player_id, general_position, minutes_played, data, competition), by = "player_id")
# 
# 
# salary_ga_total <- bind_rows(salary_ga2024, salary_ga2023, salary_ga2022, salary_ga2021,)
# 
# 
# 
# 
# #Unvectoring 2021-2024 Ga data
# salary_ga_total <- salary_ga_total |>
#   mutate(
#     total_goals_added_raw = map_dbl(data, ~sum(.x$goals_added_raw, na.rm = TRUE)),
#     total_goals_added_above_avg = map_dbl(data, ~sum(.x$goals_added_above_avg, na.rm = TRUE)),
#     total_count_actions = map_dbl(data, ~sum(.x$count_actions, na.rm = TRUE)))
# 
# 
# ##goals added per salary dollar 
# salary_ga_total <- salary_ga_total  |> 
#   mutate(ga_per_10kdollar = (total_goals_added_raw / guaranteed_compensation) *10000)
# 
# salary_ga_total <- salary_ga_total |> 
#   mutate(ga_abv_avg_per_dollar = total_goals_added_above_avg / guaranteed_compensation)
# 
# salary_ga_total <- salary_ga_total |> 
#   mutate(ga_abv_avg_per_10kdollar = ga_abv_avg_per_dollar * 10000)
# 
# ##Adding player names, nations, birthdays, and teams to 2021-2024 data set
# 
# 
# asa <- AmericanSoccerAnalysis$new()
# players_total <- asa$get_players(leagues = "mls")
# 
# salary_ga_total <- salary_ga_total |>
#   left_join(players_total |> 
#               select(player_id, player_name, nationality, birth_date,), by = "player_id") 
# 
# ##Adding Goals added per 90 
# salary_ga_total <- salary_ga_total  |> 
#   mutate(ga_per_90= ((total_goals_added_raw / minutes_played) * 90))
# 
# 
# 
# 
# ##Adding Goals added above average per 90  
# salary_ga_total <- salary_ga_total  |> 
#   mutate(ga_abv_avg_per_90= ((total_goals_added_above_avg / minutes_played) * 90))
# 
# ##Adding goals added per 90 per 10k dollar
# salary_ga_total <- salary_ga_total  |> 
#   mutate(ga_per_90_per10k= ((ga_per_90 / guaranteed_compensation) * 10000))
# 
# 
# 
# ##Adding goals added above average per 10k dollar 
# salary_ga_total <- salary_ga_total  |> 
#   mutate(ga_abv_avg_per_90_per10k= ((ga_abv_avg_per_90 / guaranteed_compensation) * 10000))
# 
# 
# 
# 
# salary_ga_total_1000_2m <- salary_ga_total |>
#   filter(minutes_played > 1000, guaranteed_compensation <= 2000000) 
# 
# 
# 
# 
# ##Cleaning nationality data
# 
# library(dplyr)
# 
# salary_ga_total_1000_2m <-  salary_ga_total_1000_2m |> 
#   mutate(
#     nationality = case_when(
#       player_name == "Brandon Vázquez" ~ "USA",
#       player_name == "Noah Cobb" ~ "USA",
#       player_name == "Jasper Löffelsend" ~ "Germany", 
#       player_name == "Patrick Agyemang" ~ "USA",
#       TRUE ~ nationality
#     ))
# 
# ##More data cleaning for jasper Loefeelsend 
# 
# ##install.packages("stringr")
# ##install.packages("stringi")  
# library(stringr)
# library(stringi)
# salary_ga_total_1000_2m <- salary_ga_total_1000_2m |>
#   mutate(
#     nationality = case_when(
#       str_detect(
#         stri_trans_general(player_name, "Latin-ASCII"), 
#         fixed("Jasper Loeffelsend")
#       ) ~ "Germany",
#       TRUE ~ nationality
#     ))
# 
# 
# library(stringr)
# library(stringi)
# salary_ga_total_1000_2m <- salary_ga_total_1000_2m |>
#   mutate(
#     nationality = case_when(
#       str_detect(
#         stri_trans_general(player_name, "Latin-ASCII"), 
#         fixed("Jasper Loeffelsend")
#       ) ~ "Germany",
#       TRUE ~ nationality
#     ))
# 
# 
# 
# library(stringr)
# library(stringi)
# 
# 
# 
# ## International vs domestic 
# library(dplyr)
# salary_ga_total_1000_2m <- salary_ga_total_1000_2m |>
#   mutate(
#     international = if_else(!(nationality %in% c("USA", "Canada")), 1, 0)
#   )
# 
# 
# ##Creating 6 region based nationality bins 
# salary_ga_total_1000_2m <- salary_ga_total_1000_2m |>
#   mutate(
#     region_group = case_when(
#       nationality %in% c("USA", "Canada") ~ "Domestic",
#       nationality %in% c("Argentina", "Brazil", "Uruguay", "Venezuela", "Colombia",
#                          "Ecuador", "Paraguay", "Peru", "Chile") ~ "South America",
#       nationality %in% c("Costa Rica", "Honduras", "Mexico", "Panama", "Jamaica",
#                          "Haiti", "Cuba", "El Salvador", "Trinidad & Tobago", "Curaçao") ~ "Central America/Caribbean",
#       nationality %in% c("France", "Germany", "Spain", "England", "Finland", "Norway",
#                          "Portugal", "Republic of Ireland", "Sweden", "Denmark", "Scotland",
#                          "Serbia", "Poland", "Hungary", "Italy", "Croatia", "Iceland",
#                          "Slovakia", "Slovenia", "Austria", "Netherlands", "Greece", "Belgium",
#                          "Luxembourg", "Switzerland", "Ukraine", "Albania", "Czech Republic",
#                          "Romania", "Bosnia and Herzegovina", "Georgia", "Russia") ~ "Europe",
#       nationality %in% c("Ghana", "Nigeria", "Cameroon", "DR Congo", "Senegal", "Liberia",
#                          "Mali", "Republic of Cape Verde", "Sierra Leone", "Zimbabwe",
#                          "Equatorial Guinea", "Ivory Coast", "Madagascar", "Tanzania",
#                          "Zambia", "Algeria", "Angola", "Kenya", "Libya", "South Africa",
#                          "Guinea-Bissau", "Gabon") ~ "Africa",
#       nationality %in% c("Japan", "New Zealand", "Australia", "South Korea", "Iraq",
#                          "Israel", "Iran") ~ "Asia/Oceania", 
#       TRUE ~ "Other/Unknown"))
# 
# 
# 
# 
# 
# 
# 
# #Turning birthdate into an age variable
# 
# library(dplyr)
# library(lubridate)
# 
# 
# 
# salary_ga_total_1000_2m <- salary_ga_total_1000_2m |>
#   mutate(
#     birth_date = as.Date(birth_date),
#     mlspa_release = as.Date(mlspa_release),  
#     age = as.integer(floor(interval(birth_date, mlspa_release) / years(1)))
#   )
# 
# ##Permanently filtering data for correct MLSpa relase dates 
# salary_ga_total_1000_2m <- salary_ga_total_1000_2m |>
#   filter(mlspa_release %in% c("2024-09-13", "2023-09-15", "2022-09-02", "2021-09-30"))
# 
# write_rds(salary_ga_total_1000_2m, "salary_ga_total_1000_2m.rds")

salary_ga_total_1000_2m <- read_rds("salary_ga_total_1000_2m.rds")
