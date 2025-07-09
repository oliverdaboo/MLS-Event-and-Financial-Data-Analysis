library(tidyverse)
library(ggplot2)
library(itscalledsoccer)
itscalledsoccer::AmericanSoccerAnalysis

asa<-AmericanSoccerAnalysis$new()

# constructing the main data set

mls_teams<-mls_teams|>
  slice(-6, -25)

mls_team_salaries_24<-asa$get_team_salaries(leagues = "mls",
                                            season_name=2024)
mls_team_xG_24<-asa$get_team_xgoals(leagues="mls",
                                    season_name=2024)

mls_team_goals_added_24<-asa$get_team_goals_added(leagues="mls",
                                                  season_name=2024)

mls_teams_24<-mls_teams|>
  left_join(mls_team_salaries_24|>
              select(team_id, count_players, total_guaranteed_compensation,
                     avg_guaranteed_compensation, median_guaranteed_compensation,
                     std_dev_guaranteed_compensation), by="team_id")
mls_teams_24<-mls_teams_24|>
  left_join(mls_team_xG_24,
            by="team_id")

mls_teams_24<-mls_teams_24|>
  left_join(mls_team_goals_added_24, by="team_id")

mls_teams_24<-mls_teams_24|>
  select(-team_id, -team_name, -team_short_name, -competition.x, -competition.y)

mls_teams_24<-mls_teams_24|>
  mutate(year=2024)

mls_team_salaries_23<-asa$get_team_salaries(leagues = "mls",
                                            season_name=2023)
mls_team_xG_23<-asa$get_team_xgoals(leagues="mls",
                                    season_name=2023)

mls_team_goals_added_23<-asa$get_team_goals_added(leagues="mls",
                                                  season_name=2023)

mls_teams_23<-mls_teams|>
  left_join(mls_team_salaries_23|>
              select(team_id, count_players, total_guaranteed_compensation,
                     avg_guaranteed_compensation, median_guaranteed_compensation,
                     std_dev_guaranteed_compensation), by="team_id")
mls_teams_23<-mls_teams_23|>
  left_join(mls_team_xG_23,
            by="team_id")

mls_teams_23<-mls_teams_23|>
  left_join(mls_team_goals_added_23, by="team_id")

mls_teams_23<-mls_teams_23|>
  select(-team_id, -team_name, -team_short_name, -competition.x, -competition.y)

mls_teams_23<-mls_teams_23|>
  mutate(year=2023)

mls_team_salaries_22<-asa$get_team_salaries(leagues = "mls",
                                            season_name=2022)
mls_team_xG_22<-asa$get_team_xgoals(leagues="mls",
                                    season_name=2022)

mls_team_goals_added_22<-asa$get_team_goals_added(leagues="mls",
                                                  season_name=2022)

mls_teams_22<-mls_teams|>
  left_join(mls_team_salaries_22|>
              select(team_id, count_players, total_guaranteed_compensation,
                     avg_guaranteed_compensation, median_guaranteed_compensation,
                     std_dev_guaranteed_compensation), by="team_id")
mls_teams_22<-mls_teams_22|>
  left_join(mls_team_xG_22,
            by="team_id")

mls_teams_22<-mls_teams_22|>
  left_join(mls_team_goals_added_22, by="team_id")

mls_teams_22<-mls_teams_22|>
  select(-team_id, -team_name, -team_short_name, -competition.x, -competition.y)

mls_teams_22<-mls_teams_22|>
  mutate(year=2022)

mls_teams_22<-mls_teams_22|>
  slice(-27)

mls_team_salaries_21<-asa$get_team_salaries(leagues = "mls",
                                            season_name=2021)
mls_team_xG_21<-asa$get_team_xgoals(leagues="mls",
                                    season_name=2021)

mls_team_goals_added_21<-asa$get_team_goals_added(leagues="mls",
                                                  season_name=2021)

mls_teams_21<-mls_teams|>
  left_join(mls_team_salaries_21|>
              select(team_id, count_players, total_guaranteed_compensation,
                     avg_guaranteed_compensation, median_guaranteed_compensation,
                     std_dev_guaranteed_compensation), by="team_id")
mls_teams_21<-mls_teams_21|>
  left_join(mls_team_xG_21,
            by="team_id")

mls_teams_21<-mls_teams_21|>
  left_join(mls_team_goals_added_21, by="team_id")

mls_teams_21<-mls_teams_21|>
  select(-team_id, -team_name, -team_short_name, -competition.x, -competition.y)

mls_teams_21<-mls_teams_21|>
  mutate(year=2021)

mls_teams_21<-mls_teams_21|>
  slice(-4, -27)

mls_team_analysis<-bind_rows(mls_teams_21, mls_teams_22, 
                             mls_teams_23, mls_teams_24)

mls_team_analysis<-mls_team_analysis|>
  rename(team=team_abbreviation)

mls_team_analysis<-mls_team_analysis|>
  relocate(year, .before=2)

mls_team_analysis<-mls_team_analysis|>
  mutate(
    total_goals_added_for = map_dbl(data, ~sum(.x$goals_added_for, na.rm = TRUE)),
    total_goals_added_against = map_dbl(data, ~sum(.x$goals_added_against, na.rm = TRUE)),
    total_num_actions_for = map_dbl(data, ~sum(.x$num_actions_for, na.rm = TRUE)),
    total_num_actions_against=map_dbl(data, ~sum(.x$num_actions_against, na.rm=TRUE))
  )

expanded_team<-mls_team_analysis|>
  select(team, year, data)|>
  unnest(data)

wide_goals_added_team<-expanded_team|>
  pivot_wider(names_from=action_type,
              values_from=c(goals_added_for, goals_added_against, num_actions_for, num_actions_against),
              names_sep="_",
              values_fn=sum)
mls_team_analysis<-mls_team_analysis|>
  select(-data)|>
  left_join(wide_goals_added_team, by=c("team", "year"))

cor(mls_team_analysis$avg_guaranteed_compensation, 
    mls_team_analysis$xgoal_difference)

cor(mls_team_analysis$total_guaranteed_compensation, 
    mls_team_analysis$xgoal_difference)

# k-means clustering teams
library(cluster)
library(factoextra)
ga_features <- mls_team_analysis |>
  select(team, year, starts_with("goals_added_for_")) |>
  group_by(team) |>
  summarise(across(starts_with("goals_added_for_"), mean, na.rm = TRUE))

ga_matrix <- ga_features |>
  column_to_rownames("team") |>
  scale()

kmeans_result <- kmeans(ga_matrix, centers = 3)

fviz_cluster(kmeans_result, data = ga_matrix, labelsize = 10)

# k-means clustering each year
team_year_ids <- mls_team_analysis |> 
  mutate(team_year = paste(team, year, sep = "_")) |> 
  pull(team_year)

cluster_data <- mls_team_analysis |>
  mutate(team_year = paste(team, year, sep = "_")) |>
  select(-team) |>  # remove non-numeric/data columns
  column_to_rownames("team_year") |> 
  select(where(is.numeric)) |> 
  drop_na()

scaled_data <- scale(cluster_data)

fviz_nbclust(scaled_data, kmeans, method = "wss")

set.seed(123)
kmeans_result <- kmeans(scaled_data, centers = 4, nstart = 25)

fviz_cluster(kmeans_result, data = scaled_data, 
             repel = TRUE, 
             labelsize = 8,
             main = "K-Means Clustering of Team-Year Observations")

mls_team_analysis <- mls_team_analysis |>
  mutate(cluster = kmeans_result$cluster)

mls_team_analysis<-mls_team_analysis|>
  relocate(cluster, .before=2)

mls_cluster_data <- mls_team_analysis |>
  select(team, year, total_goals_added_for, total_goals_added_against,
         total_guaranteed_compensation, avg_guaranteed_compensation,
         xgoal_difference) |>  # domain-relevant
  mutate(team_year = paste(team, year, sep = "_")) |>
  relocate(team_year) |>
  column_to_rownames("team_year") |>
  select(where(is.numeric)) |>
  drop_na()

scaled_data <- scale(mls_cluster_data)

fviz_nbclust(scaled_data, kmeans, method = "wss")

set.seed(123)
kmeans_result <- kmeans(scaled_data, centers = 4, nstart = 25)

fviz_cluster(kmeans_result, data = scaled_data, 
             repel = TRUE, 
             labelsize = 8,
             main = "K-Means Clustering of Team-Year Observations")

mls_team_analysis <- mls_team_analysis |>
  mutate(cluster = kmeans_result$cluster)

mls_team_analysis<-mls_team_analysis|>
  slice(-49)
## linear regression

# looking at distribution of variables

mls_team_analysis|>
  ggplot(aes(total_goals_added_against))+
  geom_histogram(bins=10)

total_guranteed_lm<-lm(xgoal_difference~total_guaranteed_compensation, 
                       data=mls_team_analysis)
library(broom)
tidy(total_guranteed_lm, conf.int = TRUE, conf.level = .95)

avg_guranteed_lm<-lm(xgoal_difference~avg_guaranteed_compensation, 
                     data=mls_team_analysis)
tidy(avg_guranteed_lm, conf.int = TRUE, conf.level = .95)

