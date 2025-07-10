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

## Clustering by xG difference and avg salary

mls_team_analysis|>
  ggplot(aes(avg_guaranteed_compensation))+
  geom_histogram(bins=10)

mls_team_analysis<-mls_team_analysis|>
  mutate(log_avg_salary=log(avg_guaranteed_compensation))

mls_team_analysis|>
  ggplot(aes(log_avg_salary))+
  geom_histogram(bins=10)

mls_simple_cluster_data<-mls_team_analysis|>
  select(team, year, avg_guaranteed_compensation, log_avg_salary,
         xgoal_difference) |>  # domain-relevant
  mutate(team_year = paste(team, year, sep = "_")) |>
  relocate(team_year) |>
  column_to_rownames("team_year") |>
  select(where(is.numeric)) |>
  drop_na()

mls_simple_cluster_data<-mls_simple_cluster_data|>
  mutate(std_avg_salary=as.numeric(scale(avg_guaranteed_compensation, 
                                         center=TRUE, scale=TRUE)),
         std_log_avg_salary=as.numeric(scale(log_avg_salary,
                                             center=TRUE, scale=TRUE)),
         std_xG_diff=as.numeric(scale(xgoal_difference, 
                                      center=TRUE, scale=TRUE)))
mls_simple_cluster_data|>
  select(std_log_avg_salary, std_xG_diff)|>
  fviz_nbclust(kmeans, method = "wss")

mls_simple_kmeans<-mls_simple_cluster_data|>
  select(std_log_avg_salary, std_xG_diff)|>
  kmeans(algorithm="Lloyd", centers=4, nstart=30)

mls_simple_cluster_data|>
  mutate(salary_xG_clusters=as.factor(mls_simple_kmeans$cluster))|>
  ggplot(aes(log_avg_salary, xgoal_difference, color=salary_xG_clusters))+
  geom_point(size=4)+
  ggthemes::scale_color_colorblind()+
  theme(legend.position = "bottom") 

mls_simple_cluster_data<-mls_simple_cluster_data|>
  mutate(salary_xG_clusters=as.factor(mls_simple_kmeans$cluster))

mls_team_analysis<-mls_team_analysis|>
  mutate(team_year = paste(team, year, sep = "_")) |>
  relocate(team_year)

mls_team_analysis<-mls_team_analysis|>
  mutate(salary_xG_clusters=as.factor(mls_simple_kmeans$cluster))|>
  relocate(salary_xG_clusters, .before=2)

mls_cluster_analysis<-mls_team_analysis |>
  group_by(salary_xG_clusters) |>
  summarise(
    mean_xG_diff = mean(xgoal_difference, na.rm = TRUE),
    mean_avg_salary = mean(avg_guaranteed_compensation, na.rm = TRUE),
    mean_total_salary = mean(total_guaranteed_compensation, na.rm = TRUE),
    mean_goals_added_for = mean(total_goals_added_for, na.rm = TRUE),
    mean_goals_added_against = mean(total_goals_added_against, na.rm = TRUE),
    mean_xPoints=mean(xpoints, na.rm=TRUE),
    mean_xG_for=mean(xgoals_for, na.rm=TRUE),
    mean_xG_against=mean(xgoals_against, na.rm=TRUE),
    mean_goals_for=mean(goals_for, na.rm=TRUE),
    mean_goals_against=mean(goals_against, na.rm=TRUE),
    mean_shots_for=mean(shots_for, na.rm=TRUE),
    mean_shots_against=mean(shots_against, na.rm=TRUE),
    n_teams = n()
  ) 

mls_team_analysis|>
  filter(salary_xG_clusters==2)

mls_teams_24<-mls_teams_24|>
  mutate(mean_diff_shots_against=shots_against-mean(shots_against),
         mean_diff_goals_against=goals_against-mean(goals_against),
         mean_diff_xG_against=xgoals_against-mean(xgoals_against))

mls_teams_24|>
  select(team_abbreviation, mean_diff_shots_against, mean_diff_goals_against,
         mean_diff_xG_against)|>
  filter(team_abbreviation %in% c("RSL", "PHI"))

player_salaries_23<-asa$get_player_salaries(leagues="mls", 
                                            season_name=2023)
player_salaries_23<-player_salaries_23|>
  left_join(mls_players|>
              select(player_id, player_name), by="player_id")

player_salaries_23<-player_salaries_23|>
  filter(mlspa_release=="2023-09-15")

player_salaries_23<-player_salaries_23|>
  left_join(mls_teams|>
              select(team_id, team_abbreviation), by="team_id")

player_salaries_23<-player_salaries_23|>
  mutate(team=team_abbreviation)|>
  select(-player_id, -team_id, -season_name, -mlspa_release, -competition, 
         -team_abbreviation)|>
  relocate(player_name, .before=1)|>
  relocate(team, .before=2)

player_salaries_23|>
  filter(position=="D", team=="ORL")



# linear regression for xG difference and goals added for
xg_goals_added_lm<-lm(xgoal_difference~total_goals_added_for, 
                      data=mls_team_analysis)
library(broom)
tidy(xg_goals_added_lm, conf.int = TRUE, conf.level = .95)
glance(xg_goals_added_lm)

mls_team_analysis<-mls_team_analysis|>
  mutate(goals_added_diff=total_goals_added_for-total_goals_added_against)

xgdiff_goals_added_diff_lm<-lm(xgoal_difference~goals_added_diff, 
                      data=mls_team_analysis)

tidy(xgdiff_goals_added_diff_lm, conf.int = TRUE, conf.level = .95)
glance(xgdiff_goals_added_diff_lm)

cor(mls_team_analysis$goals_added_diff, mls_team_analysis$points)^2
