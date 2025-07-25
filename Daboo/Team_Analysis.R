library(tidyverse)
library(ggplot2)
library(itscalledsoccer)
itscalledsoccer::AmericanSoccerAnalysis

asa<-AmericanSoccerAnalysis$new()

# constructing the main data set
mls_teams<-asa$get_teams(leagues="mls")
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


## Clustering by xG difference and log avg salary

mls_team_analysis|>
  ggplot(aes(avg_guaranteed_compensation))+
  geom_histogram(bins=10)

mls_team_analysis<-mls_team_analysis|>
  mutate(log_avg_salary=log(avg_guaranteed_compensation))

mls_team_analysis<-mls_team_analysis|>
  mutate(sqrt_avg_salary=sqrt(avg_guaranteed_compensation))

mls_team_analysis|>
  ggplot(aes(sqrt_avg_salary))+
  geom_histogram(bins=8)

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

mls_simple_kmeans_log<-mls_simple_cluster_data|>
  select(std_log_avg_salary, std_xG_diff)|>
  kmeans(algorithm="Lloyd", centers=4, nstart=30)

mls_simple_cluster_data|>
  mutate(salary_xG_clusters_log=factor(mls_simple_kmeans_log$cluster, 
                                       labels=c("1", "2", "3", "4")),
         team_year=rownames(mls_simple_cluster_data))|>
  ggplot(aes(avg_guaranteed_compensation, xgoal_difference, 
             color=salary_xG_clusters_log))+
  geom_point(size=4)+
  geom_text(aes(label=team_year), size=3, show.legend=FALSE, vjust=-1)+
  ggthemes::scale_color_colorblind()+
  labs(title = "k-Means Clustering of Salary and xG Difference for the MLS 2021-2024", 
       x="Average Guaranteed Compensation",
       y="xG Difference",
       color="Clusters")+
  theme(legend.position = "bottom", plot.title=element_text(hjust=.5))+
  theme_minimal()


mls_simple_cluster_data<-mls_simple_cluster_data|>
  mutate(salary_xG_clusters_log=as.factor(mls_simple_kmeans_log$cluster))

mls_team_analysis<-mls_team_analysis|>
  mutate(team_year = paste(team, year, sep = "_")) |>
  relocate(team_year)

mls_team_analysis<-mls_team_analysis|>
  mutate(salary_xG_clusters_log=as.factor(mls_simple_kmeans_log$cluster))|>
  relocate(salary_xG_clusters_log, .before=2)

mls_cluster_analysis<-mls_team_analysis |>
  group_by(salary_xG_clusters_log) |>
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
    mean_goals_added_for_claiming=mean(goals_added_for_Claiming, na.rm=TRUE),
    mean_goals_added_for_interrupting=mean(goals_added_for_Interrupting, na.rm=TRUE),
    mean_goals_added_for_fouling=mean(goals_added_for_Fouling, na.rm=TRUE),
    mean_goals_added_against_shooting=mean(goals_added_against_Shooting, na.rm=TRUE),
    n_teams = n()
  ) 

# ANOVA to see which defensive group means are different

anova_variables<-c("total_goals_added_against", "xgoals_against",
                   "goals_against", "shots_against", 
                   "goals_added_against_Shooting",
                   "goals_added_for_Interrupting",
                   "goals_added_for_Claiming")
simple_cluster_anova<-lapply(anova_variables, function(var){
  formula<-as.formula(paste(var, "~ salary_xG_clusters_log"))
  model<-aov(formula, data=mls_team_analysis)
  summary_results<-summary(model)[[1]]
  data.frame(variable=var,
             F_value=summary_results$'F value'[1],
             p_value=summary_results$'Pr(>F)'[1])
})

anova_df<-do.call(rbind, simple_cluster_anova)
anova_df<-anova_df[order(anova_df$p_value), ]
print(anova_df)

# Tukey-HSD to see which groups cluster 2 differs from cluster 3
significant_variables<-c("total_goals_added_against", "xgoals_against",
                         "goals_against", "shots_against", 
                         "goals_added_against_Shooting")

simple_cluster_tukey<-function(var, data){
  formula<-as.formula(paste(var, "~ salary_xG_clusters_log"))
  aov_model<-aov(formula, data=data)
  tukey<-TukeyHSD(aov_model)
  
  tukey_df <- as.data.frame(tukey[[1]])
  tukey_df$Comparison <- rownames(tukey_df)
  
  filter_row <- grep("2-3|3-2", tukey_df$Comparison)
  if (length(filter_row) > 0) {
    result <- tukey_df[filter_row, ]
    result$Variable <- var
    return(result)
  } else {
    return(NULL)
  }
}

tukey_results <- lapply(significant_variables, simple_cluster_tukey, 
                        data = mls_team_analysis)

# Combine all results into one dataframe
tukey_23_df <- do.call(rbind, tukey_results)

# Clean up and reorder columns
tukey_23_df <- tukey_23_df %>%
  select(Variable, Comparison, diff, lwr, upr, `p adj`) %>%
  rename(
    Difference = diff,
    Lower_CI = lwr,
    Upper_CI = upr,
    Adjusted_p = `p adj`
  )

print(tukey_23_df)

# Getting salary break downs positionally for 24
mls_players<-asa$get_players(leagues="mls")
salaries_2024<-asa$get_player_salaries(leagues = "mls", 
                                       start_date="2024-09-13", 
                                       end_date="2024-09-14")
salaries_24<-salaries_2024|>
  left_join(mls_players|>
              select(player_id, player_name), by="player_id")|>
  left_join(mls_teams|>
              select(team_id, team=team_abbreviation), by="team_id")

player_goals_added_24<-asa$get_player_goals_added(leagues="mls",
                                                  minimum_minutes=1000,
                                                  season_name=2024)
player_goals_added_24<-player_goals_added_24|>
  left_join(mls_teams|>
              select(team_abbreviation, team_id), by="team_id")

salaries_24<-salaries_24|>
  left_join(player_goals_added_24|>
              select(player_id, general_position, minutes_played, data), 
            by=c("player_id"))|>
  select(-player_id, -team_id, -season_name, -mlspa_release, -competition)

salaries_24<-salaries_24|>
  drop_na()|>
  mutate(goals_added=data)|>
  select(-data)

salaries_24<-salaries_24|>
  mutate(
    total_goals_added_raw = map_dbl(goals_added, ~sum(.x$goals_added_raw, na.rm = TRUE)),
    total_goals_added_above_avg = map_dbl(goals_added, ~sum(.x$goals_added_above_avg, na.rm = TRUE)),
    total_count_actions = map_dbl(goals_added, ~sum(.x$count_actions, na.rm = TRUE))
  )

# adding the goals_added details to the data set
expanded<-salaries_24|>
  select(player_name, goals_added)|>
  unnest(goals_added)
wide_goals_added<-expanded|>
  pivot_wider(names_from=action_type,
              values_from=c(goals_added_raw, goals_added_above_avg, count_actions),
              names_sep="_")

salaries_24<-salaries_24|>
  select(-goals_added)|>
  left_join(wide_goals_added, by="player_name")

salaries_24<-salaries_24|>
  mutate(ga_90=(total_goals_added_raw/minutes_played)*90,
         ga_avg_90=(total_goals_added_above_avg/minutes_played)*90,
         ga_90_10k=(ga_90/guaranteed_compensation)*10000,
         ga_avg_90_10k=(ga_avg_90/guaranteed_compensation)*10000)

salaries_24|>
  filter(team=="PHI", general_position %in% c("CB", "FB"))

salaries_24|>
  filter(team=="RSL", general_position %in% c("CB", "FB"))

salaries_24<-salaries_24|>
  mutate(year=2024,
         team_year = paste(team, year, sep = "_"))

salaries_24<-salaries_24|>
  relocate(team_year)

team_defender_stats_24<-salaries_24|>
  filter(general_position %in% c("CB", "FB", "DM"))|>
  group_by(team_year)|>
  summarise(total_salary_def=sum(guaranteed_compensation),
            avg_salary_def=total_salary_def/n(),
            total_ga_90_def=sum(ga_90),
            avg_ga_90_def=total_ga_90_def/n(),
            total_ga_90_10k_def=sum(ga_90_10k),
            avg_ga_90_10k_def=total_ga_90_10k_def/n())

team_fwd_stats_24<-salaries_24|>
  filter(general_position %in% c("W", "ST", "CM", "AM"))|>
  group_by(team_year)|>
  summarise(total_salary_fwd=sum(guaranteed_compensation),
            avg_salary_fwd=total_salary_fwd/n(),
            total_ga_90_fwd=sum(ga_90),
            avg_ga_90_fwd=total_ga_90_fwd/n(),
            total_ga_90_10k_fwd=sum(ga_90_10k),
            avg_ga_90_10k_fwd=total_ga_90_10k_fwd/n())

mls_team_analysis<-mls_team_analysis|>
  left_join(team_fwd_stats_24,
            by="team_year")

# Getting salary break downs positionally for 23

salaries_2023<-asa$get_player_salaries(leagues = "mls", 
                                       start_date="2023-09-15", 
                                       end_date="2023-09-16")
salaries_23<-salaries_2023|>
  left_join(mls_players|>
              select(player_id, player_name), by="player_id")|>
  left_join(mls_teams|>
              select(team_id, team=team_abbreviation), by="team_id")

player_goals_added_23<-asa$get_player_goals_added(leagues="mls",
                                                  minimum_minutes=1000,
                                                  season_name=2023)

salaries_23<-salaries_23|>
  left_join(player_goals_added_23|>
              select(player_id, general_position, minutes_played, data), 
            by=c("player_id"))|>
  select(-player_id, -team_id, -mlspa_release, -competition)

salaries_23<-salaries_23|>
  drop_na()|>
  mutate(goals_added=data)|>
  select(-data)

salaries_23<-salaries_23|>
  mutate(
    total_goals_added_raw = map_dbl(goals_added, ~sum(.x$goals_added_raw, na.rm = TRUE)),
    total_goals_added_above_avg = map_dbl(goals_added, ~sum(.x$goals_added_above_avg, na.rm = TRUE)),
    total_count_actions = map_dbl(goals_added, ~sum(.x$count_actions, na.rm = TRUE))
  )

# adding the goals_added details to the data set
expanded<-salaries_23|>
  select(player_name, goals_added)|>
  unnest(goals_added)
wide_goals_added<-expanded|>
  pivot_wider(names_from=action_type,
              values_from=c(goals_added_raw, goals_added_above_avg, count_actions),
              names_sep="_")

salaries_23<-salaries_23|>
  select(-goals_added)|>
  left_join(wide_goals_added, by="player_name")

salaries_23<-salaries_23|>
  mutate(ga_90=(total_goals_added_raw/minutes_played)*90,
         ga_avg_90=(total_goals_added_above_avg/minutes_played)*90,
         ga_90_10k=(ga_90/guaranteed_compensation)*10000,
         ga_avg_90_10k=(ga_avg_90/guaranteed_compensation)*10000)

salaries_23<-salaries_23|>
  mutate(team_year = paste(team, season_name, sep = "_"))

salaries_23<-salaries_23|>
  relocate(team_year)

team_defender_stats_23<-salaries_23|>
  filter(general_position %in% c("CB", "FB", "DM"))|>
  group_by(team_year)|>
  summarise(total_salary_def=sum(guaranteed_compensation),
            avg_salary_def=total_salary_def/n(),
            total_ga_90_def=sum(ga_90),
            avg_ga_90_def=total_ga_90_def/n(),
            total_ga_90_10k_def=sum(ga_90_10k),
            avg_ga_90_10k_def=total_ga_90_10k_def/n())


team_fwd_stats_23<-salaries_23|>
  filter(general_position %in% c("W", "ST", "CM", "AM"))|>
  group_by(team_year)|>
  summarise(total_salary_fwd=sum(guaranteed_compensation),
            avg_salary_fwd=total_salary_fwd/n(),
            total_ga_90_fwd=sum(ga_90),
            avg_ga_90_fwd=total_ga_90_fwd/n(),
            total_ga_90_10k_fwd=sum(ga_90_10k),
            avg_ga_90_10k_fwd=total_ga_90_10k_fwd/n())

# Getting salary break downs positionally for 22

salaries_2022<-asa$get_player_salaries(leagues = "mls", 
                                       season_name=2022)
salaries_22<-salaries_2022|>
  left_join(mls_players|>
              select(player_id, player_name), by="player_id")|>
  left_join(mls_teams|>
              select(team_id, team=team_abbreviation), by="team_id")

salaries_22<-salaries_22|>
  filter((team == "MTL" & mlspa_release == "2022-04-15") |
           (team != "MTL" & mlspa_release == "2022-09-02"))

player_goals_added_22<-asa$get_player_goals_added(leagues="mls",
                                                  minimum_minutes=1000,
                                                  season_name=2022)

salaries_22<-salaries_22|>
  left_join(player_goals_added_22|>
              select(player_id, general_position, minutes_played, data), 
            by=c("player_id"))|>
  select(-player_id, -team_id, -mlspa_release, -competition)

salaries_22<-salaries_22|>
  drop_na()|>
  mutate(goals_added=data)|>
  select(-data)

salaries_22<-salaries_22|>
  mutate(
    total_goals_added_raw = map_dbl(goals_added, ~sum(.x$goals_added_raw, na.rm = TRUE)),
    total_goals_added_above_avg = map_dbl(goals_added, ~sum(.x$goals_added_above_avg, na.rm = TRUE)),
    total_count_actions = map_dbl(goals_added, ~sum(.x$count_actions, na.rm = TRUE))
  )

# adding the goals_added details to the data set
expanded<-salaries_22|>
  select(player_name, goals_added)|>
  unnest(goals_added)
wide_goals_added<-expanded|>
  pivot_wider(names_from=action_type,
              values_from=c(goals_added_raw, goals_added_above_avg, count_actions),
              names_sep="_")

salaries_22<-salaries_22|>
  select(-goals_added)|>
  left_join(wide_goals_added, by="player_name")

salaries_22<-salaries_22|>
  mutate(ga_90=(total_goals_added_raw/minutes_played)*90,
         ga_avg_90=(total_goals_added_above_avg/minutes_played)*90,
         ga_90_10k=(ga_90/guaranteed_compensation)*10000,
         ga_avg_90_10k=(ga_avg_90/guaranteed_compensation)*10000)

salaries_22<-salaries_22|>
  mutate(team_year = paste(team, season_name, sep = "_"))

salaries_22<-salaries_22|>
  relocate(team_year)

team_defender_stats_22<-salaries_22|>
  filter(general_position %in% c("CB", "FB", "DM"))|>
  group_by(team_year)|>
  summarise(total_salary_def=sum(guaranteed_compensation),
            avg_salary_def=total_salary_def/n(),
            total_ga_90_def=sum(ga_90),
            avg_ga_90_def=total_ga_90_def/n(),
            total_ga_90_10k_def=sum(ga_90_10k),
            avg_ga_90_10k_def=total_ga_90_10k_def/n())

team_fwd_stats_22<-salaries_22|>
  filter(general_position %in% c("W", "ST", "CM", "AM"))|>
  group_by(team_year)|>
  summarise(total_salary_fwd=sum(guaranteed_compensation),
            avg_salary_fwd=total_salary_fwd/n(),
            total_ga_90_fwd=sum(ga_90),
            avg_ga_90_fwd=total_ga_90_fwd/n(),
            total_ga_90_10k_fwd=sum(ga_90_10k),
            avg_ga_90_10k_fwd=total_ga_90_10k_fwd/n())

# Getting salary breaks downs positionally for 21

salaries_2021<-asa$get_player_salaries(leagues = "mls", 
                                       start_date="2021-09-30", 
                                       end_date="2021-10-01")
salaries_21<-salaries_2021|>
  left_join(mls_players|>
              select(player_id, player_name), by="player_id")|>
  left_join(mls_teams|>
              select(team_id, team=team_abbreviation), by="team_id")

player_goals_added_21<-asa$get_player_goals_added(leagues="mls",
                                                  minimum_minutes=1000,
                                                  season_name=2021)

salaries_21<-salaries_21|>
  left_join(player_goals_added_21|>
              select(player_id, general_position, minutes_played, data), 
            by=c("player_id"))|>
  select(-player_id, -team_id, -mlspa_release, -competition)

salaries_21<-salaries_21|>
  drop_na()|>
  mutate(goals_added=data)|>
  select(-data)

salaries_21<-salaries_21|>
  mutate(
    total_goals_added_raw = map_dbl(goals_added, ~sum(.x$goals_added_raw, na.rm = TRUE)),
    total_goals_added_above_avg = map_dbl(goals_added, ~sum(.x$goals_added_above_avg, na.rm = TRUE)),
    total_count_actions = map_dbl(goals_added, ~sum(.x$count_actions, na.rm = TRUE))
  )

# adding the goals_added details to the data set
expanded<-salaries_21|>
  select(player_name, goals_added)|>
  unnest(goals_added)
wide_goals_added<-expanded|>
  pivot_wider(names_from=action_type,
              values_from=c(goals_added_raw, goals_added_above_avg, count_actions),
              names_sep="_")

salaries_21<-salaries_21|>
  select(-goals_added)|>
  left_join(wide_goals_added, by="player_name")

salaries_21<-salaries_21|>
  mutate(ga_90=(total_goals_added_raw/minutes_played)*90,
         ga_avg_90=(total_goals_added_above_avg/minutes_played)*90,
         ga_90_10k=(ga_90/guaranteed_compensation)*10000,
         ga_avg_90_10k=(ga_avg_90/guaranteed_compensation)*10000)

salaries_21<-salaries_21|>
  mutate(team_year = paste(team, season_name, sep = "_"))

salaries_21<-salaries_21|>
  relocate(team_year)

team_defender_stats_21<-salaries_21|>
  filter(general_position %in% c("CB", "FB", "DM"))|>
  group_by(team_year)|>
  summarise(total_salary_def=sum(guaranteed_compensation),
            avg_salary_def=total_salary_def/n(),
            total_ga_90_def=sum(ga_90),
            avg_ga_90_def=total_ga_90_def/n(),
            total_ga_90_10k_def=sum(ga_90_10k),
            avg_ga_90_10k_def=total_ga_90_10k_def/n())


team_fwd_stats_21<-salaries_21|>
  filter(general_position %in% c("W", "ST", "CM", "AM"))|>
  group_by(team_year)|>
  summarise(total_salary_fwd=sum(guaranteed_compensation),
            avg_salary_fwd=total_salary_fwd/n(),
            total_ga_90_fwd=sum(ga_90),
            avg_ga_90_fwd=total_ga_90_fwd/n(),
            total_ga_90_10k_fwd=sum(ga_90_10k),
            avg_ga_90_10k_fwd=total_ga_90_10k_fwd/n())

# adding positional salary stats to team analysis
mls_team_analysis<-mls_team_analysis|>
  select(-log_avg_salary_1, -sqrt_avg_salary, -total_salary_def.x, 
         -total_ga_90_def.x, -total_ga_90_10k_def.x, -total_salary_mid, 
         -total_salary_def.y, -total_salary_fwd, -total_ga_90_def.y, 
         -total_ga_90_mid, -total_ga_90_fwd, -total_ga_90_10k_def.y, 
         -total_ga_90_10k_mid, -total_ga_90_10k_fwd, -avg_salary_def.x, 
         -avg_ga_90_def.x, -avg_ga_90_10k_def.x, -avg_salary_mid, 
         -avg_salary_fwd, -avg_salary_def.y, -avg_ga_90_mid, -avg_ga_90_fwd,
         -avg_ga_90_def.y, -avg_ga_90_10k_mid, -avg_ga_90_10k_fwd, 
         -avg_ga_90_10k_def.y)

team_def_stats<-bind_rows(team_defender_stats_21, team_defender_stats_22,
                               team_defender_stats_23, team_defender_stats_24)
team_fwd_stats<-bind_rows(team_fwd_stats_21, team_fwd_stats_22, 
                          team_fwd_stats_23, team_fwd_stats_24)

mls_team_analysis<-mls_team_analysis|>
  select(-total_salary_def, -total_ga_90_def, -total_ga_90_10k_def, 
         -avg_salary_def, -avg_ga_90_def, -avg_ga_90_10k_def,
         -total_salary_mid, -total_ga_90_mid, -total_ga_90_10k_mid,
         -avg_salary_mid, -avg_ga_90_mid, -avg_ga_90_10k_mid,
         -total_salary_fwd, -total_ga_90_fwd, -total_ga_90_10k_fwd,
         -avg_salary_fwd, -avg_ga_90_fwd, -avg_ga_90_10k_fwd)

mls_team_analysis<-mls_team_analysis|>
  left_join(team_def_stats, by="team_year")
mls_team_analysis<-mls_team_analysis|>
  left_join(team_fwd_stats, by="team_year")

mls_team_analysis<-mls_team_analysis|>
  mutate(ga_per_10k=(total_goals_added_for/total_guaranteed_compensation)*10000)

mls_cluster_analysis<-mls_team_analysis |>
  group_by(salary_xG_clusters_log) |>
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
    mean_goals_added_for_claiming=mean(goals_added_for_Claiming, na.rm=TRUE),
    mean_goals_added_for_interrupting=mean(goals_added_for_Interrupting, na.rm=TRUE),
    mean_goals_added_for_fouling=mean(goals_added_for_Fouling, na.rm=TRUE),
    mean_goals_added_against_shooting=mean(goals_added_against_Shooting, na.rm=TRUE),
    mean_avg_salary_def=mean(avg_salary_def),
    mean_avg_ga_90_def=mean(avg_ga_90_def),
    mean_avg_ga_90_10k_def=mean(avg_ga_90_10k_def),
    mean_avg_salary_fwd=mean(avg_salary_fwd),
    mean_avg_ga_90_fwd=mean(avg_ga_90_fwd),
    mean_avg_ga_90_10k_fwd=mean(avg_ga_90_10k_fwd),
    n_teams = n()
  ) 



# Linear regression predicting def stats from def spending
library(broom)
def_spend_ga_lm<-lm(mls_team_analysis$ga_per_10k~
                      mls_team_analysis$avg_salary_def)
tidy(def_spend_ga_lm, conf.int=TRUE, conf.level = .95)
glance(def_spend_ga_lm)
  
fwd_spend_ga_lm<-lm(mls_team_analysis$ga_per_10k~
                      mls_team_analysis$avg_salary_fwd)
tidy(fwd_spend_ga_lm, conf.int=TRUE, conf.level = .95)
glance(fwd_spend_ga_lm)

mls_team_analysis<-mls_team_analysis|>
  mutate(fwd_def_spend_ratio=avg_salary_fwd/avg_salary_def)

fwd_def_spend_ratio_lm<-lm(mls_team_analysis$ga_per_10k~
                             mls_team_analysis$fwd_def_spend_ratio)
tidy(fwd_def_spend_ratio_lm, conf.int=TRUE, conf.level = .95)
glance(fwd_def_spend_ratio_lm)

mls_team_analysis|>
  ggplot(aes(fwd_def_spend_ratio, ga_per_10k))+
  geom_point(size=4, color="red")+
  geom_smooth(method="lm", se= FALSE, color="blue")+
  theme_minimal()+
  labs(title="There is a significant relationship for the more that teams spend on their \n offense proportionally to their defense, the less efficient they are",
      x="Forward to Defense Spend Ratio",
       y="Goals Added per $10k")+
  theme(plot.title=element_text(hjust=.5, face="bold", size=18))

fwd_def_spend_ratio_ga_lm<-lm(mls_team_analysis$total_goals_added_for~
                             mls_team_analysis$fwd_def_spend_ratio)
tidy(fwd_def_spend_ratio_ga_lm, conf.int=TRUE, conf.level = .95)
glance(fwd_def_spend_ratio_ga_lm)

mls_team_analysis<-mls_team_analysis|>
  mutate(total_fwd_def_spend_ratio=total_salary_fwd/total_salary_def)

total_fwd_def_spend_ratio_lm<-lm(mls_team_analysis$ga_per_10k~
                             mls_team_analysis$total_fwd_def_spend_ratio)
tidy(total_fwd_def_spend_ratio_lm, conf.int=TRUE, conf.level = .95)
glance(total_fwd_def_spend_ratio_lm)

mls_team_analysis|>
  summarise(mean(total_guaranteed_compensation))

mls_team_analysis|>
  summarise(mean(fwd_def_spend_ratio))

# k-means clustering teams
library(cluster)
library(factoextra)

mls_cluster_data <- mls_team_analysis |>
  select(team, year, total_goals_added_for, total_goals_added_against,
         avg_guaranteed_compensation, xgoal_difference) |>  # domain-relevant
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

# Add cluster assignments to original (unscaled) data
mls_cluster_data$cluster <- factor(kmeans_result$cluster)

# Calculate mean value of each variable per cluster
cluster_profiles <- mls_cluster_data |>
  group_by(cluster) |>
  summarise(across(everything(), mean))

# Reshape to long format for plotting
library(tidyr)
library(ggplot2)

cluster_profiles_long <- pivot_longer(cluster_profiles, 
                                      cols = -cluster, 
                                      names_to = "variable", 
                                      values_to = "mean_value")

# Plot
ggplot(cluster_profiles_long, aes(x = variable, y = mean_value, color = cluster, group = cluster)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_minimal(base_size = 14) +
  labs(title = "Cluster Profiles by Variable",
       y = "Mean Value (Original Scale)",
       x = "Variable") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Perform PCA on scaled data
pca_res <- prcomp(scaled_data)

# Visualize PCA with clusters
fviz_pca_biplot(pca_res,
                label = "var",      # shows variable vectors
                habillage = kmeans_result$cluster, 
                addEllipses = TRUE, 
                repel = TRUE) +
  ggtitle("PCA Biplot with K-Means Clusters")

library(broom)

mls_cluster_data_long <- pivot_longer(mls_cluster_data, 
                                      cols = -cluster,
                                      names_to = "variable", 
                                      values_to = "value")

anova_results <- mls_cluster_data_long |>
  group_by(variable) |>
  summarise(p_value = summary(aov(value ~ cluster))[[1]][["Pr(>F)"]][1])

# Order by significance
anova_results |> arrange(p_value)

analyze_cluster_drivers <- function(original_data, scaled_data, kmeans_result) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(factoextra)
  library(broom)
  
  # 1. Add cluster labels to original (unscaled) data
  data_with_clusters <- original_data %>%
    mutate(cluster = factor(kmeans_result$cluster))
  
  # 2. Cluster Profile Plot
  cluster_profiles <- data_with_clusters %>%
    group_by(cluster) %>%
    summarise(across(where(is.numeric), mean), .groups = "drop")
  
  cluster_profiles_long <- pivot_longer(cluster_profiles, 
                                        cols = -cluster, 
                                        names_to = "variable", 
                                        values_to = "mean_value")
  
  profile_plot <- ggplot(cluster_profiles_long, aes(x = variable, y = mean_value, color = cluster, group = cluster)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    theme_minimal(base_size = 14) +
    labs(title = "Cluster Profiles by Variable",
         y = "Mean Value",
         x = "Variable") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(profile_plot)
  
  # 3. PCA Biplot with clusters
  pca_res <- prcomp(scaled_data)
  pca_plot <- fviz_pca_biplot(pca_res,
                              label = "var",
                              habillage = kmeans_result$cluster,
                              addEllipses = TRUE,
                              repel = TRUE) +
    ggtitle("PCA Biplot with K-Means Clusters")
  print(pca_plot)
  
  # 4. ANOVA per variable across clusters
  long_data <- pivot_longer(data_with_clusters, 
                            cols = -cluster,
                            names_to = "variable", 
                            values_to = "value")
  
  anova_results <- long_data %>%
    group_by(variable) %>%
    summarise(p_value = summary(aov(value ~ cluster))[[1]][["Pr(>F)"]][1]) %>%
    arrange(p_value)
  
  cat("\nANOVA variable importance (sorted by p-value):\n")
  print(anova_results)
}


mls_cluster_data_2<-mls_team_analysis|>
  select(team_year, avg_guaranteed_compensation,
         std_dev_guaranteed_compensation, xgoal_difference)|>
  column_to_rownames("team_year")

scaled_data_2<-scale(mls_cluster_data_2)

fviz_nbclust(scaled_data_2, kmeans, method = "wss")

set.seed(123)
kmeans_result_2 <- kmeans(scaled_data_2, centers = 4, nstart = 25)

fviz_cluster(kmeans_result_2, data = scaled_data_2, 
             repel = TRUE, 
             labelsize = 8,
             main = "K-Means Clustering of Team-Year Observations")

analyze_cluster_drivers(mls_cluster_data_2, scaled_data_2, kmeans_result_2)

# Percentile Analysis

# Getting salary break downs positionally for 24
mls_players<-asa$get_players(leagues="mls")
all_salaries_2024<-asa$get_player_salaries(leagues = "mls", 
                                       start_date="2024-09-13", 
                                       end_date="2024-09-14")
all_salaries_24<-all_salaries_2024|>
  left_join(mls_players|>
              select(player_id, player_name), by="player_id")|>
  left_join(mls_teams|>
              select(team_id, team=team_abbreviation), by="team_id")

all_player_goals_added_24<-asa$get_player_goals_added(leagues="mls",
                                                  season_name=2024)
all_salaries_24<-all_salaries_24|>
  left_join(all_player_goals_added_24|>
              select(player_id, general_position, minutes_played, data), 
            by=c("player_id"))|>
  select(-player_id, -team_id, -season_name, -mlspa_release, -competition)

all_salaries_24<-all_salaries_24|>
  drop_na()|>
  mutate(goals_added=data)|>
  select(-data)

all_salaries_24<-all_salaries_24|>
  mutate(
    total_goals_added_raw = map_dbl(goals_added, ~sum(.x$goals_added_raw, na.rm = TRUE)),
    total_goals_added_above_avg = map_dbl(goals_added, ~sum(.x$goals_added_above_avg, na.rm = TRUE)),
    total_count_actions = map_dbl(goals_added, ~sum(.x$count_actions, na.rm = TRUE))
  )

# adding the goals_added details to the data set
expanded<-all_salaries_24|>
  select(player_name, goals_added)|>
  unnest(goals_added)
wide_goals_added<-expanded|>
  pivot_wider(names_from=action_type,
              values_from=c(goals_added_raw, goals_added_above_avg, count_actions),
              names_sep="_")

all_salaries_24<-all_salaries_24|>
  select(-goals_added)|>
  left_join(wide_goals_added, by="player_name")

all_salaries_24<-all_salaries_24|>
  mutate(ga_90=(total_goals_added_raw/minutes_played)*90,
         ga_avg_90=(total_goals_added_above_avg/minutes_played)*90,
         ga_90_10k=(ga_90/guaranteed_compensation)*10000,
         ga_avg_90_10k=(ga_avg_90/guaranteed_compensation)*10000)

all_salaries_24<-all_salaries_24|>
  mutate(year=2024,
         team_year = paste(team, year, sep = "_"))

all_salaries_24<-all_salaries_24|>
  relocate(team_year)

top15_24<-all_salaries_24|>
  group_by(team_year)|>
  arrange(desc(guaranteed_compensation), .bygroup=TRUE)|>
  slice_head(n=15)|>
  mutate(rank_15=row_number())|>
  ungroup()

percentiles_24<-top15_24|>
  mutate(tier=case_when(
    rank_15<=5 ~ "Top 5",
    rank_15<=10 ~ "Middle 5",
    TRUE ~ "Bottom 5"))|>
  group_by(team_year, tier)|>
  summarise(tier_comp = sum(guaranteed_compensation, na.rm = TRUE), .groups = "drop") |>
  left_join(
    top15_24 |>
      group_by(team_year) |>
      summarise(team_total_comp = sum(guaranteed_compensation, na.rm = TRUE), .groups = "drop"),
    by = "team_year"
  ) |>
  mutate(percent_of_team_total = (tier_comp / team_total_comp) * 100)|>
  select(team_year, tier, percent_of_team_total) |>
  pivot_wider(names_from = tier, values_from = percent_of_team_total)

# Getting salary break downs positionally for 23

all_salaries_2023<-asa$get_player_salaries(leagues = "mls", 
                                       start_date="2023-09-15", 
                                       end_date="2023-09-16")
all_salaries_23<-all_salaries_2023|>
  left_join(mls_players|>
              select(player_id, player_name), by="player_id")|>
  left_join(mls_teams|>
              select(team_id, team=team_abbreviation), by="team_id")

all_player_goals_added_23<-asa$get_player_goals_added(leagues="mls",
                                                  season_name=2023)

all_salaries_23<-all_salaries_23|>
  left_join(all_player_goals_added_23|>
              select(player_id, general_position, minutes_played, data), 
            by=c("player_id"))|>
  select(-player_id, -team_id, -mlspa_release, -competition)

all_salaries_23<-all_salaries_23|>
  drop_na()|>
  mutate(goals_added=data)|>
  select(-data)

all_salaries_23<-all_salaries_23|>
  mutate(
    total_goals_added_raw = map_dbl(goals_added, ~sum(.x$goals_added_raw, na.rm = TRUE)),
    total_goals_added_above_avg = map_dbl(goals_added, ~sum(.x$goals_added_above_avg, na.rm = TRUE)),
    total_count_actions = map_dbl(goals_added, ~sum(.x$count_actions, na.rm = TRUE))
  )

# adding the goals_added details to the data set
expanded<-all_salaries_23|>
  select(player_name, goals_added)|>
  unnest(goals_added)
wide_goals_added<-expanded|>
  pivot_wider(names_from=action_type,
              values_from=c(goals_added_raw, goals_added_above_avg, count_actions),
              names_sep="_")

all_salaries_23<-all_salaries_23|>
  select(-goals_added)|>
  left_join(wide_goals_added, by="player_name")

all_salaries_23<-all_salaries_23|>
  mutate(ga_90=(total_goals_added_raw/minutes_played)*90,
         ga_avg_90=(total_goals_added_above_avg/minutes_played)*90,
         ga_90_10k=(ga_90/guaranteed_compensation)*10000,
         ga_avg_90_10k=(ga_avg_90/guaranteed_compensation)*10000)

all_salaries_23<-all_salaries_23|>
  mutate(team_year = paste(team, season_name, sep = "_"))

all_salaries_23<-all_salaries_23|>
  relocate(team_year)

top15_23<-all_salaries_23|>
  group_by(team_year)|>
  arrange(desc(guaranteed_compensation), .bygroup=TRUE)|>
  slice_head(n=15)|>
  mutate(rank_15=row_number())|>
  ungroup()

percentiles_23<-top15_23|>
  mutate(tier=case_when(
    rank_15<=5 ~ "Top 5",
    rank_15<=10 ~ "Middle 5",
    TRUE ~ "Bottom 5"))|>
  group_by(team_year, tier)|>
  summarise(tier_comp = sum(guaranteed_compensation, na.rm = TRUE), .groups = "drop") |>
  left_join(
    top15_23 |>
      group_by(team_year) |>
      summarise(team_total_comp = sum(guaranteed_compensation, na.rm = TRUE), .groups = "drop"),
    by = "team_year"
  ) |>
  mutate(percent_of_team_total = (tier_comp / team_total_comp) * 100)|>
  select(team_year, tier, percent_of_team_total) |>
  pivot_wider(names_from = tier, values_from = percent_of_team_total)

# Getting salary break downs positionally for 22

all_salaries_2022<-asa$get_player_salaries(leagues = "mls", 
                                       season_name=2022)
all_salaries_22<-all_salaries_2022|>
  left_join(mls_players|>
              select(player_id, player_name), by="player_id")|>
  left_join(mls_teams|>
              select(team_id, team=team_abbreviation), by="team_id")

all_salaries_22<-all_salaries_22|>
  filter((team == "MTL" & mlspa_release == "2022-04-15") |
           (team != "MTL" & mlspa_release == "2022-09-02"))

all_player_goals_added_22<-asa$get_player_goals_added(leagues="mls",
                                                  season_name=2022)

all_salaries_22<-all_salaries_22|>
  left_join(all_player_goals_added_22|>
              select(player_id, general_position, minutes_played, data), 
            by=c("player_id"))|>
  select(-player_id, -team_id, -mlspa_release, -competition)

all_salaries_22<-all_salaries_22|>
  drop_na()|>
  mutate(goals_added=data)|>
  select(-data)

all_salaries_22<-all_salaries_22|>
  mutate(
    total_goals_added_raw = map_dbl(goals_added, ~sum(.x$goals_added_raw, na.rm = TRUE)),
    total_goals_added_above_avg = map_dbl(goals_added, ~sum(.x$goals_added_above_avg, na.rm = TRUE)),
    total_count_actions = map_dbl(goals_added, ~sum(.x$count_actions, na.rm = TRUE))
  )

# adding the goals_added details to the data set
expanded<-all_salaries_22|>
  select(player_name, goals_added)|>
  unnest(goals_added)
wide_goals_added<-expanded|>
  pivot_wider(names_from=action_type,
              values_from=c(goals_added_raw, goals_added_above_avg, count_actions),
              names_sep="_")

all_salaries_22<-all_salaries_22|>
  select(-goals_added)|>
  left_join(wide_goals_added, by="player_name")

all_salaries_22<-all_salaries_22|>
  mutate(ga_90=(total_goals_added_raw/minutes_played)*90,
         ga_avg_90=(total_goals_added_above_avg/minutes_played)*90,
         ga_90_10k=(ga_90/guaranteed_compensation)*10000,
         ga_avg_90_10k=(ga_avg_90/guaranteed_compensation)*10000)

all_salaries_22<-all_salaries_22|>
  mutate(team_year = paste(team, season_name, sep = "_"))

all_salaries_22<-all_salaries_22|>
  relocate(team_year)

top15_22<-all_salaries_22|>
  group_by(team_year)|>
  arrange(desc(guaranteed_compensation), .bygroup=TRUE)|>
  slice_head(n=15)|>
  mutate(rank_15=row_number())|>
  ungroup()

percentiles_22<-top15_22|>
  mutate(tier=case_when(
    rank_15<=5 ~ "Top 5",
    rank_15<=10 ~ "Middle 5",
    TRUE ~ "Bottom 5"))|>
  group_by(team_year, tier)|>
  summarise(tier_comp = sum(guaranteed_compensation, na.rm = TRUE), .groups = "drop") |>
  left_join(
    top15_22 |>
      group_by(team_year) |>
      summarise(team_total_comp = sum(guaranteed_compensation, na.rm = TRUE), .groups = "drop"),
    by = "team_year"
  ) |>
  mutate(percent_of_team_total = (tier_comp / team_total_comp) * 100)|>
  select(team_year, tier, percent_of_team_total) |>
  pivot_wider(names_from = tier, values_from = percent_of_team_total)


# Getting salary breaks downs positionally for 21

all_salaries_2021<-asa$get_player_salaries(leagues = "mls", 
                                       start_date="2021-09-30", 
                                       end_date="2021-10-01")
all_salaries_21<-all_salaries_2021|>
  left_join(mls_players|>
              select(player_id, player_name), by="player_id")|>
  left_join(mls_teams|>
              select(team_id, team=team_abbreviation), by="team_id")

all_player_goals_added_21<-asa$get_player_goals_added(leagues="mls",
                                                  season_name=2021)

all_salaries_21<-all_salaries_21|>
  left_join(all_player_goals_added_21|>
              select(player_id, general_position, minutes_played, data), 
            by=c("player_id"))|>
  select(-player_id, -team_id, -mlspa_release, -competition)

all_salaries_21<-all_salaries_21|>
  drop_na()|>
  mutate(goals_added=data)|>
  select(-data)

all_salaries_21<-all_salaries_21|>
  mutate(
    total_goals_added_raw = map_dbl(goals_added, ~sum(.x$goals_added_raw, na.rm = TRUE)),
    total_goals_added_above_avg = map_dbl(goals_added, ~sum(.x$goals_added_above_avg, na.rm = TRUE)),
    total_count_actions = map_dbl(goals_added, ~sum(.x$count_actions, na.rm = TRUE))
  )

# adding the goals_added details to the data set
expanded<-all_salaries_21|>
  select(player_name, goals_added)|>
  unnest(goals_added)
wide_goals_added<-expanded|>
  pivot_wider(names_from=action_type,
              values_from=c(goals_added_raw, goals_added_above_avg, count_actions),
              names_sep="_")

all_salaries_21<-all_salaries_21|>
  select(-goals_added)|>
  left_join(wide_goals_added, by="player_name")

all_salaries_21<-all_salaries_21|>
  mutate(ga_90=(total_goals_added_raw/minutes_played)*90,
         ga_avg_90=(total_goals_added_above_avg/minutes_played)*90,
         ga_90_10k=(ga_90/guaranteed_compensation)*10000,
         ga_avg_90_10k=(ga_avg_90/guaranteed_compensation)*10000)

all_salaries_21<-all_salaries_21|>
  mutate(team_year = paste(team, season_name, sep = "_"))

all_salaries_21<-all_salaries_21|>
  relocate(team_year)

top15_21<-all_salaries_21|>
  group_by(team_year)|>
  arrange(desc(guaranteed_compensation), .bygroup=TRUE)|>
  slice_head(n=15)|>
  mutate(rank_15=row_number())|>
  ungroup()

percentiles_21<-top15_21|>
  mutate(tier=case_when(
    rank_15<=5 ~ "Top 5",
    rank_15<=10 ~ "Middle 5",
    TRUE ~ "Bottom 5"))|>
  group_by(team_year, tier)|>
  summarise(tier_comp = sum(guaranteed_compensation, na.rm = TRUE), .groups = "drop") |>
  left_join(
    top15_21 |>
      group_by(team_year) |>
      summarise(team_total_comp = sum(guaranteed_compensation, na.rm = TRUE), .groups = "drop"),
    by = "team_year"
  ) |>
  mutate(percent_of_team_total = (tier_comp / team_total_comp) * 100)|>
  select(team_year, tier, percent_of_team_total) |>
  pivot_wider(names_from = tier, values_from = percent_of_team_total)

mls_team_analysis<-mls_team_analysis|>
  select(-`Bottom 5.x`, -`Bottom 5.y`, -`Middle 5.x`, -`Middle 5.y`,
         -`Top 5.x`, -`Top 5.y`)

salary_percentiles<-bind_rows(percentiles_21, percentiles_22,
                              percentiles_23, percentiles_24)
mls_team_analysis<-mls_team_analysis|>
  left_join(salary_percentiles, by="team_year")

mls_cluster_analysis<-mls_team_analysis |>
  group_by(salary_xG_clusters_log) |>
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
    mean_goals_added_for_claiming=mean(goals_added_for_Claiming, na.rm=TRUE),
    mean_goals_added_for_interrupting=mean(goals_added_for_Interrupting, na.rm=TRUE),
    mean_goals_added_for_fouling=mean(goals_added_for_Fouling, na.rm=TRUE),
    mean_goals_added_against_shooting=mean(goals_added_against_Shooting, na.rm=TRUE),
    mean_avg_salary_def=mean(avg_salary_def),
    mean_avg_ga_90_def=mean(avg_ga_90_def),
    mean_avg_ga_90_10k_def=mean(avg_ga_90_10k_def),
    mean_avg_salary_mid=mean(avg_salary_mid),
    mean_avg_ga_90_mid=mean(avg_ga_90_mid),
    mean_avg_ga_90_10k_mid=mean(avg_ga_90_10k_mid),
    mean_avg_salary_fwd=mean(avg_salary_fwd),
    mean_avg_ga_90_fwd=mean(avg_ga_90_fwd),
    mean_avg_ga_90_10k_fwd=mean(avg_ga_90_10k_fwd),
    mean_bottom_5=mean(`Bottom 5`),
    mean_middle_5=mean(`Middle 5`),
    mean_top_5=mean(`Top 5`),
    n_teams = n()
  ) 

# Regression between salary percentiles and performance

bottom_5_lm<-lm(mls_team_analysis$xgoal_difference~mls_team_analysis$bottom5_pct)
tidy(bottom_5_lm)
glance(bottom_5_lm)

middle_5_lm<-lm(mls_team_analysis$xgoal_difference~mls_team_analysis$middle5_pct)
tidy(middle_5_lm, conf.int = TRUE, conf.level = .95)
glance(middle_5_lm)

top_5_lm<-lm(mls_team_analysis$xgoal_difference~mls_team_analysis$top5_pct)
tidy(top_5_lm, conf.int = TRUE, conf.level = .95)
glance(top_5_lm)

salary_percent_mlr<-lm(mls_team_analysis$xgoal_difference~
                         mls_team_analysis$middle5_pct + 
                         mls_team_analysis$top5_pct)
tidy(salary_percent_mlr, conf.int = TRUE, conf.level = .95)
glance(salary_percent_mlr)

# Creating a model based on salary percentiles
mls_team_analysis<-mls_team_analysis|>
  mutate(top5_pct=`Top 5`, middle5_pct=`Middle 5`, bottom5_pct=`Bottom 5`)|>
  select(-`Top 5`, -`Middle 5`, -`Bottom 5`)
library(caret)
train_data<-mls_team_analysis|>
  filter(year>=2021 & year<=2023)
test_data<-mls_team_analysis|>
  filter(year==2024)

salary_percent_formula<-xgoal_difference ~ top5_pct + middle5_pct

salary_percent_model<-train(salary_percent_formula,
                            data=train_data,
                            method="lm")

summary(salary_percent_model$finalModel)

model_pred<-predict(salary_percent_model, newdata=test_data)

model_results<-test_data|>
  mutate(pred_xG_difference=model_pred,
         residuals=xgoal_difference-pred_xG_difference)

model_rmse <- sqrt(mean(model_results$residuals^2))
model_mae <- mean(abs(model_results$residuals))
model_rmse
model_mae

summary(mls_team_analysis$xgoal_difference)
sd(mls_team_analysis$xgoal_difference)
range(mls_team_analysis$xgoal_difference)

# Finding the best break down of salary

# Creating grid of possible salary splits
grid <- expand.grid(
  top5_pct = seq(50, 87, by = .5), 
  middle5_pct= seq(9, 34, by = .5))|>
  mutate(bottom5_pct = 100 - top5_pct - middle5_pct)|>
  filter(bottom5_pct>=3 & bottom5_pct<=19)

# Predict using your trained model (e.g., linear model)
grid$predicted_xG_diff <- predict(salary_percent_model, newdata = grid)

# Find the best combination
best_combo <- grid|>
  arrange(desc(predicted_xG_diff)) |>
  slice(1)

print(best_combo)

# visualizing
ggplot(grid, aes(x = top5_pct, y = middle5_pct, fill = predicted_xG_diff)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "Predicted xG Difference by Salary Distribution",
       x = "Top 5 Salary %", y = "Middle 5 Salary %", 
       fill = "Predicted xG \n Difference") +
  theme_minimal()+
  theme(plot.title = element_text(hjust=.5))

mean(mls_team_analysis$bottom5_pct)

cor(mls_team_analysis$xgoal_difference, mls_team_analysis$avg_salary_fwd)

cor(mls_team_analysis$total_goals_added_for, mls_team_analysis$xgoal_difference)

cor(mls_team_analysis$total_goals_added_for, mls_team_analysis$goal_difference)

# EDA Plots
mls_team_analysis|>
  ggplot(aes(total_goals_added_for, xgoal_difference))+
  geom_point(size=4, color="red")+
  geom_smooth(method="lm", se= FALSE, color="blue")+
  theme_minimal()+
  labs(title="G+ has a Strong Linear Relationship with xG Difference", 
       x="Total G+ For", y="xG Difference")

mls_team_analysis|>
  ggplot(aes(total_goals_added_for, goal_difference))+
  geom_point(size=4, color="red")+
  geom_smooth(method="lm", se= FALSE, color="blue")+
  theme_minimal()+
  labs(title="G+ has a Strong Linear Relationship with Goal Difference", 
       x="Total G+ For", y="Goal Difference")

library(scales)
library(ggrepel)

mls_team_analysis|>
  ggplot(aes(avg_guaranteed_compensation, xgoal_difference, color=salary_xG_clusters_log))+
  geom_point(size=3)+
  labs(title="Average Team Spending vs. xG Difference", 
       subtitle="MLS 2021-2024 Seasons",
       x="Average Guaranteed Compensation",
       y="xG Difference",
       color="Cluster")+
  theme_light()+
  theme(plot.title=element_text(hjust=.5, face="bold", size=20),
        plot.subtitle = element_text(hjust=.5, face="bold", size=15))
