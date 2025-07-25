# installing ASA
install.packages("itscalledsoccer")
library(itscalledsoccer)
library(tidyverse)
library(dplyr)

itscalledsoccer::AmericanSoccerAnalysis

# joining salaries with player and team
asa<-AmericanSoccerAnalysis$new()
mls_teams<-asa$get_teams(leagues = "mls")
mls_players<-asa$get_players(leagues="mls")
salaries_2024<-asa$get_player_salaries(leagues = "mls", 
                                       start_date="2024-09-13", 
                                       end_date="2024-09-14")
salaries_24<-salaries_2024|>
  left_join(mls_players|>
              select(player_id, player_name), by="player_id")|>
  left_join(mls_teams|>
              select(team_id, team=team_abbreviation), by="team_id")

# joining salaries and G+
mls_goalsadded_2024_reg<-asa$get_player_goals_added(leagues = "mls",
                                                    minimum_minutes=1000,
                                                    season_name=2024,
                                                    stage_name="Regular Season")

mls_goalsadded_24_reg<-salaries_24|>
  left_join(mls_goalsadded_2024_reg|>
              select(player_id, team_id, minutes_played, goals_added=data, general_position), 
            by="player_id")
mls_goals_added_24_reg<-mls_goalsadded_24_reg|>
  select(-player_id, -team_id.x, -season_name, -mlspa_release, -competition, -team_id.y)

# adding the sum of the goals_added details to the data set
mls_goals_added_24_reg<-mls_goals_added_24_reg|>
  mutate(
    total_goals_added_raw = map_dbl(goals_added, ~sum(.x$goals_added_raw, na.rm = TRUE)),
    total_goals_added_above_avg = map_dbl(goals_added, ~sum(.x$goals_added_above_avg, na.rm = TRUE)),
    total_count_actions = map_dbl(goals_added, ~sum(.x$count_actions, na.rm = TRUE))
  )

# adding the goals_added details to the data set
expanded<-mls_goals_added_24_reg|>
  select(player_name, goals_added)|>
  unnest(goals_added)
wide_goals_added<-expanded|>
  pivot_wider(names_from=action_type,
              values_from=c(goals_added_raw, goals_added_above_avg, count_actions),
              names_sep="_")
mls_goals_added_24_reg<-mls_goals_added_24_reg|>
  select(-goals_added)|>
  left_join(wide_goals_added, by="player_name")

mls_goals_added_24_reg<-mls_goals_added_24_reg|>
  arrange(team)

# linear regression of goals added and salary
salary_goalsadded_lm<-lm(guaranteed_compensation~total_goals_added_raw, 
                         data=mls_goals_added_24_reg)
summary(salary_goalsadded_lm)

mls_players<-asa$get_players()

mls_goals_added_24_reg<-mls_goals_added_24_reg|>
  left_join(mls_players|>
              select(player_name, birth_date, nationality), by="player_name")

sum(is.na(mls_goals_added_24_reg$nationality))

# adding the nationality for Noah Cobb
mls_goals_added_24_reg<-mls_goals_added_24_reg|>
  mutate(nationality=case_when(
    player_name=="Noah Cobb"~"USA",
    TRUE~nationality))

## Creating a data set with all the team stats

mls_team_salaries_2024<-asa$get_team_salaries(leagues="mls",
                                              season_name=2024)
mls_team_goals_added_2024<-asa$get_team_goals_added(league="mls",
                                                    season_name=2024,
                                                    stage_name="Regular Season")
mls_team_xG<-asa$get_team_xgoals(leagues="mls",
                                 season_name=2024,
                                 stage_name="Regular Season")
mls_teams_24<-mls_teams|>
  dplyr::slice(-6, -25)

mls_team_stats_24<-mls_team_salaries_2024|>
  left_join(mls_teams_24|>
              select(team_id, team=team_abbreviation), by="team_id")|>
  arrange(team)|>
  dplyr::slice(-30)

mls_team_stats_24<-mls_team_stats_24|>
  left_join(mls_team_xG,
            by="team_id")
mls_team_stats_24<-mls_team_stats_24|>
  left_join(mls_team_goals_added_2024|>
              select(team_id, minutes, data), by="team_id")

mls_team_stats_24<-mls_team_stats_24|>
  select(-team_id, -competition.x, -competition.y)
mls_team_stats_24<-mls_team_stats_24|>
  relocate(team, .before=1)
mls_team_stats_24<-mls_team_stats_24|>
  relocate(count_games, .before=2)

# adding total goals_added stats
mls_team_stats_24<-mls_team_stats_24|>
  mutate(
    total_goals_added_for = map_dbl(data, ~sum(.x$goals_added_for, na.rm = TRUE)),
    total_goals_added_against = map_dbl(data, ~sum(.x$goals_added_against, na.rm = TRUE)),
    total_num_actions_for = map_dbl(data, ~sum(.x$num_actions_for, na.rm = TRUE)),
    total_num_actions_against=map_dbl(data, ~sum(.x$num_actions_against, na.rm=TRUE))
  )

expanded<-mls_team_stats_24|>
  select(team, data)|>
  unnest(data)

wide_goals_added<-expanded|>
  pivot_wider(names_from=action_type,
              values_from=c(goals_added_for, goals_added_against, num_actions_for, num_actions_against),
              names_sep="_")
mls_team_stats_24<-mls_team_stats_24|>
  select(-data)|>
  left_join(wide_goals_added, by="team")

library(ggplot2)
library(scales)
library(ggrepel)

mls_team_stats_24<-mls_team_stats_24|>
  mutate(quadrant=case_when(
    xgoal_difference>=mean(xgoal_difference) & 
      avg_guaranteed_compensation>=mean(avg_guaranteed_compensation)~"High Spend/High xG",
    xgoal_difference<mean(xgoal_difference) & 
      avg_guaranteed_compensation >=mean(avg_guaranteed_compensation)~"High Spend/Low xG",
    xgoal_difference>=mean(xgoal_difference) & 
      avg_guaranteed_compensation<mean(avg_guaranteed_compensation)~"Low Spend/High xG",
    xgoal_difference<mean(xgoal_difference) & 
      avg_guaranteed_compensation<mean(avg_guaranteed_compensation)~"Low Spend/Low xG",
  ))

x_mid <- mean(mls_team_stats_24$avg_guaranteed_compensation)
y_mid <- mean(mls_team_stats_24$xgoal_difference)

mls_team_stats_24|>
  ggplot(aes(avg_guaranteed_compensation, xgoal_difference, color=quadrant))+
  geom_point(size=3)+
  geom_smooth(method="lm", se= FALSE, color="blue")+
  geom_text_repel(aes(label=team), size=3, max.overlaps=Inf,
                  color="black", bg.color="white", bg.r=.15)+
  geom_hline(yintercept = mean(mls_team_stats_24$xgoal_difference),
             linetype="dashed", alpha=.5)+
  geom_vline(xintercept = mean(mls_team_stats_24$avg_guaranteed_compensation),
             linetype="dashed", alpha=.5)+
  scale_x_continuous(labels= comma)+
  labs(title="Average Team Spending vs. xG Difference", 
       subtitle="2024 MLS Regular Season",
       x="Average Guaranteed Compensation",
       y="xG Difference",
       color="Quadrant")+
  annotate("text", x = x_mid - 150000, y = y_mid + 15, label = "Efficient \n Spending", fontface="italic", size=6) +
  annotate("text", x = x_mid + 300000, y = y_mid + 15, label = "Expected Strong \n Performance", fontface="italic", size=6) +
  annotate("text", x = x_mid - 150000, y = y_mid - 20, label = "Expected Poor \n Performance", fontface="italic", size=6) +
  annotate("text", x = x_mid + 300000, y = y_mid - 20, label = "Inefficient \n Spending", fontface="italic", size=6) +
  theme_light()+
  theme(plot.title=element_text(hjust=.5, face="bold", size=20), 
        plot.subtitle = element_text(hjust=.5, face="bold", size=15))

mls_goalsadded_2024_reg<-asa$get_player_goals_added(leagues = "mls",
                                                    season_name=2024,
                                                    stage_name="Regular Season")
salaries_position_24<-salaries_24|>
  left_join(mls_goalsadded_2024_reg|>
              select(player_id, team_id, general_position), by="player_id")
salaries_position_24<-salaries_position_24|>
  filter(!is.na(general_position))|>
  select(-player_id, -team_id.x, -season_name, -mlspa_release, competition, -team_id.y)
salaries_position_24<-salaries_position_24|>
  select(-competition)

salaries_position_24|>
  ggplot(aes(guaranteed_compensation))+
  geom_histogram()+
  facet_wrap(~general_position)

salaries_position_24|>
  group_by(general_position)|>
  summarise(mean_salary=mean(guaranteed_compensation))|>
  arrange(desc(mean_salary))

# Looking at rosters for teams in cluster 2

salaries_position_24|>
  filter(general_position==c("DM", "CB", "FB"), team=="PHI")

salaries_position_24|>
  filter(position=="D", team=="PHI")

salaries_position_24|>
  filter(general_position==c("DM", "CB", "FB"), team=="RSL")

salaries_position_24|>
  filter(position=="D", team=="RSL")
