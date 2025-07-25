## Figure 1 Code
library(itscalledsoccer)
library(tidyverse)
library(dplyr)

# joining salaries with player and team


# asa<-AmericanSoccerAnalysis$new()
# mls_teams<-asa$get_teams(leagues = "mls")
# mls_players<-asa$get_players(leagues="mls")
# salaries_2024<-asa$get_player_salaries(leagues = "mls",                                 start_date="2024-09-13",                                        end_date="2024-09-14")
# salaries_24<-salaries_2024|> left_join(mls_players|> select(player_id, player_name), by="player_id")|> left_join(mls_teams|> select(team_id, team=team_abbreviation), by="team_id")

## Creating a data set with all the team stats

# mls_team_salaries_2024<-asa$get_team_salaries(leagues="mls",                                season_name=2024) mls_team_goals_added_2024<-asa$get_team_goals_added(league="mls",                                      season_name=2024,                                           stage_name="Regular Season")
# mls_team_xG<-asa$get_team_xgoals(leagues="mls", season_name=2024, stage_name="Regular Season")

# mls_teams_24<-mls_teams|> dplyr::slice(-6, -25)

# mls_team_stats_24<-mls_team_salaries_2024|> left_join(mls_teams_24|> select(team_id, team=team_abbreviation), by="team_id")|> arrange(team)|> dplyr::slice(-30)

# mls_team_stats_24<-mls_team_stats_24|> left_join(mls_team_xG, by="team_id")

#mls_team_stats_24<-mls_team_stats_24|> left_join(mls_team_goals_added_2024|> select(team_id, minutes, data), by="team_id")

# mls_team_stats_24<-mls_team_stats_24|> select(-team_id, -competition.x, -competition.y)

# mls_team_stats_24<-mls_team_stats_24|> relocate(team, .before=1)
# mls_team_stats_24<-mls_team_stats_24|> relocate(count_games, .before=2)
# write_rds(mls_team_stats_24, "mls_team_stats_24.rds")

mls_team_stats_24 <- read_rds("mls_team_stats_24.rds")

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



# Density RMSE Plot
library(tidyverse)
library(dplyr)
library(ggplot2)
library(glmnet)
library(xgboost)
library(itscalledsoccer)
asa<-AmericanSoccerAnalysis$new()

## constructing the main data set
mls_teams<-asa$get_teams(leagues="mls")
mls_teams<-mls_teams|>
  dplyr::slice(-6, -25)

mls_team_salaries_24<-asa$get_team_salaries(leagues = "mls",
                                            season_name=2024)
mls_team_xG_24<-asa$get_team_xgoals(leagues="mls",
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
  select(-team_id, -team_name, -team_short_name)

mls_teams_24<-mls_teams_24|>
  mutate(year=2024)

mls_team_salaries_23<-asa$get_team_salaries(leagues = "mls",
                                            season_name=2023)
mls_team_xG_23<-asa$get_team_xgoals(leagues="mls",
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
  select(-team_id, -team_name, -team_short_name)

mls_teams_23<-mls_teams_23|>
  mutate(year=2023)

mls_team_salaries_22<-asa$get_team_salaries(leagues = "mls",
                                            season_name=2022)
mls_team_xG_22<-asa$get_team_xgoals(leagues="mls",
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
  select(-team_id, -team_name, -team_short_name)

mls_teams_22<-mls_teams_22|>
  mutate(year=2022)

mls_teams_22<-mls_teams_22|>
  dplyr::slice(-27)

mls_team_salaries_21<-asa$get_team_salaries(leagues = "mls",
                                            season_name=2021)
mls_team_xG_21<-asa$get_team_xgoals(leagues="mls",
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
  select(-team_id, -team_name, -team_short_name)

mls_teams_21<-mls_teams_21|>
  mutate(year=2021)

mls_teams_21<-mls_teams_21|>
  dplyr::slice(-4, -27)

mls_team_analysis<-bind_rows(mls_teams_21, mls_teams_22, 
                             mls_teams_23, mls_teams_24)

mls_team_analysis<-mls_team_analysis|>
  rename(team=team_abbreviation)

mls_team_analysis<-mls_team_analysis|>
  relocate(year, .before=2)

mls_team_analysis<-mls_team_analysis|>
  mutate(team_year = paste(team, year, sep = "_")) |>
  relocate(team_year)

mls_players<-asa$get_players(leagues="mls")
all_salaries_2024<-asa$get_player_salaries(leagues = "mls", 
                                           start_date="2024-09-13", 
                                           end_date="2024-09-14")
all_salaries_24<-all_salaries_2024|>
  left_join(mls_players|>
              select(player_id, player_name), by="player_id")|>
  left_join(mls_teams|>
              select(team_id, team=team_abbreviation), by="team_id")

all_salaries_24<-all_salaries_24|>
  mutate(year = 2024,
         team_year = paste(team, year, sep = "_")) |>
  relocate(team_year)


all_salaries_2023<-asa$get_player_salaries(leagues = "mls", 
                                           start_date="2023-09-15", 
                                           end_date="2023-09-16")
all_salaries_23<-all_salaries_2023|>
  left_join(mls_players|>
              select(player_id, player_name), by="player_id")|>
  left_join(mls_teams|>
              select(team_id, team=team_abbreviation), by="team_id")

all_salaries_23<-all_salaries_23|>
  mutate(year = 2023,
         team_year = paste(team, year, sep = "_")) |>
  relocate(team_year)

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

all_salaries_22<-all_salaries_22|>
  mutate(year = 2022,
         team_year = paste(team, year, sep = "_")) |>
  relocate(team_year)

all_salaries_2021<-asa$get_player_salaries(leagues = "mls", 
                                           start_date="2021-09-30", 
                                           end_date="2021-10-01")
all_salaries_21<-all_salaries_2021|>
  left_join(mls_players|>
              select(player_id, player_name), by="player_id")|>
  left_join(mls_teams|>
              select(team_id, team=team_abbreviation), by="team_id")

all_salaries_21<-all_salaries_21|>
  mutate(year = 2021,
         team_year = paste(team, year, sep = "_")) |>
  relocate(team_year)

top18_21<-all_salaries_21|>
  group_by(team_year)|>
  arrange(desc(guaranteed_compensation), .bygroup=TRUE)|>
  slice_head(n=18)|>
  mutate(rank_18=row_number())|>
  ungroup()

top18_22<-all_salaries_22|>
  group_by(team_year)|>
  arrange(desc(guaranteed_compensation), .bygroup=TRUE)|>
  slice_head(n=18)|>
  mutate(rank_18=row_number())|>
  ungroup()

top18_23<-all_salaries_23|>
  group_by(team_year)|>
  arrange(desc(guaranteed_compensation), .bygroup=TRUE)|>
  slice_head(n=18)|>
  mutate(rank_18=row_number())|>
  ungroup()

top18_24<-all_salaries_24|>
  group_by(team_year)|>
  arrange(desc(guaranteed_compensation), .bygroup=TRUE)|>
  slice_head(n=18)|>
  mutate(rank_18=row_number())|>
  ungroup()

# Bucketing salaries into 6 groups of 3
six_buckets_21<-top18_21|>
  mutate(tier=case_when(
    rank_18<=3 ~ "1-3",
    rank_18<=6 ~ "4-6",
    rank_18<=9 ~ "7-9",
    rank_18<=12 ~ "10-12",
    rank_18<=15 ~ "13-15",
    TRUE ~ "16-18"))|>
  group_by(team_year, tier)|>
  summarise(tier_comp = sum(guaranteed_compensation, na.rm = TRUE), .groups = "drop") |>
  left_join(
    top18_21 |>
      group_by(team_year) |>
      summarise(team_total_comp = sum(guaranteed_compensation, na.rm = TRUE), .groups = "drop"),
    by = "team_year"
  ) |>
  mutate(percent_of_team_total = (tier_comp / team_total_comp) * 100)|>
  select(team_year, tier, percent_of_team_total) |>
  pivot_wider(names_from = tier, values_from = percent_of_team_total)|>
  select(team_year, "1-3", "4-6", "7-9", "10-12", "13-15", "16-18")

six_buckets_22<-top18_22|>
  mutate(tier=case_when(
    rank_18<=3 ~ "1-3",
    rank_18<=6 ~ "4-6",
    rank_18<=9 ~ "7-9",
    rank_18<=12 ~ "10-12",
    rank_18<=15 ~ "13-15",
    TRUE ~ "16-18"))|>
  group_by(team_year, tier)|>
  summarise(tier_comp = sum(guaranteed_compensation, na.rm = TRUE), .groups = "drop") |>
  left_join(
    top18_22 |>
      group_by(team_year) |>
      summarise(team_total_comp = sum(guaranteed_compensation, na.rm = TRUE), .groups = "drop"),
    by = "team_year"
  ) |>
  mutate(percent_of_team_total = (tier_comp / team_total_comp) * 100)|>
  select(team_year, tier, percent_of_team_total) |>
  pivot_wider(names_from = tier, values_from = percent_of_team_total)|>
  select(team_year, "1-3", "4-6", "7-9", "10-12", "13-15", "16-18")

six_buckets_23<-top18_23|>
  mutate(tier=case_when(
    rank_18<=3 ~ "1-3",
    rank_18<=6 ~ "4-6",
    rank_18<=9 ~ "7-9",
    rank_18<=12 ~ "10-12",
    rank_18<=15 ~ "13-15",
    TRUE ~ "16-18"))|>
  group_by(team_year, tier)|>
  summarise(tier_comp = sum(guaranteed_compensation, na.rm = TRUE), .groups = "drop") |>
  left_join(
    top18_23 |>
      group_by(team_year) |>
      summarise(team_total_comp = sum(guaranteed_compensation, na.rm = TRUE), .groups = "drop"),
    by = "team_year"
  ) |>
  mutate(percent_of_team_total = (tier_comp / team_total_comp) * 100)|>
  select(team_year, tier, percent_of_team_total) |>
  pivot_wider(names_from = tier, values_from = percent_of_team_total)|>
  select(team_year, "1-3", "4-6", "7-9", "10-12", "13-15", "16-18")

six_buckets_24<-top18_24|>
  mutate(tier=case_when(
    rank_18<=3 ~ "1-3",
    rank_18<=6 ~ "4-6",
    rank_18<=9 ~ "7-9",
    rank_18<=12 ~ "10-12",
    rank_18<=15 ~ "13-15",
    TRUE ~ "16-18"))|>
  group_by(team_year, tier)|>
  summarise(tier_comp = sum(guaranteed_compensation, na.rm = TRUE), .groups = "drop") |>
  left_join(
    top18_24 |>
      group_by(team_year) |>
      summarise(team_total_comp = sum(guaranteed_compensation, na.rm = TRUE), .groups = "drop"),
    by = "team_year"
  ) |>
  mutate(percent_of_team_total = (tier_comp / team_total_comp) * 100)|>
  select(team_year, tier, percent_of_team_total) |>
  pivot_wider(names_from = tier, values_from = percent_of_team_total)|>
  select(team_year, "1-3", "4-6", "7-9", "10-12", "13-15", "16-18")

six_buckets<-bind_rows(six_buckets_21, six_buckets_22,
                       six_buckets_23, six_buckets_24)
mls_team_analysis<-mls_team_analysis|>
  left_join(six_buckets, by="team_year")

six_buckets_df<-mls_team_analysis|>
  select(team_year, year, xgoal_difference, "1-3", "4-6", "7-9", "10-12", "13-15", "16-18")
model_columns_1_6 <- c("1-3", "4-6", "7-9", "10-12", "13-15", "16-18")
model_columns_2_6<- c( "4-6", "7-9", "10-12", "13-15", "16-18")

set.seed(123)
train_data <- six_buckets_df|> filter(year %in% c("2021", "2022", "2023"))
test_data  <- six_buckets_df |> filter(year == "2024")

train_x <- as.matrix(select(train_data, all_of(model_columns_1_6)))
test_x  <- as.matrix(select(test_data, all_of(model_columns_1_6)))
train_y <- train_data$xgoal_difference
test_y  <- test_data$xgoal_difference

lm_train_x <- as.matrix(select(train_data, all_of(model_columns_2_6)))
lm_test_x  <- as.matrix(select(test_data, all_of(model_columns_2_6)))

# Linear
lm_fit <- lm(
  xgoal_difference ~ .,
  data = train_data |> select(xgoal_difference, all_of(model_columns_2_6))
)

# Ridge, Lasso, Elastic Net
ridge_fit <- cv.glmnet(train_x, train_y, alpha = 0)
lasso_fit <- cv.glmnet(train_x, train_y, alpha = 1)
enet_fit  <- cv.glmnet(train_x, train_y, alpha = 0.5)

# XGBoost
xgb_train <- xgb.DMatrix(data = train_x, label = train_y)
xgb_test  <- xgb.DMatrix(data = test_x)
xgb_fit <- xgboost(
  data = xgb_train,
  nrounds = 100,
  objective = "reg:squarederror",
  subsample = 1,
  colsample_bytree = 1,
  seed = 123,
  verbose = 0
)

# 5. Make predictions
preds <- tibble(
  year = test_data$year,
  team_year = test_data$team_year,
  actual = test_y,
  lm_pred = predict(lm_fit, newdata = test_data |> select(all_of(model_columns_2_6))),
  ridge_pred = as.numeric(predict(ridge_fit, newx = test_x, s = "lambda.min")),
  lasso_pred = as.numeric(predict(lasso_fit, newx = test_x, s = "lambda.min")),
  enet_pred  = as.numeric(predict(enet_fit, newx = test_x, s = "lambda.min")),
  xgb_pred = predict(xgb_fit, xgb_test)
)

# 6. Calculate RMSE for each model on 2024
rmse_2024 <- preds |>
  pivot_longer(lm_pred:xgb_pred, names_to = "model", values_to = "prediction") |>
  group_by(model) |>
  summarize(
    rmse_2024 = sqrt(mean((prediction - actual)^2)),
    .groups = "drop"
  )

# Defining the RMSE function
rmse <- function(actual, pred) sqrt(mean((actual - pred)^2))

# Bootstraping
n_boot <- 1000
boot_rmses <- replicate(n_boot, {
  sample_indices <- sample(1:nrow(preds), replace = TRUE)
  sampled <- preds[sample_indices, ]
  
  c(
    lm = rmse(sampled$actual, sampled$lm_pred),
    ridge = rmse(sampled$actual, sampled$ridge_pred),
    lasso = rmse(sampled$actual, sampled$lasso_pred),
    enet = rmse(sampled$actual, sampled$enet_pred),
    xgb = rmse(sampled$actual, sampled$xgb_pred)
  )
}, simplify = TRUE)

# Converting to tibble
boot_rmses_df <- as_tibble(t(boot_rmses)) |> 
  mutate(iteration = 1:n()) |> 
  pivot_longer(-iteration, names_to = "model", values_to = "rmse")

# Summary stats
boot_rmses_df |> group_by(model) |> 
  summarize(mean_rmse = mean(rmse), sd_rmse = sd(rmse))

# Plotting the RMSE distribution
ggplot(boot_rmses_df, aes(x = rmse, fill = model)) +
  geom_density(alpha = 0.4) +
  labs(title = "Bootstrapped RMSE Distributions", x = "RMSE", y = "Density") +
  theme_minimal()+
  theme(plot.title=element_text(size=20, face="bold", hjust=.5))

summary(lm_fit)

hist(lm_fit$residuals)
plot(lm_fit$residuals, lm_fit$fitted.values)

# EDA for even vs. top heavy salary spreads

library(ggplot2)
library(scales)
all_salaries_21|>
  filter(team=="NYRB")|>
  ggplot(aes(x=guaranteed_compensation))+
  stat_ecdf()+
  geom_rug(aes(color=guaranteed_compensation), alpha=.5)+
  scale_x_continuous(labels = scales::label_comma())+
  scale_color_continuous(labels = label_comma()) +
  theme_minimal()+
  labs(title="ECDF For New York Red Bulls Salary 2021",
       x="Guaranteed Compensation",
       color="Guaranteed Compensation")+
  theme(plot.title=element_text(size=18, face="bold", hjust=.5))

all_salaries_24|>
  filter(team=="MIA")|>
  ggplot(aes(x=guaranteed_compensation))+
  stat_ecdf()+
  geom_rug(aes(color=guaranteed_compensation), alpha=.5)+
  scale_x_continuous(labels = scales::label_comma())+
  scale_color_continuous(labels = label_comma()) +
  theme_minimal()+
  labs(title="ECDF For Inter Miami Salary 2024",
       x="Guaranteed Compensation",
       color="Guaranteed Compensation")+
  theme(plot.title=element_text(size=18, face="bold", hjust=.5))

ga_points_lm<-lm(points~total_goals_added_for, data=mls_team_analysis)
ga_points_lm|>
  tidy()|>
  knitr::kable(digits = 5,
               col.names = c("Term", "Estimate", "SE", "t", "p"))
cor(mls_team_analysis$total_goals_added_for, mls_team_analysis$points)
