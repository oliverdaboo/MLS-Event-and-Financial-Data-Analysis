## Structuring rosters in 3, 6, 9 format

# Getting top 18 highest paid players for every team for the last 4 MLS seasons
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

# Calculating the percent of total team salary spent in the 3 groups

percents_21<-top18_21|>
  mutate(tier=case_when(
    rank_18<=3 ~ "DP",
    rank_18<=9 ~ "TAM",
    TRUE ~ "GAM"))|>
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
  relocate(TAM, .before=3)

percents_22<-top18_22|>
  mutate(tier=case_when(
    rank_18<=3 ~ "DP",
    rank_18<=9 ~ "TAM",
    TRUE ~ "GAM"))|>
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
  relocate(TAM, .before=3)

percents_23<-top18_23|>
  mutate(tier=case_when(
    rank_18<=3 ~ "DP",
    rank_18<=9 ~ "TAM",
    TRUE ~ "GAM"))|>
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
  relocate(TAM, .before=3)

percents_24<-top18_24|>
  mutate(tier=case_when(
    rank_18<=3 ~ "DP",
    rank_18<=9 ~ "TAM",
    TRUE ~ "GAM"))|>
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
  relocate(TAM, .before=3)

# Adding the percent breaksdowns to the main data set
salary_percents<-bind_rows(percents_21, percents_22,
                              percents_23, percents_24)
mls_team_analysis<-mls_team_analysis|>
  left_join(salary_percents, by="team_year")

# Modeling xG Diff on 21-23, testing on 24
library(caret)
train_data<-mls_team_analysis|>
  filter(year>=2021 & year<=2023)
test_data<-mls_team_analysis|>
  filter(year==2024)

designation_percent_formula<-xgoal_difference ~ DP + TAM

designation_percent_model<-train(designation_percent_formula,
                            data=train_data,
                            method="lm")
summary(designation_percent_model$finalModel)

designation_prediction<-predict(designation_percent_model, newdata=test_data)

designation_results<-test_data|>
  mutate(pred_xG_diff=designation_prediction,
         residuals=xgoal_difference-pred_xG_diff)

designation_rmse <- sqrt(mean(designation_results$residuals^2))
designation_mae <- mean(abs(designation_results$residuals))
designation_rmse
designation_mae

# Different models on xG diff for 21-24

# setting up cross-validation
set.seed(100)
N_FOLDS <- 5
mls_team_analysis <- mls_team_analysis |>
  mutate(fold = sample(rep(1:N_FOLDS, length.out = n())))
table(mls_team_analysis$fold)

designation_cv<- function(x){
  # test and training data
  cv_test_data<-mls_team_analysis|> filter(fold==x)
  cv_train_data<-mls_team_analysis|> filter(fold!=x)
  
  # fitting models to training data
  dp_lm<-lm(xgoal_difference ~ DP, data=cv_train_data)
  tam_lm<-lm(xgoal_difference ~ TAM, data=cv_train_data)
  dp_tam_interaction_lm<-lm(xgoal_difference ~ DP*TAM, data=cv_train_data)
  dp_tam_mlr<-lm(xgoal_difference ~ DP + TAM, data=cv_train_data)
  
  # returning test results
  out<-tibble(
    dp_pred=predict(dp_lm, newdata = cv_test_data),
    tam_pred=predict(tam_lm, newdata = cv_test_data),
    dp_tam_interaction_pred=predict(dp_tam_interaction_lm, newdata = cv_test_data),
    dp_tam_mlr_pred=predict(dp_tam_mlr, newdata = cv_test_data),
    cv_test_actual=cv_test_data$xgoal_difference,
    cv_test_fold=x
  )
  return(out)
}

designation_preds<-map(1:N_FOLDS, designation_cv)|>
  bind_rows()
designation_preds

# Computing the RMSE across the folds 
designation_models_summary<-designation_preds|>
  pivot_longer(dp_pred:dp_tam_mlr_pred, names_to="model", values_to="test_pred")|>
  group_by(model, cv_test_fold)|>
  summarize(rmse=sqrt(mean((cv_test_actual-test_pred)^2)))

designation_models_summary|>
  group_by(model)|>
  summarize(avg_cv_rmse=mean(rmse),
            sd_rmse=sd(rmse),
            k=n())|>
  mutate(se_rmse=sd_rmse/sqrt(k),
         lower_rmse=avg_cv_rmse-2*se_rmse,
         upper_rmse=avg_cv_rmse+2*se_rmse)

# Visualizing designation_cv results

designation_models_summary|>
  ggplot(aes(x=model, y=rmse))+
  geom_point(size = 4) +
  stat_summary(fun = mean, geom = "point", 
               color = "red", size = 4) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               color = "red", width = 0.2)

# ECDF Plots for certain teams

ecdf_plots<-function(team_input){
  mls_team_analysis|>
    filter(team_year==team_input)|>
    ggplot(aes(x=total_guaranteed_compensation))+
    stat_ecdf()+
    labs(title=paste("ECDF for ", team_input),
         x="Guaranteed Compensation")
}

# Inter Miami 2024
ecdf_plots("MIA_2024")
ecdf_plots("ATL_2024")

# Splitting up rosters by each player 1-18
single_percents_21<-top18_21|>
  mutate(tier=case_when(
    rank_18<=1 ~ "1st",
    rank_18<=2 ~ "2nd",
    rank_18<=3 ~ "3rd",
    rank_18<=4 ~ "4th",
    rank_18<=5 ~ "5th",
    rank_18<=6 ~ "6th",
    rank_18<=7 ~ "7th",
    rank_18<=8 ~ "8th",
    rank_18<=9 ~ "9th",
    rank_18<=10 ~ "10th",
    rank_18<=11 ~ "11th",
    rank_18<=12 ~ "12th",
    rank_18<=13 ~ "13th",
    rank_18<=14 ~ "14th",
    rank_18<=15 ~ "15th",
    rank_18<=16 ~ "16th",
    rank_18<=17 ~ "17th",
    TRUE ~ "18th"))|>
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
  select(team_year, "1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th", 
         "10th", "11th", "12th", "13th", "14th", "15th", "16th", "17th", "18th")

single_percents_22<-top18_22|>
  mutate(tier=case_when(
    rank_18<=1 ~ "1st",
    rank_18<=2 ~ "2nd",
    rank_18<=3 ~ "3rd",
    rank_18<=4 ~ "4th",
    rank_18<=5 ~ "5th",
    rank_18<=6 ~ "6th",
    rank_18<=7 ~ "7th",
    rank_18<=8 ~ "8th",
    rank_18<=9 ~ "9th",
    rank_18<=10 ~ "10th",
    rank_18<=11 ~ "11th",
    rank_18<=12 ~ "12th",
    rank_18<=13 ~ "13th",
    rank_18<=14 ~ "14th",
    rank_18<=15 ~ "15th",
    rank_18<=16 ~ "16th",
    rank_18<=17 ~ "17th",
    TRUE ~ "18th"))|>
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
  select(team_year, "1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th", 
         "10th", "11th", "12th", "13th", "14th", "15th", "16th", "17th", "18th")

single_percents_23<-top18_23|>
  mutate(tier=case_when(
    rank_18<=1 ~ "1st",
    rank_18<=2 ~ "2nd",
    rank_18<=3 ~ "3rd",
    rank_18<=4 ~ "4th",
    rank_18<=5 ~ "5th",
    rank_18<=6 ~ "6th",
    rank_18<=7 ~ "7th",
    rank_18<=8 ~ "8th",
    rank_18<=9 ~ "9th",
    rank_18<=10 ~ "10th",
    rank_18<=11 ~ "11th",
    rank_18<=12 ~ "12th",
    rank_18<=13 ~ "13th",
    rank_18<=14 ~ "14th",
    rank_18<=15 ~ "15th",
    rank_18<=16 ~ "16th",
    rank_18<=17 ~ "17th",
    TRUE ~ "18th"))|>
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
  select(team_year, "1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th", 
         "10th", "11th", "12th", "13th", "14th", "15th", "16th", "17th", "18th")

single_percents_24<-top18_24|>
  mutate(tier=case_when(
    rank_18<=1 ~ "1st",
    rank_18<=2 ~ "2nd",
    rank_18<=3 ~ "3rd",
    rank_18<=4 ~ "4th",
    rank_18<=5 ~ "5th",
    rank_18<=6 ~ "6th",
    rank_18<=7 ~ "7th",
    rank_18<=8 ~ "8th",
    rank_18<=9 ~ "9th",
    rank_18<=10 ~ "10th",
    rank_18<=11 ~ "11th",
    rank_18<=12 ~ "12th",
    rank_18<=13 ~ "13th",
    rank_18<=14 ~ "14th",
    rank_18<=15 ~ "15th",
    rank_18<=16 ~ "16th",
    rank_18<=17 ~ "17th",
    TRUE ~ "18th"))|>
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
  select(team_year, "1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th", 
         "10th", "11th", "12th", "13th", "14th", "15th", "16th", "17th", "18th")

single_percents<-bind_rows(single_percents_21, single_percents_22,
                           single_percents_23, single_percents_24)
mls_team_analysis<-mls_team_analysis|>
  left_join(single_percents, by="team_year")

# Setting up advanced modeling cross-validation

library(glmnet)
set.seed(123)
n_folds <- 5
mls_team_analysis <- mls_team_analysis |>
  mutate(folds = sample(rep(1:n_folds, length.out = n())))
table(mls_team_analysis$folds)

library(xgboost)
# Excluding the 1st column

model_columns_1_17 <- paste0(as.character(1:17), "th")  # "1st" through "17th"

# Fixing corner cases for 1st, 2nd, 3rd 
model_columns_1_17[1:3] <- c("1st", "2nd", "3rd")

# Create the df for columns I need for the model
advanced_model_df_1_17 <- mls_team_analysis |>
  select(team_year, xgoal_difference, all_of(model_columns_1_17), folds)

# Creating the cv function
advanced_models_cv <- function(x){
  cv_test_data <- advanced_model_df_1_17 |> filter(folds == x)
  cv_train_data <- advanced_model_df_1_17 |> filter(folds != x)
  cv_test_x <- as.matrix(select(cv_test_data, all_of(model_columns_1_17)))
  cv_train_x <- as.matrix(select(cv_train_data, all_of(model_columns_1_17)))
  cv_train_y<- cv_train_data$xgoal_difference
  
  lm_fit <- lm(xgoal_difference ~ . - 1, data = cv_train_data |> 
                 select(xgoal_difference, all_of(model_columns_1_17)))
  ridge_fit <- cv.glmnet(cv_train_x, cv_train_data$xgoal_difference, alpha = 0)
  lasso_fit <- cv.glmnet(cv_train_x, cv_train_data$xgoal_difference, alpha = 1)
  enet_fit  <- cv.glmnet(cv_train_x, cv_train_data$xgoal_difference, alpha = 0.5)
  
  xgb_train<-xgb.DMatrix(data=cv_train_x, label=cv_train_y)
  xgb_test<-xgb.DMatrix(data=cv_test_x)
  
  xgb_fit <- xgboost(
    data = xgb_train,
    nrounds = 100,
    objective = "reg:squarederror",
    verbose = 0
  )
  
  xgb_pred <- predict(xgb_fit, xgb_test)
  
  out <- tibble(
    lm_pred    = predict(lm_fit, newdata = cv_test_data),
    ridge_pred = as.numeric(predict(ridge_fit, newx = cv_test_x)),
    lasso_pred = as.numeric(predict(lasso_fit, newx = cv_test_x)),
    enet_pred  = as.numeric(predict(enet_fit, newx = cv_test_x)),
    xgb_pred = xgb_pred,
    test_actual = cv_test_data$xgoal_difference,
    test_fold   = x
  )
  return(out)
}

# Running the cv across the 5 folds
advanced_models_preds <- map(1:n_folds, advanced_models_cv) |>
  bind_rows()

advanced_models_summary <- advanced_models_preds |>
  pivot_longer(lm_pred:xgb_pred, names_to = "model", values_to = "test_pred") |>
  group_by(model, test_fold) |>
  summarize(rmse = sqrt(mean((test_actual - test_pred)^2)), .groups= "drop")

# getting out statistics for the models
advanced_models_summary|>
  group_by(model) |> 
  summarize(avg_cv_rmse = mean(rmse),
            sd_rmse = sd(rmse),
            k = n()) |>
  mutate(se_rmse = sd_rmse / sqrt(k),
         lower_rmse = avg_cv_rmse - 2*se_rmse,
         upper_rmse = avg_cv_rmse + 2*se_rmse)

# Plotting results
advanced_models_summary |>
  ggplot(aes(x = model, y = rmse)) + 
  geom_point(size = 4) +
  stat_summary(fun = mean, geom = "point", 
               color = "red", size = 4) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               color = "red", width = 0.2)
# Finding the most important XGBoost variables
X_full <- as.matrix(select(advanced_model_df_1_17, all_of(model_columns_1_17)))
y_full <- advanced_model_df_1_17$xgoal_difference

# Convert to DMatrix
xgb_train_full <- xgb.DMatrix(data = X_full, label = y_full)

# Train full XGBoost model
xgb_fit_full <- xgboost(
  data = xgb_train_full,
  nrounds = 100,
  objective = "reg:squarederror",
  verbose = 0
)

# Get importance matrix
xgb_importance <- xgb.importance(feature_names = model_columns_1_17, model = xgb_fit_full)

# Plot
xgb.plot.importance(xgb_importance, top_n = 15, rel_to_first = TRUE, 
                    xlab = "Relative Importance", main = "XGBoost Feature Importance")

xgb_importance |>
  mutate(Feature = reorder(Feature, Gain)) |>
  ggplot(aes(x = Feature, y = Gain)) +
  geom_col(fill = "darkblue") +
  coord_flip() +
  labs(title = "XGBoost Feature Importance (Gain)",
       x = "Player Tier",
       y = "Importance (Gain)") +
  theme_minimal()

## Modeling again but excluding the 1st column instead
library(xgboost)
# Excluding the 1st column

model_columns_2_18 <- paste0(as.character(2:18), "th")  # "1st" through "17th"

# Fixing corner cases for 1st, 2nd, 3rd 
model_columns_2_18[1:2] <- c("2nd", "3rd")

# Create the df for columns I need for the model
advanced_model_df_2_18 <- mls_team_analysis |>
  select(team_year, xgoal_difference, all_of(model_columns_2_18), folds)

# Creating the cv function
advanced_models_cv <- function(x){
  cv_test_data <- advanced_model_df_2_18 |> filter(folds == x)
  cv_train_data <- advanced_model_df_2_18 |> filter(folds != x)
  cv_test_x <- as.matrix(select(cv_test_data, all_of(model_columns_2_18)))
  cv_train_x <- as.matrix(select(cv_train_data, all_of(model_columns_2_18)))
  cv_train_y<- cv_train_data$xgoal_difference
  
  lm_fit <- lm(xgoal_difference ~ . - 1, data = cv_train_data |> 
                 select(xgoal_difference, all_of(model_columns_2_18)))
  ridge_fit <- cv.glmnet(cv_train_x, cv_train_data$xgoal_difference, alpha = 0)
  lasso_fit <- cv.glmnet(cv_train_x, cv_train_data$xgoal_difference, alpha = 1)
  enet_fit  <- cv.glmnet(cv_train_x, cv_train_data$xgoal_difference, alpha = 0.5)
  
  xgb_train<-xgb.DMatrix(data=cv_train_x, label=cv_train_y)
  xgb_test<-xgb.DMatrix(data=cv_test_x)
  
  xgb_fit <- xgboost(
    data = xgb_train,
    nrounds = 100,
    objective = "reg:squarederror",
    verbose = 0
  )
  
  xgb_pred <- predict(xgb_fit, xgb_test)
  
  out <- tibble(
    lm_pred    = predict(lm_fit, newdata = cv_test_data),
    ridge_pred = as.numeric(predict(ridge_fit, newx = cv_test_x)),
    lasso_pred = as.numeric(predict(lasso_fit, newx = cv_test_x)),
    enet_pred  = as.numeric(predict(enet_fit, newx = cv_test_x)),
    xgb_pred = xgb_pred,
    test_actual = cv_test_data$xgoal_difference,
    test_fold   = x
  )
  return(out)
}

# Running the cv across the 5 folds
advanced_models_preds <- map(1:n_folds, advanced_models_cv) |>
  bind_rows()

advanced_models_summary <- advanced_models_preds |>
  pivot_longer(lm_pred:xgb_pred, names_to = "model", values_to = "test_pred") |>
  group_by(model, test_fold) |>
  summarize(rmse = sqrt(mean((test_actual - test_pred)^2)), .groups= "drop")

# getting out statistics for the models
advanced_models_summary|>
  group_by(model) |> 
  summarize(avg_cv_rmse = mean(rmse),
            sd_rmse = sd(rmse),
            k = n()) |>
  mutate(se_rmse = sd_rmse / sqrt(k),
         lower_rmse = avg_cv_rmse - 2*se_rmse,
         upper_rmse = avg_cv_rmse + 2*se_rmse)

# Plotting results
advanced_models_summary |>
  ggplot(aes(x = model, y = rmse)) + 
  geom_point(size = 4) +
  stat_summary(fun = mean, geom = "point", 
               color = "red", size = 4) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               color = "red", width = 0.2)

# Finding most important variables for XGBoost
X_full <- as.matrix(select(advanced_model_df_2_18, all_of(model_columns_2_18)))
y_full <- advanced_model_df_2_18$xgoal_difference

# Convert to DMatrix
xgb_train_full <- xgb.DMatrix(data = X_full, label = y_full)

# Train full XGBoost model
xgb_fit_full <- xgboost(
  data = xgb_train_full,
  nrounds = 100,
  objective = "reg:squarederror",
  verbose = 0
)

# Get importance matrix
xgb_importance <- xgb.importance(feature_names = model_columns_2_18, model = xgb_fit_full)

# Plot
xgb.plot.importance(xgb_importance, top_n = 15, rel_to_first = TRUE, 
                    xlab = "Relative Importance", main = "XGBoost Feature Importance")

xgb_importance |>
  mutate(Feature = reorder(Feature, Gain)) |>
  ggplot(aes(x = Feature, y = Gain)) +
  geom_col(fill = "darkblue") +
  coord_flip() +
  labs(title = "XGBoost Feature Importance (Gain)",
       x = "Player Tier",
       y = "Importance (Gain)") +
  theme_minimal()

# Modeling XGBoost including all columns

library(tidyverse)
library(xgboost)
library(Matrix)
library(ggplot2)

# Columns from 1st to 18th
model_columns_1_18 <- c("1st", "2nd", "3rd", paste0(as.character(4:18), "th"))

# Dataset with folds and model columns
advanced_model_df_1_18 <- mls_team_analysis |> 
  select(team_year, xgoal_difference, all_of(model_columns_1_18), folds)

# Number of folds
n_folds <- length(unique(advanced_model_df_1_18$folds))

# Cross-validation function
xgb_cv_function <- function(x) {
  train_data <- advanced_model_df_1_18 |> filter(folds != x)
  test_data  <- advanced_model_df_1_18 |> filter(folds == x)
  
  dtrain <- xgb.DMatrix(data = as.matrix(train_data |> select(all_of(model_columns_1_18))),
                        label = train_data$xgoal_difference)
  dtest <- xgb.DMatrix(data = as.matrix(test_data |> select(all_of(model_columns_1_18))),
                       label = test_data$xgoal_difference)
  
  xgb_fit <- xgboost(data = dtrain,
                     objective = "reg:squarederror",
                     nrounds = 100,
                     verbose = 0)
  
  preds <- predict(xgb_fit, dtest)
  
  tibble(
    test_actual = test_data$xgoal_difference,
    test_pred = preds,
    test_fold = x
  )
}

# Run CV across folds
xgb_cv_preds <- map_dfr(1:n_folds, xgb_cv_function)

# RMSE summary
xgb_cv_summary <- xgb_cv_preds |>
  group_by(test_fold) |>
  summarize(rmse = sqrt(mean((test_actual - test_pred)^2))) |>
  ungroup()

xgb_cv_summary_stats <- xgb_cv_summary |>
  summarize(avg_rmse = mean(rmse),
            sd_rmse = sd(rmse),
            se_rmse = sd_rmse / sqrt(n()),
            lower = avg_rmse - 2 * se_rmse,
            upper = avg_rmse + 2 * se_rmse)

print(xgb_cv_summary_stats)

dall <- xgb.DMatrix(data = as.matrix(advanced_model_df_1_18 |> select(all_of(model_columns_1_18))),
                    label = advanced_model_df_1_18$xgoal_difference)

xgb_full_model <- xgboost(data = dall,
                          objective = "reg:squarederror",
                          nrounds = 100,
                          verbose = 0)

# Get feature importance
importance_matrix <- xgb.importance(model = xgb_full_model)

# Plot
xgb.plot.importance(importance_matrix,
                    main = "XGBoost Feature Importance (All Tiers)",
                    xlab = "Relative Importance")


# For PDP, we need the underlying data and model
xgb_model_wrap <- function(object, newdata) {
  predict(object, newdata = xgb.DMatrix(data = as.matrix(newdata)))
}

# Generate PDP for a given tier (example: "1st")
pdp_1st <- partial(xgb_full_model,
                   pred.var = "1st",
                   pred.fun = xgb_model_wrap,
                   train = advanced_model_df_1_18 |> select(all_of(model_columns_1_18)),
                   grid.resolution = 100)

# Plot it
plot(pdp_1st,
     main = "Partial Dependence for 1st Highest Paid Player",
     xlab = "Salary % for 1st",
     ylab = "Predicted xGD")

# Computing SHAP values and plotting
library(SHAPforxgboost)

X <- advanced_model_df_1_18 |> select(all_of(model_columns_1_18)) |> as.matrix()

shap_values <- shap.values(xgb_model = xgb_full_model, X_train = X)

shap_score <- shap_values$shap_score  # matrix of SHAP values
mean_shap_importance <- shap_values$mean_shap_score

shap_long <- shap.prep(shap_contrib = shap_score, X_train = X)
shap.plot.summary(shap_long)

shap.plot.dependence(data_long = shap_long, 
                     x = "7th", 
                     color_feature = "7th")

# Plotting SHAP values for all 18 columns

# Defining salary columns
model_columns_1_18 <- c("1st", "2nd", "3rd", paste0(4:18, "th"))

# Preparing training matrix
X <- advanced_model_df_1_18 |> select(all_of(model_columns_1_18)) |> as.matrix()

# Computing SHAP values
shap_values <- shap.values(xgb_model = xgb_full_model, X_train = X)
shap_score <- shap_values$shap_score

# Preparing for plotting
shap_long <- shap.prep(shap_contrib = shap_score, X_train = X)

# Looping over all 18 salary tiers to plot SHAP dependence
for (feature in model_columns_1_18) {
  print(shap.plot.dependence(data_long = shap_long, 
                       x = feature, 
                       color_feature = feature,
                       smooth = TRUE,
                       add_hist = TRUE,
                       alpha = 0.5,
                       size = 1,
                       main = paste("SHAP Dependence for", feature)))
}

library(pdp)
# Function wrapper for XGBoost predict
xgb_model_wrap <- function(object, newdata) {
  predict(object, newdata = xgb.DMatrix(data = as.matrix(newdata)))
}

# Define predictor function for XGBoost
xgb_predict_fun <- function(object, newdata) {
  predict(object, newdata = xgb.DMatrix(as.matrix(newdata)))
}

# Generate partial dependence data for each salary tier
pdp_list <- list()

for (feature in model_columns_1_18) {
  pdp_list[[feature]] <- pdp::partial(
    object = xgb_full_model,
    pred.var = feature,
    pred.fun = xgb_predict_fun,
    train = advanced_model_df_1_18 |> select(all_of(model_columns_1_18)),
    grid.resolution = 100,
    progress = "none"
  )
}

# Optimal Salaries

library(purrr)
library(tibble)
optimal_salaries <- map_dfr(names(pdp_list), function(feature) {
  pdp_df <- pdp_list[[feature]]
  
  # Check it's a data frame
  if (is.data.frame(pdp_df) && all(c("yhat", feature) %in% colnames(pdp_df))) {
    best_row <- pdp_df[which.max(pdp_df$yhat), ]
    tibble(
      tier = feature,
      optimal_salary_percent = best_row[[feature]],
      predicted_xgd = best_row$yhat
    )
  } else {
    # If not a valid PDP result, return NA
    tibble(
      tier = feature,
      optimal_salary_percent = NA_real_,
      predicted_xgd = NA_real_
    )
  }
})

print(optimal_salaries)
sum(optimal_salaries$optimal_salary_percent)

normalized_salaries<-optimal_salaries|>
  mutate(normalized_salary_percent=100*optimal_salary_percent/sum(optimal_salary_percent, na.rm=TRUE))
print(normalized_salaries)
sum(normalized_salaries$normalized_salary_percent)

ggplot(normalized_salaries, aes(x = reorder(tier, normalized_salary_percent), y = normalized_salary_percent)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Normalized Optimal Salary % by Tier (Total = 100%)",
    x = "Player Tier",
    y = "Normalized Salary %"
  ) +
  theme_minimal()

mean_salary_tiers<-mls_team_analysis|>
  select(all_of(model_columns_1_18))|>
  summarise(across(everything(), mean))|>
  pivot_longer(cols=everything(),
               names_to="tier",
               values_to="mean_salary_percents")
normalized_salaries<-normalized_salaries|>
  left_join(mean_salary_tiers, by="tier")

normalized_salaries<-normalized_salaries|>
  mutate(salary_diff=normalized_salary_percent-mean_salary_percents)

normalized_salaries|>
  arrange(desc(salary_diff))
