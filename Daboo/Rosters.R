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
ggplot(aes())+
  geom_histogram()


ggplot(test_2024_results, aes(x = actual, y = predicted)) +
  geom_point(color = "steelblue", size = 3, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs. Actual xGD (2024)", x = "Actual xGD", y = "Predicted xGD") +
  theme_minimal()

# Cross-validation by year on all 5 models
library(tidyverse)
library(glmnet)
library(xgboost)
library(ggplot2)

# 1. Add year column (extracted from team_year)
advanced_model_df_1_18 <- advanced_model_df_1_18 |>
  mutate(year = str_extract(team_year, "\\d{4}"))

# 2. Define features
model_columns_1_18 <- c("1st", "2nd", "3rd", paste0(4:18, "th"))
model_columns_2_18<- c("2nd", "3rd", paste0(4:18, "th"))

# 3. Split train/test
set.seed(123)
train_data <- advanced_model_df_1_18 |> filter(year %in% c("2021", "2022", "2023"))
test_data  <- advanced_model_df_1_18 |> filter(year == "2024")

train_x <- as.matrix(select(train_data, all_of(model_columns_1_18)))
test_x  <- as.matrix(select(test_data, all_of(model_columns_1_18)))
train_y <- train_data$xgoal_difference
test_y  <- test_data$xgoal_difference

lm_train_x <- as.matrix(select(train_data, all_of(model_columns_2_18)))
lm_test_x  <- as.matrix(select(test_data, all_of(model_columns_2_18)))
# 4. Fit models
# Linear
lm_fit <- lm(
  xgoal_difference ~ .,
  data = train_data |> select(xgoal_difference, all_of(model_columns_2_18))
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
  seed = 123,  # fix this!
  verbose = 0
)

# 5. Make predictions
preds <- tibble(
  year = test_data$year,
  team_year = test_data$team_year,
  actual = test_y,
  lm_pred = predict(lm_fit, newdata = test_data |> select(all_of(model_columns_2_18))),
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

print(rmse_2024)

summary(lm_fit)

# Booststrapping to compare rmse's across more samples

set.seed(123) 
library(tidyverse)

# Define RMSE function
rmse <- function(actual, pred) sqrt(mean((actual - pred)^2))

# Bootstrap
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

# Convert to tibble
boot_rmses_df <- as_tibble(t(boot_rmses)) |> 
  mutate(iteration = 1:n()) |> 
  pivot_longer(-iteration, names_to = "model", values_to = "rmse")

# Summary stats
boot_rmses_df |> group_by(model) |> 
  summarize(mean_rmse = mean(rmse), sd_rmse = sd(rmse))

# Plot RMSE distribution
ggplot(boot_rmses_df, aes(x = rmse, fill = model)) +
  geom_density(alpha = 0.4) +
  labs(title = "Bootstrapped RMSE Distributions", x = "RMSE", y = "Density") +
  theme_minimal()

summary(enet_fit)

# Finding important variables with enet

# 1. Extract coefficients from elastic net model at optimal lambda
enet_coef <- coef(enet_fit, s = "lambda.min")

enet_coef_mat <- as.matrix(enet_coef)
str(enet_coef_mat)

# 2. Convert sparse matrix to tidy data frame
enet_coef_df <- enet_coef %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  rename(coefficient = `s0`)

# 3. Filter out intercept and zero coefficients
enet_coef_filtered <- enet_coef_df %>%
  filter(variable != "(Intercept)", coefficient != 0) %>%
  arrange(desc(abs(coefficient)))

# 4. Plot important variables
ggplot(enet_coef_filtered, aes(x = reorder(variable, abs(coefficient)), y = coefficient)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Elastic Net: Most Important Roster Tiers",
    x = "Roster Tier",
    y = "Coefficient"
  ) +
  theme_minimal()

# Adding error bars to plots
library(glmnet)
library(dplyr)

set.seed(123)
n_boot <- 1000
boot_coefs <- vector("list", n_boot)

for (i in 1:n_boot) {
  boot_idx <- sample(1:nrow(train_x), replace = TRUE)
  boot_x <- train_x[boot_idx, ]
  boot_y <- train_y[boot_idx]
  
  boot_fit <- cv.glmnet(boot_x, boot_y, alpha = 0.5)
  boot_coef <- as.matrix(coef(boot_fit, s = "lambda.min"))
  boot_coefs[[i]] <- boot_coef
}

# Combine into matrix
coef_mat <- do.call(cbind, boot_coefs)

# Convert to summary dataframe
coef_df <- as.data.frame(coef_mat)
coef_df$variable <- rownames(coef_mat)

# Get mean and sd across bootstrap samples
coef_summary <- coef_df %>%
  pivot_longer(-variable, names_to = "iteration", values_to = "coefficient") %>%
  group_by(variable) %>%
  summarize(
    mean_coef = mean(coefficient),
    sd_coef = sd(coefficient),
    .groups = "drop"
  )

# Filter out intercept and near-zero mean coefficients
coef_plot_df <- coef_summary %>%
  filter(variable != "(Intercept)", abs(mean_coef) > 1e-6) %>%
  arrange(desc(abs(mean_coef)))

# Plot
library(ggplot2)

ggplot(coef_plot_df, aes(x = reorder(variable, abs(mean_coef)), y = mean_coef)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(
    aes(ymin = mean_coef - sd_coef, ymax = mean_coef + sd_coef),
    width = 0.3,
    color = "black"
  ) +
  coord_flip() +
  labs(
    title = "Elastic Net: Coefficient Estimates with Bootstrapped Error Bars",
    x = "Roster Tier",
    y = "Mean Coefficient (± SD)"
  ) +
  theme_minimal()


# Finding optimal salary percentages with elastic net model

# 1. Model input columns
model_columns_1_18 <- c("1st", "2nd", "3rd", paste0(4:18, "th"))

# 2. Objective function: NEGATIVE prediction (since constrOptim minimizes)
enet_objective <- function(percentages) {
  # Enforce hard constraint on sum and value range
  if (any(percentages < 0) || any(percentages > 1) || abs(sum(percentages) - 1) > 1e-6) {
    return(1e6)
  }
  
  # Create named input row
  input_matrix <- matrix(percentages, nrow = 1)
  colnames(input_matrix) <- model_columns_1_18
  
  # Predict using elastic net
  pred <- predict(enet_fit, newx = input_matrix, s = "lambda.min")
  
  return(as.numeric(pred))  # negative for maximization
}

# 3. Initial guess (even split)
init <- rep(1 / length(model_columns_1_18), length(model_columns_1_18))

# 4. Constraints for constrOptim
ui_matrix <- rbind(
  diag(18),          # percentages >= 0
  -diag(18),         # percentages <= 1
  rep(1, 18)         # sum == 1
)
ci_vector <- c(
  rep(0, 18),        # lower bounds
  rep(-1, 18),       # upper bounds (negated)
  1                  # sum == 1
)

# 5. Run optimizer
res <- constrOptim(
  theta = init,
  f = enet_objective,
  grad = NULL,
  ui = ui_matrix,
  ci = ci_vector
)

# 6. Results
optimal_allocation <- tibble(
  tier = model_columns_1_18,
  percentage = res$par
)

predicted_xG_diff <- -res$value  # un-negate to get actual prediction

# 7. Output
print(optimal_allocation |> arrange(desc(percentage)))
cat("\nPredicted xG difference with optimal allocation:", predicted_xG_diff, "\n")


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
lm_test_x  <- as.matrix(select(test_data, all_of(model_columns_1_6)))

                        
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

print(rmse_2024)

summary(lm_fit)


set.seed(123) 
library(tidyverse)

# Define RMSE function
rmse <- function(actual, pred) sqrt(mean((actual - pred)^2))

# Bootstrap
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

# Convert to tibble
boot_rmses_df <- as_tibble(t(boot_rmses)) |> 
  mutate(iteration = 1:n()) |> 
  pivot_longer(-iteration, names_to = "model", values_to = "rmse")

# Summary stats
boot_rmses_df |> group_by(model) |> 
  summarize(mean_rmse = mean(rmse), sd_rmse = sd(rmse))

# Plot RMSE distribution
ggplot(boot_rmses_df, aes(x = rmse, fill = model)) +
  geom_density(alpha = 0.4) +
  labs(title = "Bootstrapped RMSE Distributions", x = "RMSE", y = "Density") +
  theme_minimal()+
  theme(plot.title=element_text(size=20, face="bold", hjust=.5))

grid <- expand.grid(
  leftmiddle3_pct = seq(1, 50, by = .5),
  middle3_pct     = seq(1, 20, by = .5),
  rightmiddle3_pct= seq(1, 20, by = .5),
  bottom3_pct     = seq(1, 20, by = .5)
) |>
  mutate(top3_pct = 100 - leftmiddle3_pct - middle3_pct - rightmiddle3_pct - bottom3_pct) |>
  filter(top3_pct >= 30 & top3_pct<=80) |>    # Optional: exclude invalid negative percentages
  as_tibble()

grid_model <- grid |>
  rename(
    `4-6` = leftmiddle3_pct,
    `7-9` = middle3_pct,
    `10-12` = rightmiddle3_pct,
    `13-15` = bottom3_pct,
    `16-18` = top3_pct
  )

grid$predicted_xG_diff <- predict(lm_fit, newdata = grid_model)

best_combo <- grid |>
  arrange(desc(predicted_xG_diff)) |>
  dplyr::slice(1)

print(best_combo)

# plotting confidence intervals for linear model
coefs <- summary(lm_fit)$coefficients
conf_int <- confint(lm_fit)

coef_df <- as.data.frame(coefs)
coef_df$term <- rownames(coef_df)
conf_df <- as.data.frame(conf_int)
names(conf_df) <- c("conf_low", "conf_high")
conf_df$term <- rownames(conf_df)

# Merge
plot_df <- merge(coef_df, conf_df, by = "term")

ggplot(plot_df |> filter(term != "(Intercept)"), aes(x = term, y = Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "95% Confidence Intervals for Coefficients (Linear Model)",
       x = "Predictor", y = "Estimate") +
  theme_minimal()

# Finding each team's indivudal xG prediction for 2024
lm_predictions <- predict(lm_fit, newdata = test_data |> select(all_of(model_columns_2_6)))

# Combine with test_data and calculate residuals
lm_results <- test_data |>
  mutate(
    predicted_xG_diff = lm_predictions,
    residual = xgoal_difference - predicted_xG_diff
  ) |>
  select(team_year, xgoal_difference, predicted_xG_diff, residual)

top_accurate_lm_preds <- lm_results |>
  mutate(abs_residual = abs(residual)) |>
  arrange(abs_residual) |>
  slice_head(n = 30)  # Change to `n = 5` if you want top 5

print(top_accurate_lm_preds, n=30)

lm_model <- lm(
  xgoal_difference ~ .,
  data = mls_team_analysis |> select(xgoal_difference, all_of(model_columns_2_6))
)

summary(lm_model)
