library(xgboost)
library(dplyr)
library(tidyr)
library(zoo)
library(caret)
library(Metrics)

player_data <- readRDS("data//player_cluster_data.rds")

head(player_data)

# Use machine learning and XGBoost to predict player stats
model_player_stats <- function(player_id, target_var) {
  player_data <- player_data %>%
    mutate(bats = as.factor(bats),
           throws = as.factor(throws))

  # Remove players with missing target variable
  player_data_clean <- player_data %>%
    filter(!is.na(.[[target_var]]))

  # Feature selection
  features <- player_data_clean %>%
    select(G, BA, HR, H, X2B, X3B, AB, R, RBI, BB, SO, SB, OBP, SLG,
           age, height, weight) %>%
    mutate(across(c(age, height, weight), scale)) 

  # Define target and remove it from features
  target <- player_data_clean[[target_var]]
  features <- features %>% select(-all_of(target_var))

  # Convert to matrix
  dtrain <- xgb.DMatrix(data = as.matrix(features), label = target)

  # XGBoost parameters
  params <- list(
    objective = "reg:squarederror",
    max_depth = 4,
    eta = 0.02,
    min_child_weight = 5,
    subsample = 0.8,
    colsample_bytree = 0.8,
    eval_metric = "rmse",
    lambda = 1,
    alpha = 0.5
  )

  xgb_cv <- xgb.cv(params = params, data = dtrain, nrounds = 200, nfold = 3,
                   early_stopping_rounds = 10, verbose = 0)

  best_nrounds <- xgb_cv$best_iteration

  # Train model
  model <- xgboost(params = params, data = dtrain,
                   nrounds = best_nrounds, verbose = 0)

  # Get past stats of the player
  latest_data <- player_data_clean %>%
    filter(playerID == player_id) %>%
    arrange(desc(yearID)) %>% 
    slice_head(n = 1) 

  # Create a new row for next year
  next_year_data <- latest_data %>%
    mutate(
      yearID = latest_data$yearID + 1,  # Predict for next year
      age = latest_data$age + 1,  # Increase player's age
    )

  # Prepare features for prediction
  predict_features <- next_year_data %>%
    select(G, BA, HR, H, X2B, X3B, AB, R, RBI, BB, SO, SB, OBP, SLG,
           age, height, weight) %>%
    mutate(across(c(age, height, weight), scale))
  predict_features <- predict_features %>% select(-all_of(target_var))

  # Convert to matrix
  predict_matrix <- as.matrix(predict_features)

  # Predict next year's value and return
  pred_value <- predict(model, predict_matrix)

  return(pred_value)
}

# Predict stats using clustering
predict_stats <- function(player_id, current_year) {
  player_cluster <- player_data %>%
    filter(playerID == player_id) %>%
    select(Cluster)

  if (nrow(player_cluster) == 0) {
    return(tibble(message = "Player not found"))
  }

  cluster_value <- tail(player_cluster$Cluster, n = 1)

  similar_players <- player_data %>%
    filter(Cluster == cluster_value, yearID < current_year)

  # Predict stats based on clusters using weighted.mean
  # Applies more weight or emphasis on more recent years
  next_year_stats <- similar_players %>%
    group_by(next_year = yearID + 1) %>%
    summarize(
      pred_g = weighted.mean(G, w = exp(yearID - min(yearID)), na.rm = TRUE),
      pred_hr = weighted.mean(HR, w = exp(yearID - min(yearID)), na.rm = TRUE),
      pred_h = weighted.mean(H, w = exp(yearID - min(yearID)), na.rm = TRUE),
      pred_2b = weighted.mean(X2B, w = exp(yearID - min(yearID)), na.rm = TRUE),
      pred_3b = weighted.mean(X3B, w = exp(yearID - min(yearID)), na.rm = TRUE),
      pred_ab = weighted.mean(AB, w = exp(yearID - min(yearID)), na.rm = TRUE),
      pred_r = weighted.mean(R, w = exp(yearID - min(yearID)), na.rm = TRUE),
      pred_rbi = weighted.mean(RBI, w = exp(yearID - min(yearID)),
                               na.rm = TRUE),
      pred_bb = weighted.mean(BB, w = exp(yearID - min(yearID)) / (1 + BB), 
                              na.rm = TRUE),
      pred_so = weighted.mean(SO, w = exp(yearID - min(yearID)), na.rm = TRUE),
      pred_sb = weighted.mean(SB, w = exp(yearID - min(yearID)), na.rm = TRUE)
    ) %>%
    filter(next_year == current_year)

  # Apply age adjustment to statistics
  age_adjustment <- similar_players %>%
    group_by(age) %>%
    summarize(
      avg_change_g = weighted.mean(G - lag(G), w = AB, na.rm = TRUE),
      avg_change_hr = weighted.mean(HR - lag(HR), w = AB, na.rm = TRUE),
      avg_change_h = weighted.mean(H - lag(H), w = AB, na.rm = TRUE),
      avg_change_2b = weighted.mean(X2B - lag(X2B), w = AB, na.rm = TRUE),
      avg_change_3b = weighted.mean(X3B - lag(X3B), w = AB, na.rm = TRUE),
      avg_change_ab = weighted.mean(AB - lag(AB), w = AB, na.rm = TRUE),
      avg_change_r = weighted.mean(R - lag(R), w = AB, na.rm = TRUE),
      avg_change_rbi = weighted.mean(RBI - lag(RBI), w = AB, na.rm = TRUE),
      avg_change_bb = weighted.mean(BB - lag(BB), w = AB, na.rm = TRUE),
      avg_change_so = weighted.mean(SO - lag(SO), w = AB, na.rm = TRUE),
      avg_change_sb = weighted.mean(SB - lag(SB), w = AB, na.rm = TRUE),
    )

  player_age <- player_data %>%
    filter(playerID == player_id) %>%
    pull(age)

  age_correction <- age_adjustment %>%
    filter(age == tail(player_age, n = 1) + 1)

  next_year_stats <- next_year_stats %>%
    mutate(
      pred_g = pred_g + ifelse(!is.nan(age_correction$avg_change_g),
                               age_correction$avg_change_g, 0),
      pred_hr = pred_hr + ifelse(!is.nan(age_correction$avg_change_hr),
                                 age_correction$avg_change_hr, 0),
      pred_h = pred_h + ifelse(!is.nan(age_correction$avg_change_h),
                               pmin(age_correction$avg_change_h, 0.4 *
                                      age_correction$avg_change_ab), 0),
      pred_2b = pred_2b + ifelse(!is.nan(age_correction$avg_change_2b),
                                 age_correction$avg_change_2b, 0),
      pred_3b = pred_3b + ifelse(!is.nan(age_correction$avg_change_3b),
                                 age_correction$avg_change_3b, 0),
      pred_ab = pred_ab + ifelse(!is.nan(age_correction$avg_change_ab),
                                 age_correction$avg_change_ab, 0),
      pred_r = pred_r + ifelse(!is.nan(age_correction$avg_change_r),
                               age_correction$avg_change_r, 0),
      pred_rbi = pred_rbi + ifelse(!is.nan(age_correction$avg_change_rbi),
                                   age_correction$avg_change_rbi, 0),
      pred_bb = pred_bb + ifelse(!is.nan(age_correction$avg_change_bb),
                                 age_correction$avg_change_bb, 0),
      pred_so = pred_so + ifelse(!is.nan(age_correction$avg_change_so),
                                 age_correction$avg_change_so, 0),
      pred_sb = pred_sb + ifelse(!is.nan(age_correction$avg_change_sb),
                                 age_correction$avg_change_sb, 0),
      pred_ba = pred_h / pred_ab,
      pred_obp = ifelse(!is.nan(pred_h) & !is.nan(pred_bb) &
                          !is.nan(pred_ab) & pred_ab != 0,
                        (pred_h + pred_bb) / (pred_ab + pred_bb),
                        NA_real_),
      pred_slg = ifelse(!is.nan(pred_2b) & !is.nan(pred_3b) & !is.nan(pred_hr) &
                          !is.nan(pred_h) & !is.nan(pred_ab) & pred_ab != 0,
                        (pred_2b * 2 + pred_3b * 3 + pred_hr * 4 +
                           (pred_h -(pred_2b + pred_3b + pred_hr))) / pred_ab,
                        NA_real_)
    )

  # Ensure that predicted games played do not exceed 162
  next_year_stats$pred_g <- ifelse(next_year_stats$pred_g > 162,
                                   162, next_year_stats$pred_g)


  if (nrow(next_year_stats) == 0) {
    return(tibble(message = "No data available for prediction"))
  }

  return(next_year_stats)
}

# Run XGBoost predictions and store in data frame
predict_multiple_stats <- function(player_id, target_vars) {
  predictions <- sapply(target_vars, function(target_var) {
    model_player_stats(player_id, target_var)
  })

  predictions <- t(data.frame(predictions))

  return(predictions)
}

# Use both models to get final player predictions
get_predicted_stats <- function(player_id) {
  cluster_stats <- predict_stats(player_id, 2024)
  cluster_stats <- subset(cluster_stats, select = -c(next_year))

  target_vars <- c("G", "HR", "H", "X2B", "X3B", "AB", "R",
                   "RBI", "BB", "SO", "SB")
  player_predictions <- predict_multiple_stats(player_id, target_vars)

  colnames(player_predictions) <- c("pred_g", "pred_hr", "pred_h",
                                    "pred_2b", "pred_3b", "pred_ab", "pred_r",
                                    "pred_rbi", "pred_bb", "pred_so", "pred_sb")
  player_predictions <- data.frame(player_predictions)
  player_predictions <- player_predictions %>%
    mutate(
      pred_ba = pred_h / pred_ab,
      pred_obp = (pred_h + pred_bb) / (pred_ab + pred_bb),
      pred_slg = (pred_2b * 2 + pred_3b * 3 + pred_hr * 4 +
                    (pred_h - sum(pred_2b, pred_3b, pred_hr))) / pred_ab
    )

  # Calculate the average row
  average_predicted_stats <- colMeans(rbind(cluster_stats, player_predictions))

  # Create a new data frame with the average row
  average_predictions <- data.frame(average_predicted_stats)
  average_predictions <- data.frame(t(average_predictions))
  average_predictions <- average_predictions %>%
    select(pred_g, pred_ba, pred_hr, pred_h, pred_2b, pred_3b,
           pred_ab, pred_r, pred_rbi, pred_bb, pred_so,
           pred_sb, pred_obp, pred_slg)

  return(average_predictions)
}

# Get career stats for a player to display
get_past_stats <- function(player_id) {
  past_player_stats <- player_data %>%
    filter(playerID == player_id)
  past_player_stats <- subset(past_player_stats,
                              select = -c(Cluster, birthYear, nameFirst,
                                          nameLast))
  return(past_player_stats)
}

# Sort player data by name to get ID, used in app
player_name_data <- function(first_name, last_name) {
  summarize_player_data <- player_data %>%
    filter(nameFirst == first_name, nameLast == last_name)

  if (nrow(summarize_player_data) == 0) {
    return(NULL)
  }
  
  summarize_player_data <- summarize_player_data %>%
    group_by(playerID) %>%
    summarize(
      year_range = paste(min(yearID), max(yearID), sep = " - "),
      bats = first(bats),
      throws = first(throws),
      height = first(height),
      weight = first(weight),
      birthYear = first(birthYear)
    ) %>%
    arrange(playerID)

  return(summarize_player_data)
}