library(xgboost)
library(dplyr)
library(tidyr)

player_data <- readRDS("data//player_cluster_data.rds")

head(player_data)

model_player_stats_two <- function(first_name, last_name, target_var) {
  player_data <- player_data %>%
    mutate(
      Cluster = as.factor(Cluster),
      nameFirst = as.factor(nameFirst),
      nameLast = as.factor(nameLast),
      bats = as.factor(bats),
      throws = as.factor(throws)
    )

  player_data_clean <- player_data %>%
    filter(!is.na(.[[target_var]]))

  # Prepare the features (selecting relevant columns)
  features <- player_data_clean %>%
    select(BA, HR, H, X2B, X3B, AB, R, RBI, BB, SO, SB, OBP, SLG, age, height, weight, Cluster, nameFirst, nameLast) %>%
    mutate(
      Cluster = as.numeric(Cluster), # Convert the factor to numeric
      nameFirst = as.numeric(nameFirst), # Encode 'nameFirst' to numeric
      nameLast = as.numeric(nameLast)   # Encode 'nameLast' to numeric
    )

  target <- player_data_clean[[target_var]]
  features <- features %>%
    select(-all_of(target_var))

  # Convert the data to a matrix for XGBoost
  dtrain <- xgb.DMatrix(data = as.matrix(features), label = target)

  # Set the XGBoost parameters
  params <- list(
    objective = "reg:squarederror", # Regression task (continuous output)
    max_depth = 6,
    eta = 0.1,
    nthread = 2,
    eval_metric = "rmse"
  )

  # Train the model
  model_ba <- xgboost(
    params = params,
    data = dtrain,
    nrounds = 200
  )

  # Prepare the features for prediction (use the same preprocessing steps as before)
  predict_features <- player_data_clean %>%
    filter(nameFirst == first_name, nameLast == last_name) %>%
    select(BA, HR, H, X2B, X3B, AB, R, RBI, BB, SO, SB, OBP, SLG, age, height, weight, Cluster, nameFirst, nameLast) %>%
    mutate(
      Cluster = as.numeric(Cluster),
      nameFirst = as.numeric(nameFirst),
      nameLast = as.numeric(nameLast)
    ) %>%
    select(-all_of(target_var))
  
  # Convert the data to a matrix
  predict_matrix <- as.matrix(predict_features)
  predict_matrix
  # Predict the target variable (BA in this case)
  pred_ba <- predict(model_ba, predict_matrix)
  return(pred_ba[1])
}

pred_ba <- model_player_stats_two("George", "Springer", "AB")
pred_ba

predict_stats <- function(first_name, last_name, current_year) {
  player_cluster <- player_data %>%
    filter(nameFirst == first_name, nameLast == last_name) %>%
    select(Cluster)

  if (nrow(player_cluster) == 0) {
    return(tibble(message = "Player not found"))
  }

  similar_players <- player_data %>%
    filter(Cluster == player_cluster$Cluster, yearID < current_year)

  next_year_stats <- similar_players %>%
    group_by(next_year = yearID + 1) %>%
    summarize(
      pred_ba = weighted.mean(BA, w = exp(yearID - min(yearID)), na.rm = TRUE),
      pred_hr = weighted.mean(HR, w = exp(yearID - min(yearID)), na.rm = TRUE),
      pred_h = weighted.mean(H, w = exp(yearID - min(yearID)), na.rm = TRUE),
      pred_2b = weighted.mean(X2B, w = exp(yearID - min(yearID)), na.rm = TRUE),
      pred_3b = weighted.mean(X3B, w = exp(yearID - min(yearID)), na.rm = TRUE),
      pred_ab = weighted.mean(AB, w = exp(yearID - min(yearID)), na.rm = TRUE),
      pred_r = weighted.mean(R, w = exp(yearID - min(yearID)), na.rm = TRUE),
      pred_rbi = weighted.mean(RBI, w = exp(yearID - min(yearID)),
                               na.rm = TRUE),
      pred_bb = weighted.mean(BB, w = exp(yearID - min(yearID)),
                              na.rm = TRUE),
      pred_so = weighted.mean(SO, w = exp(yearID - min(yearID)), na.rm = TRUE),
      pred_sb = weighted.mean(SB, w = exp(yearID - min(yearID)), na.rm = TRUE),
      pred_obp = weighted.mean(OBP, w = exp(yearID - min(yearID)),
                               na.rm = TRUE),
      pred_slg = weighted.mean(SLG, w = exp(yearID - min(yearID)), na.rm = TRUE)
    ) %>%
    filter(next_year == current_year)

  age_adjustment <- similar_players %>%
    group_by(age) %>%
    summarize(
      avg_change_ba = weighted.mean(BA - lag(BA), w = AB, na.rm = TRUE),
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
      avg_change_obp = weighted.mean(OBP - lag(OBP), w = AB, na.rm = TRUE),
      avg_change_slg = weighted.mean(SLG - lag(SLG), w = AB, na.rm = TRUE)
    )

  player_age <- player_data %>%
    filter(nameFirst == first_name, nameLast == last_name) %>%
    pull(age)

  age_correction <- age_adjustment %>%
    filter(age == tail(player_age, n = 1) + 1)

  next_year_stats <- next_year_stats %>%
    mutate(
      pred_ba = pred_ba + age_correction$avg_change_ba,
      pred_hr = pred_hr + age_correction$avg_change_hr,
      pred_h = pred_h + age_correction$avg_change_h,
      pred_2b = pred_2b + age_correction$avg_change_2b,
      pred_3b = pred_3b + age_correction$avg_change_3b,
      pred_ab = pred_ab + age_correction$avg_change_ab,
      pred_r = pred_r + age_correction$avg_change_r,
      pred_rbi = pred_rbi + age_correction$avg_change_rbi,
      pred_bb = pred_bb + age_correction$avg_change_bb,
      pred_so = pred_so + age_correction$avg_change_so,
      pred_sb = pred_sb + age_correction$avg_change_sb,
      pred_obp = pred_obp + age_correction$avg_change_obp,
      pred_slg = pred_slg + age_correction$avg_change_slg,
    )

  if (nrow(next_year_stats) == 0) {
    return(tibble(message = "No data available for prediction"))
  }

  return(next_year_stats)
}

test <- predict_stats("George", "Springer", 2024)
View(test)

test_df <- player_data %>%
  filter(nameFirst == "Vladimir", nameLast == "Guerrero")
test_df
View(test_df)
