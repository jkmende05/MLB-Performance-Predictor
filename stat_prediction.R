library(xgboost)
library(dplyr)
library(tidyr)
library(zoo)

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

  player_data_clean <- player_data_clean %>%
    group_by(nameFirst, nameLast) %>%
    mutate(
      avg_HR = zoo::rollmean(HR, 3, fill = NA, align = 'right'), # 3-year rolling average of HR
      avg_AB = zoo::rollmean(AB, 3, fill = NA, align = 'right')  # 3-year rolling average of AB
    )

  # Prepare the features (selecting relevant columns)
  features <- player_data_clean %>%
    select(G, BA, HR, H, X2B, X3B, AB, R, RBI, BB, SO, SB, OBP, SLG, age, height, weight, Cluster, nameFirst, nameLast) %>%
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
    eval_metric = "rmse",
    lambda = 1,  # L2 regularization
    alpha = 0.5
  )

  # Train the model
  model_ba <- xgboost(
    params = params,
    data = dtrain,
    nrounds = 200,
    verbose = 1
  )

  # Prepare the features for prediction (use the same preprocessing steps as before)
  predict_features <- player_data_clean %>%
    filter(nameFirst == first_name, nameLast == last_name) %>%
    select(G, BA, HR, H, X2B, X3B, AB, R, RBI, BB, SO, SB, OBP, SLG, age, height, weight, Cluster, nameFirst, nameLast) %>%
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

pred_ba <- model_player_stats_two("Max", "Kepler", "AVG")
pred_ba

predict_stats <- function(first_name, last_name, current_year) {
  player_cluster <- player_data %>%
    filter(nameFirst == first_name, nameLast == last_name) %>%
    select(Cluster)

  if (nrow(player_cluster) == 0) {
    return(tibble(message = "Player not found"))
  }

  cluster_value <- tail(player_cluster$Cluster, n = 1)

  similar_players <- player_data %>%
    filter(Cluster == cluster_value, yearID < current_year)

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
      pred_bb = weighted.mean(BB, w = exp(yearID - min(yearID)) / (1 + BB), na.rm = TRUE),
      pred_so = weighted.mean(SO, w = exp(yearID - min(yearID)), na.rm = TRUE),
      pred_sb = weighted.mean(SB, w = exp(yearID - min(yearID)), na.rm = TRUE)
    ) %>%
    filter(next_year == current_year)

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
    filter(nameFirst == first_name, nameLast == last_name) %>%
    pull(age)

  age_correction <- age_adjustment %>%
    filter(age == tail(player_age, n = 1) + 1)

  next_year_stats <- next_year_stats %>%
    mutate(
      pred_g = pred_g + ifelse(!is.nan(age_correction$avg_change_g), age_correction$avg_change_g, 0),
      pred_ba = pred_h / pred_ab,
      pred_hr = pred_hr + ifelse(!is.nan(age_correction$avg_change_hr), age_correction$avg_change_hr, 0),
      pred_h = pred_h + ifelse(!is.nan(age_correction$avg_change_h), age_correction$avg_change_h, 0),
      pred_2b = pred_2b + ifelse(!is.nan(age_correction$avg_change_2b), age_correction$avg_change_2b, 0),
      pred_3b = pred_3b + ifelse(!is.nan(age_correction$avg_change_3b), age_correction$avg_change_3b, 0),
      pred_ab = pred_ab + ifelse(!is.nan(age_correction$avg_change_ab), age_correction$avg_change_ab, 0),
      pred_r = pred_r + ifelse(!is.nan(age_correction$avg_change_r), age_correction$avg_change_r, 0),
      pred_rbi = pred_rbi + ifelse(!is.nan(age_correction$avg_change_rbi), age_correction$avg_change_rbi, 0),
      pred_bb = pred_bb + ifelse(!is.nan(age_correction$avg_change_bb), age_correction$avg_change_bb, 0),
      pred_so = pred_so + ifelse(!is.nan(age_correction$avg_change_so), age_correction$avg_change_so, 0),
      pred_sb = pred_sb + ifelse(!is.nan(age_correction$avg_change_sb), age_correction$avg_change_sb, 0),
      pred_obp = ifelse(!is.nan(pred_h) & !is.nan(pred_bb) & !is.nan(pred_ab) & pred_ab != 0, 
                        (pred_h + pred_bb) / (pred_ab + pred_bb),
                        NA_real_),
      pred_slg = ifelse(!is.nan(pred_2b) & !is.nan(pred_3b) & !is.nan(pred_hr) & !is.nan(pred_h) & !is.nan(pred_ab) & pred_ab != 0, 
                        (pred_2b * 2 + pred_3b * 3 + pred_hr * 4 + (pred_h - (pred_2b + pred_3b + pred_hr))) / pred_ab, 
                        NA_real_)
    )

  # Ensure that predicted games played do not exceed 162
  next_year_stats$pred_g <- ifelse(next_year_stats$pred_g > 162, 162, next_year_stats$pred_g)


  if (nrow(next_year_stats) == 0) {
    return(tibble(message = "No data available for prediction"))
  }

  return(next_year_stats)
}

test <- predict_stats("Ronald", "Acuna", 2024)
View(test)

test_df <- player_data %>%
  filter(nameFirst == "Salvador", nameLast == "Perez")
test_df
View(test_df)
