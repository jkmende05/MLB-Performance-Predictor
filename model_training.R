library(randomForest)
library(tidyverse)

# Load data
player_data <- readRDS("data//player_cluster_data.rds")
head(player_data)

# Prepare dataset: Use past seasons to predict the next season
training_data <- player_data %>%
  arrange(playerID, yearID) %>%
  group_by(playerID) %>%
  mutate(
    next_BA = lead(BA),
    next_HR = lead(HR),
    next_H = lead(H),
    next_2B = lead(X2B),
    next_3B = lead(X3B),
    next_AB = lead(AB),
    next_R = lead(R),
    next_RBI = lead(RBI),
    next_BB = lead(BB),
    next_SO = lead(SO),
    next_SB = lead(SB),
    next_OBP = lead(OBP),
    next_SLG = lead(SLG)
  ) %>%
  drop_na()

# Select features and targets
features <- training_data %>%
  select(age, AB, Cluster, BA, HR, H, X2B, X3B, R, RBI, BB, SO, SB, OBP, SLG)

targets <- training_data %>%
  select(next_BA, next_HR, next_H, next_2B, next_3B, next_AB, next_R,
         next_RBI, next_BB, next_SO, next_SB, next_OBP, next_SLG)
str(features)

# Train Random Forest models for each target stat
models <- list()
for (target in colnames(targets)) {
  models[[target]] <- randomForest(features, targets[[target]], ntree = 500)
}

# Save trained models
saveRDS(models, "data/player_stat_models.rds")