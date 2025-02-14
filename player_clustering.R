library(cluster)

player_data <- readRDS("data//player_data.rds")
head(player_data)

cluster_features <- player_data %>%
  select(height, weight, age, BA, HR, H, X2B, X3B, AB, R, RBI, BB, SO, SB,
         OBP, SLG, bats, throws)

str(cluster_features)
cluster_features <- cluster_features %>%
  mutate(across(where(is.character), as.numeric)) %>%
  mutate(across(where(is.factor), as.numeric))
scaled_data <- scale(cluster_features)

set.seed(42)
kmeans_model <- kmeans(scaled_data, centers = 400)

player_data$Cluster <- as.factor(kmeans_model$cluster)

saveRDS(player_data, "data//player_cluster_data.rds")
player_data
