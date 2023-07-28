library(tidyverse)
library(caret)
library(devtools)
library(GeomMLBStadiums)

setwd("~/Documents/Hyannis")
# Read in Trackman Data
data <- read.csv("cape724.csv")

# Find landing coordinates
coords <- data %>%
  mutate(
    hc_x = sin(Bearing * pi / 180) * Distance,
    hc_y = cos(Bearing * pi / 180) * Distance
  )

# Filter for Flyballs, Line Drives, and Popups
flyballs <- coords %>%
  filter(AutoHitType %in% c("FlyBall", "LineDrive", "Popup")) %>%
  filter(PlayResult != "Undefined", PlayResult != "HomeRun", TaggedHitType != "Bunt") %>%
  mutate(Catch = ifelse(PlayResult == "Out" | PlayResult == "Sacrifice", 1, 0))

fly_data <- flyballs %>%
  select(Catch, ExitSpeed, Angle, HangTime, hc_x, hc_y)


# Plot Catch Likelihood
ggplot(data = flyballs, aes(x = hc_x, y= hc_y, z = Catch)) + 
  stat_summary_hex(bins = 30) + 
  scale_fill_distiller(palette = 'RdBu', direction = -1) + 
  geom_mlb_stadium(stadium_ids = 'generic', stadium_transform_coords = TRUE, stadium_segments = 'all',color = 'black') + 
  coord_fixed() +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  labs(x = NULL, y = NULL,fill = "Catch Prob") 

# Standardize all data for KNN Regression
Processor <- preProcess(fly_data[, -1], method = c('center', 'scale'))
fly_data <- predict(Processor, fly_data)

# Train Test Split
Split <- .7
N <- nrow(fly_data)
TrainingSize <- round(N * Split)
TrainingCases <- sample(N, TrainingSize)
Training <- fly_data[TrainingCases, ]
Test <- fly_data[-TrainingCases, ]
  
# Set the range of k values to test
k_values <- seq(5, 100, by = 5)
  
# Store best model characteristics
min_rmse <- 1
best_model <- NULL
best_k <- NULL

# Loop through different k values and perform KNN regression
for (i in seq_along(k_values)) {
    k <- k_values[i]
    KNNReg <-  knnreg(Catch ~ ., data = Training, k = k)
    Predictions <- predict(KNNReg, Test)
    
    Actuals <- Test$Catch
    Errors <- Actuals - Predictions
    RMSE <- sqrt(mean(Errors^2))
    if (RMSE < min_rmse) {
      best_model <- KNNReg
      min_rmse <- RMSE
      best_k <- k
    }
    print(k)
}

# Save the best model
saveRDS(best_model, file = "knn_model.RData")

# Filter for groundballs now
grounders <- coords %>%
  filter(AutoHitType == c("GroundBall")) %>%
  filter(PlayResult != "Undefined", PlayResult != "HomeRun", PlayResult != "CaughtStealing", PlayResult != "StolenBase",  PlayResult != "Sacrifice", TaggedHitType != "Bunt") %>%
  mutate(PlayMade = ifelse(PlayResult == "Out" | PlayResult == "FieldersChoice", 1, 0))

gb_data <- grounders %>%
  select(PlayMade, ExitSpeed, Angle, hc_x, hc_y)

# Plot probability of play success
ggplot(data = gb_data, aes(x = hc_x, y= hc_y, z = PlayMade)) + 
  stat_summary_hex(bins = 30) + 
  scale_fill_distiller(palette = 'RdBu', direction = -1) + 
  geom_mlb_stadium(stadium_ids = 'generic', stadium_transform_coords = TRUE, stadium_segments = 'all',color = 'black') + 
  coord_fixed() +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  labs(x = NULL, y = NULL,fill = "Catch Prob") 

# Standardize data for KNN Regression
Processor <- preProcess(gb_data[, -1], method = c('center', 'scale'))
gb_data <- predict(Processor, gb_data)

# Train Test Split
Split <- .7
N <- nrow(gb_data)
TrainingSize <- round(N * Split)
TrainingCases <- sample(N, TrainingSize)
Training <- gb_data[TrainingCases, ]
Test <- gb_data[-TrainingCases, ]

# Set the range of k values you want to explore
k_values <- seq(5, 100, by = 5)

# Store best model characteristics
min_rmse <- 1
best_model <- NULL
best_k <- NULL

# Loop through different k values and perform KNN regression
for (i in seq_along(k_values)) {
  k <- k_values[i]
  KNNReg <- knnreg(PlayMade ~ ., data = Training, k = k)
  Predictions <- predict(KNNReg, Test)
  
  Actuals <- Test$PlayMade
  Errors <- Actuals - Predictions
  RMSE <- sqrt(mean(Errors^2))
  if (RMSE < min_rmse) {
    best_model <- KNNReg
    min_rmse <- RMSE
    best_k <- k
  }
  print(k)
}

# Save the best model
saveRDS(best_model, file = "knn_gb.RData")

