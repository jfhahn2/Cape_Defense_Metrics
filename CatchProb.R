library(tidyverse)
library(caret)
setwd("~/Documents/Hyannis")

# Read in fly ball model
knn_model <- readRDS("knn_model.RData")

# Read in Trackman Data
data <- read_csv("cape724.csv")

# Calculate Landing Coordinates
coords <- data %>%
  mutate(
    hc_x = sin(Bearing * pi / 180) * Distance,
    hc_y = cos(Bearing * pi / 180) * Distance
  )

# Filter for flyballs, line drives, and popups
flyballs <- coords %>%
  filter(AutoHitType %in% c("FlyBall", "LineDrive", "Popup")) %>%
  filter(PlayResult != "Undefined", PlayResult != "HomeRun", TaggedHitType != "Bunt") %>%
  mutate(Catch = ifelse(PlayResult == "Out" | PlayResult == "Sacrifice", 1, 0))

fly_data <- flyballs %>%
  select(Catch, ExitSpeed, Angle, HangTime, hc_x, hc_y, Date, Batter, Pitcher, PitcherTeam, Inning, PlayResult)

# Standardize data for KNN Regression
Processor <- preProcess(fly_data[, -c(1,11)], method = c('center', 'scale'))

# Predict probability of an out
fly_data <- predict(Processor, fly_data)
fly_data$Predictions <- predict(knn_model, fly_data)
Predictions <- fly_data$Predictions 
Actuals <- fly_data$Catch
Errors <- Actuals - Predictions
RMSE <- sqrt(mean(Errors^2))

# Find Outs Above Average on Flyballs by Team
fly_data %>% mutate(OAA = Catch - Predictions) %>% group_by(PitcherTeam) %>% summarize(OAA = sum(OAA)) %>% arrange(-OAA)


# This section calculates catch probability for a "newly" observed flyball
new_observation <- data.frame(
  Catch = 1,
  ExitSpeed = 70,
  Angle = 35,
  Bearing = 0,
  HangTime = 4.1,
  hc_x = 0,
  hc_y = 241.5,
  Date = "1",
  Batter = "1",
  Pitcher = "1",
  Inning = 1,
  PlayResult = "Out"
  # Add other variables as required
)

# Center and scale the new observation using the preprocessor from the KNN model
# Assuming the preprocessor is saved as "preprocessor" in the KNN model
new_observation_scaled <- predict(Processor, new_observation)

# Use the KNN model to make predictions on the scaled new observation
predict(knn_model, new_observation_scaled, type = "class")


# Read in ground ball model
knn_gb <- readRDS("knn_gb.RData")

# Filter for groundballs
grounders <- coords %>%
  filter(AutoHitType == c("GroundBall")) %>%
  filter(PlayResult != "Undefined", PlayResult != "HomeRun", PlayResult != "CaughtStealing", PlayResult != "StolenBase",  PlayResult != "Sacrifice", TaggedHitType != "Bunt") %>%
  mutate(PlayMade = ifelse(PlayResult == "Out" | PlayResult == "FieldersChoice", 1, 0))

gb_data <- grounders %>%
  select(PlayMade, ExitSpeed, Angle, hc_x, hc_y, Date, Batter, Pitcher, PitcherTeam, Inning, PlayResult)

# Standardize data for KNN Regression
Processor <- preProcess(gb_data[, -c(1,10)], method = c('center', 'scale'))

# Predict probability of an out
gb_data <- predict(Processor, gb_data)
gb_data$Predictions <- predict(knn_gb, gb_data)
Predictions <- gb_data$Predictions 
Actuals <- gb_data$PlayMade
Errors <- Actuals - Predictions
RMSE <- sqrt(mean(Errors^2))

# Find Outs Above Average on Groundballs by Team
gb_data %>% mutate(OAA = PlayMade - Predictions) %>% group_by(PitcherTeam) %>% summarize(OAA = sum(OAA)) %>% arrange(-OAA)

