# Cape_Defense_Metrics
Still work in progress, next steps include trying other model types to increase accuracy, and merging with play-by-play data to calculate Outs Above Average for specific players

Uses K-Nearest-Neighbors Regression to calculate probability of an out being made based on batted ball data from a Trackman file.

First, run KNNCatch.R to train KNN models on a chosen dataset
Then, run CatchProb.R to apply the KNN models to new Trackman data
