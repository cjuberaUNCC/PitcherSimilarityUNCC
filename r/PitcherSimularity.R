library(tidyverse)
library(tidymodels)
library(kknn)
library(modelr)

# Read in data
trackman <- read_csv("TrackmanGames_2022_Ball_CLT.csv")
# Get Pitcher averages for each pitch type in their arsenal 
trackman_grouped <- trackman %>%
  group_by(PitcherId, TaggedPitchType, Pitcher, PitcherTeam, PitcherThrows) %>% 
  mutate(PitcherName = Pitcher) %>% 
  summarise(
    VertBreak = mean(VertBreak),
    HorzBreak = mean(HorzBreak),
    RelHeight = mean(RelHeight),
    RelSide = mean(RelSide),
    Extension = mean(Extension),
    RelSpeed = mean(RelSpeed)
  ) %>%
  drop_na()
# Normalize numeric values
min_max_norm <- function(x){(x-min(x))/(max(x)-min(x))}

trackman_grouped_normalized <- trackman_grouped
trackman_grouped_normalized[, 6:11] <- lapply(trackman_grouped_normalized[, 6:11], min_max_norm)

# Compare pitchers using euclidean distance
euclidean <- function(a, b) sqrt(sum((a - b)^2))

# fix
get_sim <- function(PlayerName, PitchType) {
  list_sim_scores = c()
  list_pitcher_names = c()
  trackman_grouped_filtered <- trackman_grouped_normalized %>% filter(TaggedPitchType == PitchType)
  player_index <- which((trackman_grouped_filtered$Pitcher == PlayerName))
  player_row <- trackman_grouped_filtered[player_index, ]
  players_to_compare_to <- trackman_grouped_filtered[-player_index,]
  # For right now compare to all, later compare to only charlotte players
  for(compared_player_index in 1:length(players_to_compare_to)){
    list_pitcher_names <- append(list_pitcher_names, players_to_compare_to[compared_player_index, 'Pitcher'])
    list_sim_scores <- append(list_sim_scores, euclidean(player_row[,6:11], players_to_compare_to[compared_player_index, 6:11]))
  }
  matrix_test = tibble(names = list_pitcher_names, scores = list_sim_scores)
  
  return(matrix_test)
  
}

matrix_test = tibble(names = list_pitcher_names, scores = list_sim_scores)
# Compare pitchers using KNN

  
  