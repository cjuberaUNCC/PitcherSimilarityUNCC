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
    RelSpeed = mean(RelSpeed),
    NumPitches = n()
  ) %>%
  drop_na()
# Normalize numeric values
min_max_norm <- function(x){(x-min(x))/(max(x)-min(x))}

trackman_grouped[, 6:11] <- lapply(trackman_grouped[, 6:11], min_max_norm)
# Compare pitchers using euclidean distance
euclidean <- function(a, b) sqrt(sum((a - b)^2))

# fix
get_sim <- function(PlayerName, PitchType) {
  list_sim_scores = c()
  trackman_grouped_filtered <- trackman_grouped %>% filter(TaggedPitchType == PitchType)
  player_index <- which((trackman_grouped_filtered$Pitcher == PlayerName))
  # For right now compare to all, later compare to only charlotte players
  for(compared_player_index in 1:nrow(trackman_grouped_filtered)){
    list_sim_scores <- append(list_sim_scores, euclidean(trackman_grouped_filtered[player_index,6:11], trackman_grouped_filtered[compared_player_index, 6:11]))
  }
  trackman_grouped_filtered$SimScore <- list_sim_scores
  return(trackman_grouped_filtered)
}

view(get_sim("Schafer, Luke", "Fastball"))

# Compare pitchers using KNN

# Compare arsenal

# make table for all pitches in arsenal and compare based on avg simscore or euclidean between the players
pitch_columns <- c("Fastball","Slider","Curveball","ChangeUp","Cutter","Knuckleball","Splitter","Sinker")

arsenal <- tibble()
arsenal["PitcherName"] <- NA
arsenal[pitch_columns] <- NA
for(pitch in pitch_columns){
  temp_pitch_tbl <- get_sim("Schafer, Luke", pitch)
  for(index in 1:nrow(temp_pitch_tbl)){
    arsenal[index, "PitcherName"] <- temp_pitch_tbl[index,]$Pitcher
    arsenal[index, pitch] <- temp_pitch_tbl[index,]$SimScore
  }
    
}
get_sim("Schafer, Luke", "Slider")
