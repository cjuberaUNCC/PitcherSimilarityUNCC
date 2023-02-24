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
  Pitchers <- unique(trackman_grouped$Pitcher)
  compare_pitchers_pitch = tibble(Pitcher = Pitchers, Similarity = NA)
  trackman_grouped_filtered <- trackman_grouped %>% filter(TaggedPitchType == PitchType)
  if(!(PlayerName %in% trackman_grouped_filtered$Pitcher)){
    compare_pitchers_pitch[, "Similarity"] <- NA
    return(compare_pitchers_pitch)
  }
  player_index <- which((trackman_grouped_filtered$Pitcher == PlayerName))
  # For right now compare to all, later compare to only charlotte players
  for(pitcher_index in 1:length(Pitchers)){
    if(Pitchers[pitcher_index] %in% unique(trackman_grouped_filtered$Pitcher)){
      temp_pitcher_index <- which((trackman_grouped_filtered$Pitcher == Pitchers[pitcher_index]))
      temp <- euclidean(trackman_grouped_filtered[player_index,6:11], trackman_grouped_filtered[temp_pitcher_index, 6:11])
    } else{
      temp <- 4
    }
    compare_pitchers_pitch[pitcher_index, "Similarity"] <- temp
  }
  return(compare_pitchers_pitch)
}
get_sim("Schafer, Luke", "Slider") %>% view()

# Compare pitchers using KNN

# Compare arsenal

# make table for all pitches in arsenal and compare based on avg simscore or euclidean between the players
pitch_columns <- c("Fastball","Slider","Curveball","ChangeUp","Cutter","Knuckleball","Splitter","Sinker")
arsenal <- tibble(Pitcher = unique(trackman_grouped$Pitcher))
arsenal[pitch_columns] <- NA
for(pitch_index in 1:length(pitch_columns)){
  temp_pitch_tbl <- get_sim("Schafer, Luke", pitch_columns[pitch_index])
  arsenal[,pitch_columns[pitch_index]] <- temp_pitch_tbl$Similarity
}
get_sim("Schafer, Luke", "ChangeUp")
