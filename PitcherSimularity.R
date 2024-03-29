library(tidyverse)
# Read in and calculate pitcher averages
trackman <- read_csv("CLT_TrackmanGames_2022.csv")
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

# Define a function to calculate the minmax norm
min_max_norm <- function(x){(x-min(x))/(max(x)-min(x))}
# Normalize the numeric columns with the min max norm fucntion
trackman_grouped[, 6:11] <- lapply(trackman_grouped[, 6:11], min_max_norm)

# Define function to calculate the euclidean distance between two values
euclidean <- function(a, b) sqrt(sum((a - b)^2))
# Define a function for similarity using the coaches frame work for 0-4
similarity <- function(a, b) {
  similarity <- 4 / (1 + euclidean(a, b))
  return(round(similarity, 3))
}

# Define function to calculate similarity between pitchers for a given pitch type
get_sim <- function(PlayerName, PitchType) {
  
  Pitchers <- unique(trackman_grouped$Pitcher) # Get a vector of the pitcher names, If you only want to compare certain types of players filter here(ie. PitcherTeam == "CHA_FOR")
  compare_pitchers_pitch = tibble(Pitcher = Pitchers, Similarity = NA) # Create a table with the pitchers vector and a column for Similarity
  trackman_grouped_filtered <- trackman_grouped %>% filter(TaggedPitchType == PitchType) # Filter table by PitchType passed in function
  # If the player passed does not have the pitch passed make the similarity score of those who do have the pitch equal to 2 and those who also dont equal to 0
  if(!(PlayerName %in% trackman_grouped_filtered$Pitcher)){
    for(pitcher_index in 1:length(Pitchers)){
      if(Pitchers[pitcher_index] %in% unique(trackman_grouped_filtered$Pitcher)){
        similarity_score  <- 4
      } else{
        similarity_score  <- 0
      }
      compare_pitchers_pitch[pitcher_index, "Similarity"] <- similarity_score 
    }
    return(compare_pitchers_pitch)
  }
  
  player_index <- which((trackman_grouped_filtered$Pitcher == PlayerName)) # Get the index of the player passed to compare later
  # Iterate through each pitcher in pitchers list
  for(pitcher_index in 1:length(Pitchers)){
    # If the pitcher is in the data frame for pitchers with the passed pitch type calculate euclidean distance, else give similarity score of 2
    if(Pitchers[pitcher_index] %in% unique(trackman_grouped_filtered$Pitcher)){
      temp_pitcher_index <- which((trackman_grouped_filtered$Pitcher == Pitchers[pitcher_index]))
      similarity_score  <- euclidean(trackman_grouped_filtered[player_index,6:11], trackman_grouped_filtered[temp_pitcher_index, 6:11])
    } else{
      similarity_score  <- 4
    }
    compare_pitchers_pitch[pitcher_index, "Similarity"] <- similarity_score 
  }
  return(compare_pitchers_pitch)
}



# make table for all pitches in arsenal and compare based on avg simscore or euclidean between the players
arsenal_func <- function(PitcherName) {
  pitch_columns <- c("Fastball","Slider","Curveball","ChangeUp","Cutter","Knuckleball","Splitter","Sinker")
  arsenal <- tibble(Pitcher = unique(trackman_grouped$Pitcher))
  arsenal[pitch_columns] <- NA
  for(pitch_index in 1:length(pitch_columns)){
    temp_pitch_tbl <- get_sim(PitcherName, pitch_columns[pitch_index])
    arsenal[,pitch_columns[pitch_index]] <- temp_pitch_tbl$Similarity
  }
  arsenal <- arsenal %>% mutate(mean = round(rowSums(arsenal[, 2:9])/8, 2))
  return(arsenal)
}

arsenal_func("Brooks, Matt") %>% view()

