library(tidyverse)
library(tidymodels)
library(kknn)
library(modelr)

trackman <- read_csv("TrackmanGames_2022_Ball_CLT.csv")

# Pitcher, PitcherThrows, CHA_FOR, TaggedPitchType, RelSpeed, VertBreak, HorzaBreak, RelHeight, RelSide, Extention

trackman_grouped <- trackman %>% select(c(PitcherId, PitcherThrows, PitcherTeam, TaggedPitchType, RelSpeed, VertBreak, HorzBreak, RelHeight, RelSide, Extension)) %>%
  group_by(PitcherId, TaggedPitchType) %>% 
  summarise(
    PitcherTeam = mode(PitcherTeam),
    PitcherThrows = mode(PitcherThrows),
    PitcherTeam = mean(RelSpeed),
    PitcherTeam = mean(VertBreak),
    PitcherTeam = mean(HorzBreak),
    PitcherTeam = mean(RelHeight),
    PitcherTeam = mean(RelSide),
    PitcherTeam = mean(Extension)
  )



knn_1 <- nearest_neighbor(
  mode = 'regression',
  engine = "kknn",
  neighbors = 5
) %>% fit(PitcherId ~ RelSpeed + VertBreak + HorzBreak + RelHeight + RelSide + Extension,
          data = trackman_pre)

mckree <- trackman %>% filter(PitcherId == 1000121528 & TaggedPitchType == "Slider")

mckree$pred <- predict(knn_1, new_data = mckree)

mckree <- mckree %>% select(c(PitcherId, PitcherThrows, PitcherTeam, TaggedPitchType, RelSpeed, VertBreak, HorzBreak, RelHeight, RelSide, Extension, pred))
  
  
  