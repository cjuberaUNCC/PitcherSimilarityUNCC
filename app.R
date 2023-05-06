library(shiny)
library(tidyverse)
options(shiny.maxRequestSize=30*1024^2)
ui <- fluidPage(
  titlePanel("Pitcher Similarity Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File",
                accept=c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
      selectInput("name", "Select a Pitcher", ""),
      selectInput("type", "Select Analysis Type",
                  choices = c("Pitch", "Arsenal")),
      conditionalPanel(
        condition = "input.type == 'Pitch'",
        selectInput("pitch_type", "Select Pitch Type", choices = c("Fastball","Slider","Curveball","ChangeUp","Cutter","Knuckleball","Splitter","Sinker"))
      ),
      actionButton("submit", "Submit"),
      
    ),
    mainPanel(
      tableOutput("similarity_table")
    )
  )
)

min_max_norm <- function(x){(x-min(x))/(max(x)-min(x))}
# Define function to calculate the euclidean distance between two values
euclidean <- function(a, b) sqrt(sum((a - b)^2))
# Define a function for similarity using the coaches frame work for 0-4
similarity <- function(a, b) {
  similarity <- 4 / (1 + euclidean(a, b))
  return(round(similarity, 3))
}
get_sim <- function(data, PlayerName, PitchType) {
  Pitchers <- unique(data$Pitcher) # Get a vector of the pitcher names, If you only want to compare certain types of players filter here(ie. PitcherTeam == "CHA_FOR")
  compare_pitchers_pitch = tibble(Pitcher = Pitchers, Similarity = NA) # Create a table with the pitchers vector and a column for Similarity
  data_filtered <- data %>% filter(TaggedPitchType == PitchType) # Filter table by PitchType passed in function
  # If the player passed does not have the pitch passed make the similarity score of those who do have the pitch equal to 2 and those who also dont equal to 0
  if(!(PlayerName %in% data_filtered$Pitcher)){
    for(pitcher_index in 1:length(Pitchers)){
      if(Pitchers[pitcher_index] %in% unique(data_filtered$Pitcher)){
        similarity_score  <- 4
      } else{
        similarity_score  <- 0
      }
      compare_pitchers_pitch[pitcher_index, "Similarity"] <- similarity_score 
    }
    compare_pitchers_pitch <- compare_pitchers_pitch %>% merge(data[c("Pitcher", "PitcherTeam")], by = "Pitcher")
    compare_pitchers_pitch <- compare_pitchers_pitch[!duplicated(compare_pitchers_pitch$Pitcher), ]
    return(compare_pitchers_pitch[c(1, 3, 2)])
  }
  
  player_index <- which((data_filtered$Pitcher == PlayerName)) # Get the index of the player passed to compare later
  # Iterate through each pitcher in pitchers list
  for(pitcher_index in 1:length(Pitchers)){
    # If the pitcher is in the data frame for pitchers with the passed pitch type calculate euclidean distance, else give similarity score of 2
    if(Pitchers[pitcher_index] %in% unique(data_filtered$Pitcher)){
      temp_pitcher_index <- which((data_filtered$Pitcher == Pitchers[pitcher_index]))
      similarity_score  <- euclidean(data_filtered[player_index,6:11], data_filtered[temp_pitcher_index, 6:11])
    } else{
      similarity_score  <- 4
    }
    compare_pitchers_pitch[pitcher_index, "Similarity"] <- similarity_score 
  }
  compare_pitchers_pitch <- compare_pitchers_pitch %>% merge(data[c("Pitcher", "PitcherTeam")], by = "Pitcher")
  compare_pitchers_pitch <- compare_pitchers_pitch[!duplicated(compare_pitchers_pitch$Pitcher), ]
  return(compare_pitchers_pitch[c(1, 3, 2)])
}
# make table for all pitches in arsenal and compare based on avg simscore or euclidean between the players
arsenal_func <- function(data, PitcherName) {
  pitch_columns <- c("Fastball","Slider","Curveball","ChangeUp","Cutter","Knuckleball","Splitter","Sinker") 
  PitcherNames <- unique(data$Pitcher) 
  PitcherNames <- PitcherNames[order(names(setNames(PitcherNames, PitcherNames)))]
  arsenal <- tibble(Pitcher = PitcherNames)
  arsenal[pitch_columns] <- NA
  for(pitch_index in 1:length(pitch_columns)){
    temp_pitch_tbl <- get_sim(data, PitcherName, pitch_columns[pitch_index]) %>% arrange(Pitcher)
    arsenal[,pitch_columns[pitch_index]] <- temp_pitch_tbl$Similarity
  }
  arsenal <- arsenal %>% mutate(Similarity = round(rowSums(arsenal[, 2:9]), 2)) %>% 
    merge(data[c("Pitcher", "PitcherTeam")], by = "Pitcher")
  arsenal <- arsenal[!duplicated(arsenal$Pitcher), ]
  
  return(arsenal[c(1, 11, 2:10)])
}

server <- function(input, output, session) {
  mydata <- reactive({
    file <- input$file
    if(is.null(file)) {
      return(NULL)
    }
    read_csv(file$datapath)
  })
  
  observe({
    updateSelectInput(session, "name", choices = unique(mydata()$Pitcher))
  })
  
  
  fixed_data <- reactive({
    data <- mydata()
    
    data_grouped <- data %>%
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
    data_grouped[, 6:11] <- lapply(data_grouped[, 6:11], min_max_norm)
    if(input$type == 'Pitch'){
      get_sim(data_grouped, input$name, input$pitch_type)
    } else if(input$type == 'Arsenal'){
      arsenal_func(data_grouped, input$name)
    }
  })
  observeEvent(input$submit, {
    output$similarity_table <- renderTable(fixed_data() %>% arrange(Similarity))
  })
  
}
shinyApp(ui, server)