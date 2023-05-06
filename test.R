library(shiny)
library(tidyverse)

ui <- fluidPage(
  fileInput("file", "Choose CSV File", accept = ".csv"),
  tableOutput("table")
)

server <- function(input, output) {
  
  mydata <- reactive({
    file <- input$file
    if(is.null(file)) {
      return(NULL)
    }
    read_csv(file$datapath)
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
    
    
    
  })
  
  
  
  output$table <- renderTable({
    fixed_data() %>% head()
  })
}

shinyApp(ui, server)
