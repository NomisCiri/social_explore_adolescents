library(shiny)
library(tidyverse)

explore_data <- readRDS("../select_participants_app/explore_data.RDS")

source("../select_participants_app/clean_data.R")


players <- unique(late_gems$player)

# Define UI ----
ui <- fluidPage(
  
  titlePanel("visualize participants decisions"),
  
  sidebarLayout(
    
    sidebarPanel(  
      
    helpText("values to find a player"),
    
    selectInput("player_number", label = "choose a player", choices = players),
  
    selectInput("performance", label = "what level of performance", choices = c('low', 'average', "high")),

    sliderInput("gem_when", label = "Gem found before round: ",  min = 1, max = 25, value = 10))
    
    ,

    mainPanel(plotlyOutput("plot")
      
      
    )
  )
)
  

# Define server logic ----
server <- function(input, output) {
  
  
  output$plot <- renderPlotly({ 
    
    one_sequence <- late_gems %>%
      filter(player == input$player_number & round == input$gem_when) %>%
      ggplot() +
      #geom_bin_2d(aes(x = x, y = y, fill = gem), binwidth = c(1, 1)) +
      geom_rect(
        data = gems_coords,
        size = 1,
        colour = "red",
        fill = "white",
        
        aes(
          xmin = x - 1,
          xmax = x  ,
          ymin = y - 1,
          ymax = y
        )
      ) +
      geom_rect(aes(
        xmin = x - 1,
        xmax = x  ,
        ymin = y - 1,
        ymax = y,
        fill = points,
        frame = trial
      ),
      color = "black",
      size = .2) +
      
      scale_x_continuous(limits = c(-1, 7), breaks = -1:7) +
      scale_y_continuous(limits = c(-1, 7), breaks = -1:7) +
      scale_fill_continuous_divergingx(palette = "RdBu", mid = 0)
    
    ggplotly(one_sequence) %>% 
      animation_opts( transition = 0)
  })
    }

# Run the app ----
shinyApp(ui = ui, server = server)

