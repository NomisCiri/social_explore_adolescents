library(shiny)
library(tidyverse)
library(dplyr)
library(here)

explore_data <-
  readRDS(paste0(here(),"/select_participants_app/explore_data.RDS"))

explore_data <- explore_data %>%
  filter(player != 188 & player > 50) %>%
  group_by(player, round) %>%
  mutate(trial = 1:25,
         unique_rounds = cur_group_id()) %>%
  mutate(round_id = as.numeric(unique_rounds))


source("../select_participants_app/clean_data.R")


players <- unique(late_gems$player)

# Define UI ----
ui <- fluidPage(
  titlePanel("visualize participants decisions"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("values to find a player"),
      
      selectInput("env_number", label = "choose an environment", c(1:6)),
      
      selectInput("performance", label = "what level of performance (1 = low, 3 = high)", choices = c(1:3)),
      
      sliderInput(
        "gem_when",
        label = "Gem found before round: ",
        min = 1,
        max = 25,
        value = 10
      ),

        uiOutput("unique_round")
    )
    ,
      mainPanel(plotOutput("plot"),
              textOutput("min_max"))
  )
)


# Define server logic ----
server <- function(input, output, session) {
  
  #data <- reactiveValues(a = NULL)
  
  
  output$unique_round <- renderUI({
    ### is there a player?
    
    # select based on environment
    environment_selected <- points_when_gem_found %>%
      filter(env_number == input$env_number)
    
    # select based on performance
    performance_selected <- environment_selected %>%
      filter(performance_group == input$performance)
    
    # select based on when gem is found
    
    early_gems_idx <-
      which(explore_data$gem & explore_data$trial <= input$gem_when)
    round_to_exclude <-
      unique(explore_data$unique_rounds[early_gems_idx])
    
    gem_when_selected <- performance_selected %>%
      filter(unique_rounds %!in% round_to_exclude)
    
   selectInput(
      inputId = "selected_round",
      label = "Choose a round to examine",
      choices = gem_when_selected$unique_rounds
      
    )
  })
  
  
plotData <- eventReactive(  input$selected_round, {   
    # Make sure that input$slider2 is ready to be used
       # Will be executed as soon as above requirement is fulfilled
      explore_data %>%
      dplyr::filter(unique_rounds == as.numeric(input$selected_round))
})
    

  
  
    # output$plot <- renderPlot({
    #   req(input$selected_round)
    #   
    #   # ggplot() +
    #   #   geom_histogram(data = data$a, aes(x = points ))
    #   
    #     ### make this into a function!
    #   one_sequence <-
    #     ggplot(data$a) +
    #       #geom_bin_2d(aes(x = x, y = y, fill = gem), binwidth = c(1, 1)) +
    #       # geom_rect(
    #       #   data = gemCoords(),
    #       #   size = 1,
    #       #   colour = "red",
    #       #   fill = "white",
    #       # 
    #       #   aes(
    #       #     xmin = x - 1,
    #       #     xmax = x  ,
    #       #     ymin = y - 1,
    #       #     ymax = y
    #       #   )
    #       # ) +
    #       geom_rect(
    #         aes(
    #           xmin = x - 1,
    #           xmax = x  ,
    #           ymin = y - 1,
    #           ymax = y,
    #           fill = points,
    #           frame = trial
    #         ),
    #         color = "black",
    #         size = .2
    #       ) +
    # 
    #       scale_x_continuous(limits = c(-1, 7), breaks = -1:7) +
    #       scale_y_continuous(limits = c(-1, 7), breaks = -1:7) +
    #       scale_fill_continuous_divergingx(palette = "RdBu", mid = 0)
    # 
    #     ggplotly(one_sequence) %>%
    #       animation_opts(transition = 0)
    #     
    # })  
    
    
    
    
    
  
  # gemCoords <- eventReactive(input$selected_round, {
  #     
  #     # Make sure that input$slider2 is ready to be used
  #     req(input$selected_round)
  #     
  #     explore_data %>% 
  #       filter( points >160) %>%
  #       select(x , y, env_number, round, unique_rounds, trial) %>%
  #       dplyr::filter(unique_rounds == as.numeric(input$selected_round)) %>% 
  #       distinct()
  #     
  #   })
  # 

  output$min_max <- renderText({

    req(input$selected_round)
a <- plotData()
    paste(a$points)

  })

  # 
  # 
 


} 
  


# Run the app ----
shinyApp(ui = ui, server = server)
