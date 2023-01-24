library(shiny)
library(shinyWidgets)


# User Interface
ui <- fluidPage(
  titlePanel("visualize participants decisions"),
  
  sidebarLayout(
    sidebarPanel(
      helpText(" You can use the filters below to choose a round to vizualise.
               In each round participants have 25 clicks to find a gem or at least earn the maximum amount of points. 
               By clicking play, you can visualize how participants clicked through the grid.
               The moving box with the black frame represent the cliked tile for that specific round, 
               and the color represents the amount of points that they got (blue means positive, blue means negative,
               darker shades indicate a higher absolute number of points).
               The white boxes with a red frame represent the gems present in that specific environment.
                            "),
      
      radioButtons("gem_present", "Show rounds in which a gem was present:", c("yes","no")),
      
      conditionalPanel(condition = "input.gem_present== 'yes'",
      radioButtons("gem_found", "Was the gem found?", c("yes","no"))
      ),
      
      pickerInput(
        inputId = 'filter_env',
        'Select environment(s)',
        choices = NULL,
        multiple = TRUE,
        #width = "1250px",
        options = list(`actions-box` = TRUE),
        selected = NULL
      )
      ,
      
      pickerInput(
        inputId = 'filter_performance',
        'Select level of performance',
        choices = NULL,
        multiple = TRUE,
        #width = "1250px",
        options = list(`actions-box` = TRUE),
        selected = NULL
      )
      ,
      
      conditionalPanel(
        condition = "input.gem_present == 'yes' & input.gem_found == 'yes'",
      sliderInput(
        inputId = 'filter_gem',
        'Gem found after round:',
        min = 1,
        max = 25,
        value = 10
      )),
      
      # selectInput(inputId = 'filter_gem', 'Gem found before round:',
      #               choices = NULL,
      #               #multiple = TRUE,
      #               #width = "1250px"
      #               #options = list("max-options" = 4, `actions-box` = TRUE)
      #             )
      
      
      
      selectInput(
        inputId = 'filter_round',
        'select run to visualize:',
        choices = NULL,
      )
    ),
    
    mainPanel(plotlyOutput("plot"))
    
  )
  
)
