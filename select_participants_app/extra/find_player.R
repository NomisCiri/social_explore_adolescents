library(shiny)
library(tidyverse)
library(dplyr)
library(here)
library(reactlog)
library(shinyWidgets)
library(DT)

### Load ----------------------------------------------------------------------
# Load dataset and functions


pacman::p_load(
  tidyverse,
  rjson,
  data.table,
  gghalves,
  plotly,
  gganimate,
  av,
  colorspace
)
`%!in%` <- Negate(`%in%`)


data_1 <- as_tibble(read.csv("data_coord.csv")) %>%
  filter(player > 10)
# readRDS(paste0(here(),"/select_participants_app/explore_data.RDS"))



make_plot <- function(dat, gem_coords) {
  data_round <- dat
  gem_coords_round <- gem_coords

  ggplot() +
    geom_rect(
      data = gem_coords_round,
      size = 1,
      colour = "red",
      fill = "white",
      aes(
        xmin = x - 1,
        xmax = x,
        ymin = y - 1,
        ymax = y
      )
    ) +
    geom_rect(
      data = data_round,
      aes(
        xmin = x - 1,
        xmax = x,
        ymin = y - 1,
        ymax = y,
        fill = points,
        frame = trial
      ),
      color = "black",
      size = .2
    ) +
    scale_x_continuous(limits = c(-1, 7), breaks = -1:7) +
    scale_y_continuous(limits = c(-1, 7), breaks = -1:7) +
    scale_fill_continuous_divergingx(palette = "RdBu", mid = 0) +
    labs(title = "")
}

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
      pickerInput(
        inputId = "filter_env",
        "Select environment(s)",
        choices = NULL,
        multiple = TRUE,
        # width = "1250px",
        options = list(`actions-box` = TRUE),
        selected = NULL
      ),
      pickerInput(
        inputId = "filter_performance",
        "Select level of performance",
        choices = NULL,
        multiple = TRUE,
        # width = "1250px",
        options = list(`actions-box` = TRUE),
        selected = NULL
      ),
      sliderInput(
        inputId = "filter_gem",
        "Gem found after round:",
        min = 1,
        max = 25,
        value = 10
      ),

      # selectInput(inputId = 'filter_gem', 'Gem found before round:',
      #               choices = NULL,
      #               #multiple = TRUE,
      #               #width = "1250px"
      #               #options = list("max-options" = 4, `actions-box` = TRUE)
      #             )



      selectInput(
        inputId = "filter_round",
        "select run to visualize:",
        choices = NULL,
      )
    ),
    mainPanel(plotlyOutput("plot",
      width = 800,
      height = 600
    ))
  )
)

### Server Logic---------------------------------------------------------------


server <- function(input, output, session) {
  # First level: complete dataset

  filterPpts <- reactive({
    filterPpt <- data_1
    return(filterPpt)
  })


  # first filter: which environments we want

  filterEnv <- reactive({
    unique(filterPpts()$env_number)
  })

  # dynamically update filter options when input is changed
  observeEvent(filterEnv(), {
    updatePickerInput(session,
      "filter_env",
      choices = filterEnv(),
      selected = sort(filterEnv())
    )
  })

  # Subset dynamically the previous reactive filter (evironment number)
  datasub1 <- reactive({
    data_1[data_1$env_number %in% input$filter_env, ]
  })

  # second filter: how many points were scored
  filterPerformance <- reactive({
    unique(as.character(datasub1()$performance_group_f))
  })

  # dynamically update filter options when input is changed
  observeEvent(filterPerformance(), {
    updatePickerInput(
      session,
      "filter_performance",
      choices = sort(filterPerformance()),
      selected = sort(filterPerformance())
    )
  })

  # Subset dynamically the previous reactive filter (performance)
  datasub2 <- reactive({
    # browser()
    data_1[data_1$performance_group_f %in% input$filter_performance, ]
  })

  # third filter: how late was a gem found
  filterGem <- reactive({
    datasub2() %>%
      ungroup() %>%
      filter(!is.na(round_gem_found)) %>%
      select(round_gem_found) %>%
      distinct() %>%
      pull(round_gem_found)
  })
  #
  # dynamically update filter options when input is changed
  observeEvent(filterGem(), {
    updateSliderInput(
      session,
      "filter_gem",
      value = min(filterGem()),
      min = 1,
      max = max(filterGem())
    )
  })
  #
  # Subset dynamically according to all thre filters above, to output which
  # rounds satisfy the conditions

  datasub3 <- reactive({
    # browser()
    Filter1 <- data_1
    filter(
      Filter1,
      Filter1$env_number %in% input$filter_env,
      Filter1$performance_group_f %in% input$filter_performance,
      Filter1$round_gem_found >= input$filter_gem
    )
  })


  # reactive dataset: show the rounds that satisfy the filters

  filterRound <- reactive({
    datasub3() %>%
      ungroup() %>%
      select(unique_rounds) %>%
      distinct() %>%
      pull(unique_rounds)
  })

  # update the input picker with the rounds that work
  observeEvent(filterRound(), {
    updateSelectInput(session,
      "filter_round",
      choices = sort(filterRound())
    )
  })


  # find coordinates of the gems for current environment
  gemCoords <- eventReactive(input$filter_round, {
    # which environment number is the selected round?
    current_env <- data_1 %>%
      filter(unique_rounds == input$filter_round) %>%
      select(env_number) %>%
      distinct() %>%
      pull()

    # find coordinates of gems
    data_1 %>%
      filter(points > 160) %>%
      select(x, y, env_number) %>%
      dplyr::filter(env_number == current_env) %>%
      distinct()
  })

  # Make the plot and save into output$plot

  output$plot <- renderPlotly({
    # require that a round is selected
    req(input$filter_round)

    # final subset of the data according to all the filters
    data_plot <- data_1
    data_plot <- filter(
      data_plot,
      data_plot$unique_rounds == input$filter_round
    )

    # make plot with function
    one_sequence <- make_plot(data_plot, gemCoords())

    # make animation
    ggplotly(one_sequence) %>%
      layout(title = list(text = paste(
        "Round nr", data_plot$unique_rounds[1],
        "<br>",
        "<sup>",
        "environment nr:", data_plot$env_number[1],
        "performance = ", data_plot$performance_group_f[1],
        "gem found in trial", data_plot$round_gem_found[1]
      ))) %>%
      animation_opts(transition = 0)
  })
}

### RUN APP --------------------------------------------------------------------


shinyApp(ui, server)
