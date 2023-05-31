library(shiny)
library(shinyWidgets)


### Server Logic---------------------------------------------------------------
server <- function(input, output, session) {
  # First level: complete dataset

  filterPpts <- reactive({
    # start with whole dataseta
    whole_data <- data_1
    # subset into data with gems
    data_gem <- whole_data[whole_data$gempresent == 1, ]

    # and data with no gems
    data_no_gem <- whole_data[whole_data$gempresent == 0, ]

    # make default data with no gems
    filterPpt <- data_no_gem

    # if options to show rounds with gems is selected
    if (input$gem_present == "yes") {
      # check option if gem was found
      if (input$gem_found == "yes") {
        filterPpt <- data_gem[data_gem$gem_found == 1, ]

        # or not
      } else if (input$gem_found == "no") {
        filterPpt <- data_gem[data_gem$gem_found == 0, ]
      }
    }

    # return dataset subset as above
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
    filter_slider <- c(1:25)

    if (input$gem_present == "yes" & input$gem_found == "yes") {
      filter_slider <- datasub2() %>%
        ungroup() %>%
        filter(!is.na(round_gem_found)) %>%
        select(round_gem_found) %>%
        distinct() %>%
        pull(round_gem_found)
    }

    return(filter_slider)
  })

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

  # Subset dynamically according to all three filters above, to output which
  # rounds satisfy the conditions

  datasub3 <- reactive({
    # browser()

    Filter1 <- data_1

    # filter for environments with no gems
    filter_final <- filter(
      Filter1,
      Filter1$gempresent == 0,
      Filter1$env_number %in% input$filter_env,
      Filter1$performance_group_f %in% input$filter_performance
      # Filter1$round_gem_found >= input$filter_gem
    )

    if (input$gem_present == "yes") {
      if (input$gem_found == "yes") {
        # filter for environments with gems
        filter_final <- filter(
          Filter1,
          Filter1$env_number %in% input$filter_env,
          Filter1$performance_group_f %in% input$filter_performance,
          Filter1$round_gem_found >= input$filter_gem
        )
      } else if (input$gem_found == "no") {
        # filter for environments with gems
        filter_final <- filter(
          Filter1,
          Filter1$gempresent == 1,
          Filter1$gem_found == 0,
          Filter1$env_number %in% input$filter_env,
          Filter1$performance_group_f %in% input$filter_performance
        )
      }
    }

    return(filter_final)
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

    gem_coords <- NA

    if (input$gem_present == "yes") {
      # find coordinates of gems
      gem_coords <- data_1 %>%
        filter(points > 160) %>%
        select(x, y, env_number) %>%
        dplyr::filter(env_number == current_env) %>%
        distinct()
    }

    return(gem_coords)
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

    gem <- input$gem_present

    # make plot with function
    one_sequence <- make_plot(data_plot, gem, gemCoords())

    gem_text <- "; gem not present"
    if (input$gem_present == "yes") {
      if (input$gem_found == "no") {
        gem_text <- "; gem not found"
      } else if (input$gem_found == "yes") {
        gem_text <-
          paste("; gem found in trial", data_plot$round_gem_found[1])
      }
    }

    # make animation
    ggplotly(one_sequence) %>%
      layout(title = list(text = paste(
        "Round nr", data_plot$unique_rounds[1],
        "<br>",
        "<sup>",
        "environment nr:", data_plot$env_number[1],
        "performance = ", data_plot$performance_group_f[1],
        gem_text
      ))) %>%
      animation_opts(transition = 0)
  })
}
