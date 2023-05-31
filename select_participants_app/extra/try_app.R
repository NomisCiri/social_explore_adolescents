library(shiny)
library(DT)
library(dplyr)
library(shinyWidgets)


col_1 <- c("A1", "A1", "A1", "A2", "A2", "B1", "B2", "C1", "C1", "C1")
col_2 <- c("a", "b", "c", "d", "e", "a", "b", "a", "b", "c")
col_3 <- c("Benz", "Audi", "Renault", "Ferrari", "Porsche", "Mercedes", "Benz", "Benz", "Audi", "Renault")

data_1 <- data.frame(col_1, col_2, col_3, stringsAsFactors = TRUE)
colnames(data_1) <- c("Building", "Spot", "Car")

server <- function(input, output, session) {
  filterCars <- reactive({
    filterCar <- data_1
    filterCar <- droplevels.data.frame(filterCar)
    return(filterCar)
  })


  filterBuilding <- reactive({
    unique(as.character(filterCars()$Building))
  })

  observeEvent(filterBuilding(), {
    updatePickerInput(session,
      "filter_Building",
      choices = filterBuilding(),
      selected = sort(filterBuilding())
    )
  })

  # # Subset dynamically the previous reactive filter #
  datasub1 <- reactive({
    data_1[data_1$Building %in% input$filter_Building, ]
  })

  filterSpot <- reactive({
    unique(as.character(datasub1()$Spot))
  })

  observeEvent(filterSpot(), {
    updatePickerInput(session,
      "filter_Spot",
      choices = sort(filterSpot()),
      selected = sort(filterSpot())
    )
  })

  # Subset dynamically the previous reactive filter #
  datasub2 <- reactive({
    # browser()
    data_1[data_1$Spot %in% input$filter_Spot, ]
  })

  filterBrand <- reactive({
    unique(as.character(datasub2()$Car))
  })

  observeEvent(filterBrand(), {
    updatePickerInput(session,
      "filter_Brand",
      choices = sort(filterBrand()),
      selected = sort(filterBrand())
    )
  })


  output$databaseCars <- DT::renderDT({
    #  Subset for plotly reactivity
    Filter1 <- droplevels.data.frame(data_1)
    Filter2 <- filter(
      Filter1,
      Filter1$Building %in% input$filter_Building,
      Filter1$Spot %in% input$filter_Spot,
      Filter1$Car %in% input$filter_Brand
    )

    # Plot
    datatable(Filter2,
      filter = "none",
      selection = "none",
      escape = FALSE,
      rownames = FALSE,
      # colnames = c("", ""),
      autoHideNavigation = TRUE,
      style = "bootstrap4",
      options = list(
        searching = FALSE, # remove search option
        ordering = FALSE, # remove sort option
        paging = FALSE, # remove paging
        info = FALSE # remove bottom information
      )
    ) %>%
      formatStyle(columns = 1, fontWeight = "bold", `text-align` = "left") # text to bold and lign left in first column
  })
}

# User Interface
ui <- fluidPage(
  mainPanel(
    fluidRow(
      column(
        12,
        pickerInput(
          inputId = "filter_Building", "Building",
          choices = NULL,
          multiple = TRUE,
          width = "1250px",
          options = list(`actions-box` = TRUE),
          selected = NULL
        )
      )
    ),
    fluidRow(
      column(
        12,
        pickerInput(
          inputId = "filter_Spot", "Spot",
          choices = NULL,
          multiple = TRUE,
          width = "1250px",
          options = list(`actions-box` = TRUE),
          selected = NULL
        )
      )
    ),
    fluidRow(
      column(
        12,
        pickerInput(
          inputId = "filter_Brand", "ID",
          choices = NULL,
          multiple = TRUE,
          width = "1250px",
          selected = NULL,
          options = list("max-options" = 4, `actions-box` = TRUE)
        )
      )
    ),
    p(DTOutput("databaseCars"))
  )
)

shinyApp(ui, server)
