library(shiny)
library(shinydashboard)
library(shinyauthr)
library(dplyr)
library(DT)
library(plotly)
library(aws.s3)
library(e1071)
library(RSQLite)
library(glue)
library(rsconnect)

source("helpers.R")
readRenviron("./.Renviron")

ui <- dashboardPage(
  dashboardHeader(title = "Bullpen @ Boshamer V2"),
  dashboardSidebar(
    sidebarMenu(
        menuItem(tabName = "Home", text = "Homepage", icon = icon("house")),
        menuItem(tabName = "Data", text = "Data View", icon = icon("table")),
        menuItem(tabName = "Notes", text = "Notes", icon = icon("pencil")),
        menuItem(tabName = "Metrics", text = "Metrics", icon = icon("arrow-up")),
        menuItem(tabName = "Perf", text = "Performance Models", icon = icon("gear"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Home",
              selectInput(inputId = "pitcherSelectHP", label = "Player Select:",
                          choices = NULL, multiple = FALSE),
              selectInput(inputId = "datesSelectHP", label = "Select Sessions:",
                          choices = NULL, multiple = TRUE),
              h3("Bullpen Statistics"),
              fluidRow(
                dataTableOutput("summDisplay")
                )
              ),
      tabItem(tabName = "Notes",
              tabsetPanel(
                tabPanel("Add",
                         textInput(inputId = "titleAdd", label = h3("Title"), placeholder = "Main Point(s): What do you want to say?"),
                         br(),
                         textAreaInput(inputId = "noteBody", label = h3("Body"), placeholder = "Say it!",
                                       width = "500px", height = "300px")),
                tabPanel("View")
              )),
      tabItem(tabName = "Data",
              column(width = 4,
                     selectInput(inputId = "pitcherSelectDV", label = "Player Select:",
                                 choices = NULL, multiple = FALSE)),
              column(width = 4,
                     selectInput(inputId = "datesSelectDV", label = "Select Sessions:",
                                 choices = NULL, multiple = TRUE)),
              DT::dataTableOutput("sessionDataView"),
              style = "overflow-x:scroll;"),
      tabItem(tabName = "Metrics",
              column(width = 6,
                     selectInput(inputId = "pitcherSelectMT", label = "Player Select:",
                                 choices = NULL, multiple = FALSE),
                     selectInput(inputId = "metricSelect1", label = "Choose Pitch Metric:",
                                 choices = NULL),
                     plotlyOutput("metricPlot1", width = "100%",
                                  height = "50%")
                     ),
              column(width = 6,
                     selectInput(inputId = "datesSelectMT", label = "Select Sessions:",
                                 choices = NULL, multiple = TRUE),
                     selectInput(inputId = "metricSelect2", label = "Choose Another Pitch Metric:",
                                 choices = NULL),
                     plotlyOutput("metricPlot2", width = "100%",
                                  height = "50%")),
              ),
      tabItem(tabName = "Perf",
              column(width = 4,
              selectInput(inputId = "pitcherSelectPM", label = "Player Select:",
                          choices = NULL, multiple = FALSE)),
              column(width = 4,
              selectInput(inputId = "datesSelectPM", label = "Select Sessions:",
                          choices = NULL, multiple = TRUE)),
              column(width = 4,
              selectInput(inputId = "batterSideSel", label = "Apply Filters to Left-/Right- Handed Hitters?",
                          choices = c("Left", "Right"), selected = "Left")),
              br(),

              box(
                title = "Whiff Probability",
                solidHeader = T, collapsible = T, collapsed = F,
                sliderInput(inputId = "whiffFilter", label = "Filter by Whiff Prob.", min = 0, max = 1,
                            value = c(0,1)),
                plotlyOutput("whiffPlot")
              ),
              box(
                title = "Called Strike Probability",
                solidHeader = T, collapsible = T, collapsed = F,
                sliderInput(inputId = "csFilter", label = "Filter by Called-Strike Prob.", min = 0, max = 1,
                            value = c(0,1)),
                plotlyOutput("csPlot")
              )
            )
    )
  )
)

server <- function(input, output, session) {

  values <- reactiveValues(gotData = F, data = NULL, notes = NULL,
                           testDataHP = NULL, testDataPM = NULL, testDataMT = NULL,
                           testDataDV = NULL, gotMod = F, readyForModels = F, gotPreds = F,
                           metricChoice = c("RelSpeed", "ZoneSpeed", "VertRelAngle",
                                            "HorzRelAngle", "SpinRate",
                                            "SpinAxis", "RelHeight",
                                            "RelSide", "Extension",
                                            "HorzBreak", "InducedVertBreak",
                                            "PlateLocHeight", "PlateLocSide"))
  #observe({
  #  if (!values$gotLogin) {
  #    showModal(
  #      modalDialog(
  #        title = "Bullpen @ Boshamer Login", footer = NULL, easyClose = F,
  #        h4("Welcome to the Bullpen App for the 2022 ACC Champions!"),
  #        textInput(inputId = "loginEnter", label = "Enter Login Info", value = ""),
  #        textInput(inputId = "passEnter", label = "Enter Password", value = ""),
  #        actionButton(inputId = "login", label = "Login", class = "btn-primary")
  #      )
  #    )
  #  }
  #})

  observe({
    if (!values$gotData) {
      values$data <- getData()
      #values$notes <- getNotes()
      values$gotData <- T
    }
  })

  observeEvent(values$gotData, {
    updateSelectInput(inputId = "pitcherSelectHP",
                      choices = unique(values$data$Pitcher))
    updateSelectInput(inputId = "pitcherSelectMT",
                      choices = unique(values$data$Pitcher))
    updateSelectInput(inputId = "pitcherSelectPM",
                      choices = unique(values$data$Pitcher))
    updateSelectInput(inputId = "pitcherSelectDV",
                      choices = unique(values$data$Pitcher))

    updateSelectInput(inputId = "metricSelect1",
                      choices = values$metricChoice)

    output$sessionDataView <- renderDataTable({values$data})

  })

  observeEvent(input$metricSelect1, {
    updateSelectInput(inputId = "metricSelect2",
                      choices = setdiff(values$metricChoice,
                                        c(input$metricSelect1)))
  })

  observeEvent(input$pitcherSelectHP, {
    if (values$gotData) {
      values$testDataHP <- prep(values$data %>% filter(Pitcher == input$pitcherSelectHP))
      updateSelectInput(inputId = "datesSelectHP",
                        choices = unique(values$testDataHP$Date),
                        selected = unique(values$testDataHP$Date)[1])
    }
  })

  observeEvent(input$pitcherSelectDV, {
    if (values$gotData) {
      values$testDataDV <- prep(values$data %>% filter(Pitcher == input$pitcherSelectDV))
      updateSelectInput(inputId = "datesSelectDV",
                        choices = unique(values$testDataDV$Date),
                        selected = unique(values$testDataDV$Date)[1])
    }
  })

  observeEvent({input$pitcherSelectDV
                input$datesSelectDV}, {
                  output$sessionDataView <- renderDataTable({values$data %>%
                                                             filter(Pitcher == input$pitcherSelectDV) %>%
                                                             filter(Date %in% input$datesSelectDV)
                      })
                })

  observeEvent(input$pitcherSelectPM, {
    if (values$gotData) {
      values$testDataPM <- prep(values$data %>% filter(Pitcher == input$pitcherSelectPM))
      updateSelectInput(inputId = "datesSelectPM",
                        choices = unique(values$testDataPM$Date),
                        selected = unique(values$testDataPM$Date)[1])
    }
  })

  observeEvent(input$pitcherSelectMT, {
    if (values$gotData) {
      values$testDataMT <- prep(values$data %>% filter(Pitcher == input$pitcherSelectMT))
      updateSelectInput(inputId = "datesSelectMT",
                        choices = unique(values$testDataMT$Date),
                        selected = unique(values$testDataMT$Date)[1])
    }
  })

  observeEvent({input$datesSelectHP}, {
    if (values$gotData) {
      output$summDisplay <- renderDataTable({
        summaryStats(values$data,
                     input$pitcherSelectHP,
                     input$datesSelectHP)})
    }
  })

  observeEvent(input$datesSelectMT, {
    if (!is.null(values$testDataMT)) {
      output$metricPlot1 <- renderPlotly({timeGraphs(values$testDataMT,
                                       input$pitcherSelectMT,
                                       input$datesSelectMT,
                                       input$metricSelect1)})

      output$metricPlot2 <- renderPlotly({timeGraphs(values$testDataMT,
                                       input$pitcherSelectMT,
                                       input$datesSelectMT,
                                       input$metricSelect2)})
    }
  })

  observe({
    if (!is.null(values$testDataPM) && nrow(values$testDataPM) > 0) {
      if (length(unique(values$testDataPM$PitcherThrows)) > 1) {
        print(glue("Check for Switch Pitcher: {p}",
                   p = values$testDataPM$Pitcher[1]))
      }

      values$testDataPM$pWhiffL <- useModels(values$testDataPM,
                                             unique(values$testDataPM$PitcherThrows[1]),
                                             "Left", T)
      values$testDataPM$pWhiffR <- useModels(values$testDataPM,
                                             unique(values$testDataPM$PitcherThrows[1]),
                                             "Right", T)
      values$testDataPM$pCSL <- useModels(values$testDataPM,
                                          unique(values$testDataPM$PitcherThrows[1]),
                                          "Left", F)
      values$testDataPM$pCSR <- useModels(values$testDataPM,
                                          unique(values$testDataPM$PitcherThrows[1]),
                                          "Right", F)
      values$gotPreds <- T
    }
  })

  observeEvent({input$datesSelectPM
                input$batterSideSel
                input$whiffFilter
                input$csFilter}, {

    if (values$gotPreds) {
      test <- values$testDataPM %>% filter(Date %in% input$datesSelectPM)

      minWF <- input$whiffFilter[1]
      maxWF <- input$whiffFilter[2]
      minCS <- input$csFilter[1]
      maxCS <- input$csFilter[2]

      output$whiffPlot <- renderPlotly({makeWhiffPlot(test, input$batterSideSel,
                                                      minWF, maxWF)})
      output$csPlot <- renderPlotly({makeCSPlot(test, input$batterSideSel,
                                                minCS, maxCS)})
    }
  })
}

shinyApp(ui, server)
