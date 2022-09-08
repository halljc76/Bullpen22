library(shiny)
library(shinydashboard)
library(shinyauthr)
library(dplyr)
library(DT)
library(plotly)
library(aws.s3)
library(e1071)
library(glue)
library(RPostgres)
library(DBI)
library(rsconnect)

source("helpers.R")
source("./dbFuncs.R")
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
              sidebarLayout(
                sidebarPanel = sidebarPanel(width = 2,
                                            h4("Your Feed"),
                                            hr()),
                mainPanel = mainPanel(width = 10,
                                      h2("View Note"),
                                      br(),
                                      h2("Add Note"),
                                      column(width = 6,
                                             textInput(inputId = "noteTitle",
                                                       label = "Title",
                                                       width = "100%",
                                                       placeholder = "Main Point or Message"),
                                             textAreaInput(inputId = "noteMessage",
                                                           label = "Note",
                                                           width = "100%",
                                                           placeholder = "Say something!"),
                                             actionButton(inputId = "noteAdd", label = "Add Note", class = "btn-primary"),
                                             h5(HTML("<b>References</b>")),
                                             verbatimTextOutput("refView"),),
                                      column(width = 5,
                                             h5(HTML("<b>Include Pitches</b>")),
                                             h5("To add possible references, click on rows in the 'Data View' tab.
                                                Summary info about each selected pitch will update below.
                                                Select from the pitches below the ones you want to include in the note."),
                                             actionButton(inputId = "refSelectInsert", label = "Insert Selected Pitch", icon = icon("arrow-up")),
                                             br(),
                                             br(),
                                             DT::dataTableOutput(outputId = "refSelectTable")))
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

  # Database Name and Connection Information
  db <- 'defaultdb'
  host_db <- 'free-tier14.aws-us-east-1.cockroachlabs.cloud'
  db_port <- '26257'

  con <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=Sys.getenv("COCKROACH_USER"),
                   password=Sys.getenv("COCKROACH_PASSWORD"), sslmode = NULL, options = "--cluster=bullpen-notes-4213")
  ###########################################

  values <- reactiveValues(gotLogin = T, gotData = F, data = NULL, login = NULL,
                           testDataHP = NULL, testDataPM = NULL, testDataMT = NULL,
                           testDataDV = NULL, allRefs = NULL, refData = NULL,
                           sessionDV = NULL, uids = c(), refdUIDs = c(),
                           gotMod = F, readyForModels = F, gotPreds = F,
                           metricChoice = c("RelSpeed", "ZoneSpeed", "VertRelAngle",
                                            "HorzRelAngle", "SpinRate",
                                            "SpinAxis", "RelHeight",
                                            "RelSide", "Extension",
                                            "HorzBreak", "InducedVertBreak",
                                            "PlateLocHeight", "PlateLocSide"))
  observe({
    if (!values$gotLogin) {
      showModal(
        modalDialog(
          title = "Bullpen @ Boshamer Login", footer = NULL, easyClose = F,
          h4("Welcome to the Bullpen App for the 2022 ACC Champions!"),
          textInput(inputId = "loginEnter", label = "Enter Login Info", value = ""),
          textInput(inputId = "passEnter", label = "Enter Password", value = ""),
          actionButton(inputId = "login", label = "Login", class = "btn-primary")
        )
      )
    }
  })

  observeEvent(input$login, {
    if (loginUser(con, input$loginEnter, input$passEnter)) {
      values$gotLogin <- T
      values$login <- input$loginEnter
      removeModal()
    }
  })

  observe({
    if (!values$gotData && values$gotLogin) {
      values$data <- getData()
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

  observeEvent(input$sessionDataView_rows_selected, {
    values$uids <- append(values$uids, values$sessionDV[input$sessionDataView_rows_selected,]$PitchUID)
    values$uids <- unique(values$uids)
    values$refData <- shortenRef(values$data %>% filter(PitchUID %in% values$uids))
    output$refSelectTable <- renderDataTable({DT::datatable(values$refData[,1:4],selection = list(mode = "single"))})
  })

  observeEvent(input$refSelectInsert, {
    if (length(input$refSelectTable_rows_selected) == 1) {
      pitchUID <- values$refData[input$refSelectTable_rows_selected,]$PitchUID
      values$refdUIDs <- append(values$refdUIDs, pitchUID)
      values$refdUIDs <- unique(values$refdUIDs) # Safeguard that should be unnecessary
    }


    uidstring <- ""
    for (id in values$refdUIDs) {
      uidstring <- paste(uidstring, id, sep = "\n")
    }
    output$refView <- renderText(({uidstring}))
  })

  observeEvent(input$noteAdd, {
    print(paste("Login: ", values$login))
    print(paste("Date: ", Sys.Date()))
    print(paste("Title: ", input$noteTitle))
    print(paste("Message: ", input$noteMessage))
    print(values$refdUIDs)
    addNote(con, values$login, input$noteTitle, input$noteMessage, values$refdUIDs)
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
                  values$sessionDV <- values$data %>%
                    filter(Pitcher == input$pitcherSelectDV) %>%
                    filter(Date %in% input$datesSelectDV)
                  output$sessionDataView <- renderDataTable({values$sessionDV})
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
