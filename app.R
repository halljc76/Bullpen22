source("./global.R")
ui <- dashboardPage(
  dashboardHeader(title = "Bullpen @ Boshamer V2"),
  dashboardSidebar(
    sidebarMenu(
        menuItem(tabName = "Home", text = "Homepage", icon = icon("house")),
        menuItem(tabName = "Notes", text = "Notes", icon = icon("pencil")),
        menuItem(tabName = "Comp", text = "Sessions vs. Games", icon = icon("film")),
        menuItem(tabName = "Metrics", text = "Metrics", icon = icon("arrow-up")),
        menuItem(tabName = "Perf", text = "Performance Models", icon = icon("gear"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Home",
              column(width = 6,
              selectInput(inputId = "pitcherSelectHP", label = "Player Select:",
                          choices = NULL, multiple = FALSE),
              fluidRow(
                uiOutput("mostRecentDate"),
                dataTableOutput("summLastDisplay"),
                h4("Entire Season's Sessions Data"),
                dataTableOutput("summSznDisplay")
                ),
              style = "overflow-y:scroll;
                       width:650px;
                       height:550px;
                       border-radius:15px;
                       padding-right:40px;"
              ),
              column(width = 4,
                     h3("Team Notes"),
                     h4("Select Note:"),
                     fluidRow(
                     dataTableOutput("notesTable"),
                     style = "overflow-y:scroll;
                              height:200px;"
                     ),
                     br(),
                     fluidRow(
                     h3(
                       textOutput("selectedNoteTitle")),
                     hr(),
                     textOutput("selectedNoteMsg"),
                     style = "background-color:white;
                              border-radius:15px;
                              padding-left:5px;"),
                     br(),
                     actionButton("vizView", label = "View Associated Pitches",
                                  class = "btn-success", icon = icon("binoculars"))
              )),
      tabItem(tabName = "Notes",
            fluidRow(
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
                   verbatimTextOutput("refView"),),
            column(width = 6,
                   h5(HTML("<b>Include Pitches</b>")),
                   h5("To add possible references, click on rows in the 'Data View' tab.
                      Summary info about each selected pitch will update below.
                      Select from the pitches below the ones you want to include in the note."),
                   actionButton(inputId = "refSelectInsert", label = "Insert Selected Pitch", icon = icon("arrow-up")),
                   br(),
                   br(),
                   DT::dataTableOutput(outputId = "refSelectTable"))),
            hr(),
            br(),
            h4("Include Pitches in Note! (Click Once to Select -- Click Headings to Sort By Category)"),
            fluidRow(
              column(width = 4,
                     selectInput(inputId = "pitcherSelectDV", label = "Player Select:",
                                 choices = NULL, multiple = FALSE)),
              column(width = 4,
                     selectInput(inputId = "datesSelectDV", label = "Select Sessions:",
                                 choices = NULL, multiple = TRUE)),
            DT::dataTableOutput("sessionDataView"))),
      tabItem(tabName = "Comp",
              selectInput(inputId = "pitcherSelectSG", label = "Player Select:",
                          choices = NULL, multiple = FALSE),
              column(width = 6,
                     h4("Before Last Live Appearance"),
                     DT::dataTableOutput("beforeLAView"),
                     uiOutput("mostRecentLA"),
                     DT::dataTableOutput("LAView"),
                     h4("After Last Live Appearance"),
                     DT::dataTableOutput("afterLAView"),
                     style = "overflow-y:scroll;height:500px;"),
              column(width = 6,
                     h3("Pitch Outcomes From Last Live Appearance"),
                     h4("Versus LHH"),
                     DT::dataTableOutput("LALHH"),
                     h4("Versus RHH"),
                     DT::dataTableOutput("LARHH"))
              ),
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
              h4("Hover Over Points to See More Info!"),
              br(),
              box(
                title = "Whiff Probability (Given a Swing)",
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

  con <- dbConnect(RPostgres::Postgres(), dbname = Sys.getenv("DB"),
                   host=Sys.getenv("HOSTDB"), port=26257,
                   user=Sys.getenv("COCKROACH_USER"),
                   password=Sys.getenv("COCKROACH_PASSWORD"), sslmode = NULL,
                   options = "--cluster=bullpen-notes-4213")

  values <- reactiveValues(gotLogin = F, gotData = F, data = NULL, login = NULL,
                           testDataHP = NULL, testDataPM = NULL, testDataMT = NULL,
                           testDataDV = NULL, allNotes = NULL, allRefs = NULL,
                           refData = NULL, sessionDV = NULL, uids = c(), refdUIDs = c(),
                           gotMod = F, readyForModels = F, gotPreds = F, vizData = NULL,
                           metricChoice = c("RelSpeed", "SpinRate",
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
      values$data$Date <- strftime(as.Date(values$data$Date, format = "%m/%d/%Y"),
                                   format = "%Y-%m-%d")
      values$livegame <- getOurGames()
      values$gotData <- T
      values$allNotes <- getNotes(con)
    }
  })

  observeEvent(values$gotData, {
    updateSelectInput(inputId = "pitcherSelectHP",
                      choices = unique(values$data$Pitcher))
    updateSelectInput(inputId = "pitcherSelectMT",
                      choices = unique(values$data$Pitcher))
    updateSelectInput(inputId = "pitcherSelectSG",
                      choices = unique(intersect(
                                    values$livegame$Pitcher,
                                    values$data$Pitcher)
                                ))
    updateSelectInput(inputId = "pitcherSelectPM",
                      choices = unique(values$data$Pitcher))
    updateSelectInput(inputId = "pitcherSelectDV",
                      choices = unique(values$data$Pitcher))

    updateSelectInput(inputId = "metricSelect1",
                      choices = values$metricChoice)

    output$sessionDataView <- renderDataTable({values$data %>% select(PitchNo,
                                                              TaggedPitchType,
                                                              RelSpeed,
                                                              SpinRate,Tilt,
                                                              InducedVertBreak,
                                                              HorzBreak,
                                                              RelHeight,
                                                              RelSide,
                                                              Extension)})
  })

  observeEvent(values$allNotes, {
    temp <- data.frame(Date = values$allNotes[,2], User = values$allNotes[,3], Title = values$allNotes[,4])
    output$notesTable <- renderDataTable({DT::datatable(temp, options = list(dom = 't', pageLength = -1),
                                                        rownames = F, selection = list(mode = "single"))})
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
    values$allNotes <- getNotes(con)

    updateTextInput("noteTitle", placeholder = "")
    updateTextAreaInput("noteMessage", placeholder = "")
    values$refdUIDs <- c()
  })

  observeEvent(input$notesTable_rows_selected, {
    output$selectedNoteTitle <- renderText({values$allNotes[input$notesTable_rows_selected,4]})
    output$selectedNoteMsg <- renderText({values$allNotes[input$notesTable_rows_selected,5]})

    clickedID <- values$allNotes[input$notesTable_rows_selected,1]
    refdPitches <- dbGetQuery(con, glue("SELECT * FROM Refs WHERE noteid = '{n}'", n = clickedID))$pitchuid

    values$vizData <- data.frame()
    for (uid in refdPitches) {
      values$vizData <- rbind(values$vizData, values$data[values$data$PitchUID == uid,])
    }
  })

  observeEvent(input$vizView, {
    print(values$vizData)
    if (!is.null(values$vizData)) {
      showModal(
        modalDialog(
          h4("Hover over points for more information!"),
          constructViz(values$vizData)
        )
      )
    }
  })

  observeEvent(input$metricSelect1, {
    updateSelectInput(inputId = "metricSelect2",
                      choices = setdiff(values$metricChoice,
                                        c(input$metricSelect1)))
  })

  observeEvent(input$pitcherSelectHP, {
    if (values$gotData) {
      values$testDataHP <- prep(values$data %>% filter(Pitcher == input$pitcherSelectHP))
      output$summSznDisplay <- renderDataTable({DT::datatable(summaryStats(values$testDataHP,
                                                               input$pitcherSelectHP, unique(values$testDataHP$Date)),
                                                               options = list(dom = 't'),rownames = F)})

      lastDate <- unique(values$testDataHP$Date)[length(unique(values$testDataHP$Date))]
      output$mostRecentDate <- renderUI({h4(paste0("Most Recent Session: ", lastDate))})
      output$summLastDisplay <- renderDataTable({DT::datatable(summaryStats(values$testDataHP, input$pitcherSelectHP,
                                                                           c(lastDate)), options = list(dom = 't'),rownames = F)})
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
                  output$sessionDataView <- renderDataTable({values$sessionDV %>%
                      select(PitchNo, TaggedPitchType,RelSpeed,SpinRate,Tilt,
                             InducedVertBreak,HorzBreak,
                             RelHeight, RelSide, Extension)}, rownames = FALSE)
                })

  observeEvent(input$pitcherSelectSG, {
    if (!is.null(values$livegame)) {
      liveapps <- values$livegame %>% filter(Pitcher == input$pitcherSelectSG)
      appearances <- unique(liveapps$Date)
      appearances <- appearances[which(nchar(appearances) > 0)]
      lastApp <- sort(appearances, decreasing = T)[length(appearances)]
      lastAppDF <- liveapps %>% filter(Date == lastApp)



      output$mostRecentLA <- renderUI({h4(paste0("Last Live Appearance: ", unique(lastApp)[1], " Versus ", unique(lastAppDF$BatterTeam)))})

      if (!is.null(lastApp)) {
        seshBefore <- values$data %>% filter(Pitcher == input$pitcherSelectSG) %>%
          filter(Date <= lastApp)
        seshAfter <- values$data %>% filter(Pitcher == input$pitcherSelectSG) %>%
          filter(Date > lastApp)

        if (nrow(seshBefore) > 0) {
          output$beforeLAView <- renderDataTable({DT::datatable(summaryStats(seshBefore, flag = F),
                                                                options = list(dom = 't'),rownames = F)})
        } else {
          output$beforeLAView <- renderDataTable({DT::datatable(data.frame("No Data Before Last Live Appearance"),
                                                                options = list(dom = 't'),rownames = F)})
        }

        if (nrow(lastAppDF) > 0) {
          output$LAView <- renderDataTable({DT::datatable(summaryStats(lastAppDF, flag = F),
                                                          options = list(dom = 't'),rownames = F)})
          output$LALHH <- renderDataTable({DT::datatable(pitchOutcomes(lastAppDF, T),options = list(dom = 't'),rownames = F)})
          output$LARHH <- renderDataTable({DT::datatable(pitchOutcomes(lastAppDF, F),options = list(dom = 't'),rownames = F)})
        }

        if (nrow(seshAfter) > 0) {
          output$afterLAView <- renderDataTable({DT::datatable(summaryStats(seshAfter, flag = F),
                                                               options = list(dom = 't'),rownames = F)})
        } else {
          output$afterLAView <- renderDataTable({DT::datatable(data.frame("No Data After Last Live Appearance"),
                                                               options = list(dom = 't'),rownames = F)})
        }
      }
    }
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
