source("./global.R")
ui <- dashboardPage(
  dashboardHeader(title = "Bullpen @ Boshamer V2"),
  dashboardSidebar(
    sidebarMenu(
        menuItem(tabName = "Home", text = "Homepage", icon = icon("house")),
        menuItem(tabName = "Notes", text = "Notes", icon = icon("pencil")),
        menuItem(tabName = "Comp", text = "Sessions vs. Games", icon = icon("film")),
        menuItem(tabName = "Metrics", text = "Metrics", icon = icon("arrow-up")),
        menuItem(tabName = "Perf", text = "Performance Models", icon = icon("gear"),
                 menuSubItem("Whiff/Called-Strike Model",
                             tabName = "WhiffCS",icon = icon("arrow-right")),
                 menuSubItem("Pitch-Class Model", tabName = "PClass",
                             icon = icon("magnifying-glass")))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Home",
              column(width = 12,
              selectInput(inputId = "pitcherSelectHP", label = "Player Select:",
                          choices = NULL, multiple = FALSE),
              fluidRow(
                uiOutput("mostRecentDate"),
                uiOutput("summLastDisplay"),
                h4("All Bullpen Sessions This Season"),
                uiOutput("summSznDisplay")
                ),
              style = "overflow-y:scroll;
                       height:550px;
                       border-radius:15px;
                       padding-right:40px;"
              )),
      tabItem(tabName = "Notes",

              tabsetPanel(
                tabPanel("Add Note",
                         br(),
                         sidebarLayout(
                           sidebarPanel = sidebarPanel(
                             width = 4,
                             h2("Add Note"),
                             textInput(inputId = "noteTitle",
                                       label = "Title",
                                       width = "100%",
                                       placeholder = "Main Point or Message"),
                             textAreaInput(inputId = "noteMessage",
                                           label = "Note",
                                           width = "100%",
                                           placeholder = "Say something! (Avoid using quotes around words if possible, for now. The app currently cannot handle that type of input.)"),
                             actionButton(inputId = "noteAdd", label = "Add Note", class = "btn-primary"),
                             br(),
                             h5(HTML("<b>Include Pitches</b>")),
                             h5("Any included pitches below will be included. Deselect pitches to the right
                               to un-include them. Click the 'Clear' Button to clear the table below."),
                             actionButton(inputId = "refClear", label = "Clear Ref. Table", class = "btn-primary"),
                             tableOutput(outputId = "refSelectTable")
                           ),
                           mainPanel = mainPanel(
                             h4("Include Pitches in Note! (Click Once to Select -- Click Headings to Sort By Category)"),
                             fluidRow(
                               column(width = 4,
                                      selectInput(inputId = "pitcherSelectDV", label = "Player Select:",
                                                  choices = NULL, multiple = FALSE)),
                               column(width = 4,
                                      selectInput(inputId = "datesSelectDV", label = "Select Sessions:",
                                                  choices = NULL, multiple = TRUE)),
                               DT::dataTableOutput("sessionDataView"))))),
                tabPanel("View Notes",
                                h3("Team Notes"),
                                selectInput(inputId = "pitcherSelectNV", label = "Player Select:",
                                     choices = NULL, multiple = FALSE),
                                column(width = 6,
                                       h4("Select Note:"),
                                       fluidRow(
                                         dataTableOutput("notesTable"),
                                         style = "overflow-y:scroll;
                                                  height:100px;
                                                  padding-right:10px;"
                                       )),
                                column(width = 6,
                                       h3(
                                         textOutput("selectedNoteTitle")),
                                       hr(),
                                       textOutput("selectedNoteMsg"),
                                       style = "background-color:white;
                              border-radius:15px;
                              padding-left:5px;"),
                              dataTableOutput("ascPitchesTable")
                         )
                       #         actionButton("vizView", label = "View Associated Pitches",
                       #                       class = "btn-success", icon = icon("binoculars"))
              )),
      tabItem(tabName = "Comp",
              selectInput(inputId = "pitcherSelectSG", label = "Player Select:",
                          choices = NULL, multiple = FALSE),
              column(width = 7,
                     h4("Before Last Live Appearance"),
                     DT::dataTableOutput("beforeLAView"),
                     uiOutput("mostRecentLA"),
                     DT::dataTableOutput("LAView"),
                     h4("After Last Live Appearance"),
                     DT::dataTableOutput("afterLAView"),
                     style = "overflow-y:scroll;height:500px;"),
              column(width = 5,
                     h3("Pitch Outcomes From Last Live Appearance"),
                     h4("Versus LHH"),
                     DT::dataTableOutput("LALHH"),
                     h4("Versus RHH"),
                     DT::dataTableOutput("LARHH"))
              ),
      tabItem(tabName = "Metrics",
              fluidRow(column(3,selectInput(inputId = "pitcherSelectMT", label = "Player Select:",
                                   choices = NULL, multiple = FALSE)),
                       column(3,selectInput(inputId = "datesSelectMT", label = "Select Sessions:",
                                   choices = NULL, multiple = TRUE)),
                       column(3,selectInput(inputId = "metricSelect1", label = "Choose Pitch Metric:",
                                   choices = NULL)),
                       column(3,selectInput(inputId = "metricSelect2", label = "Choose Another Pitch Metric:",
                                   choices = NULL))),
              fluidRow(column(12,plotOutput("metricPlot1"))),
              br(),
              fluidRow(column(12,plotOutput("metricPlot2")))
              ),
      tabItem(tabName = "WhiffCS",
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
                title = "Strike-Looking Probability",
                solidHeader = T, collapsible = T, collapsed = F,
                sliderInput(inputId = "csFilter", label = "Filter by Strike-Looking Prob.", min = 0, max = 1,
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
                           whiffThresh = 0, csThresh = 0, stageNewRef = F, keepRefs = T,
                           metricChoice = c("RelSpeed", "SpinRate",
                                            "SpinAxis", "RelHeight",
                                            "RelSide", "Extension",
                                            "HorzBreak", "InducedVertBreak",
                                            "PlateLocHeight", "PlateLocSide"),
                           pitcherList = readLines("./2023PitcherList.txt"))

  observe({
    if (!values$gotData) {
      values$data <- getData()
      values$data$Date <- strftime(as.Date(values$data$Date, format = "%m/%d/%Y"),
                                   format = "%Y-%m-%d")
      values$livegame <- getOurGames()

      values$gotData <- T
      values$allNotes <- getNotes(con)

      # values$whiffThresh <- calThresh(values$livegame, "Whiff")
      # values$csThresh <- calThresh(prep(values$livegame))
    }
  })

  observeEvent(values$gotData, {
    options1 <- unique(values$data$Pitcher)
    options2 <- unique(values$livegame$Pitcher)

    updateSelectInput(inputId = "pitcherSelectHP",
                      choices = sort(intersect(options1, values$pitcherList)))
    updateSelectInput(inputId = "pitcherSelectNV",
                      choices = sort(intersect(options1, values$pitcherList)))
    updateSelectInput(inputId = "pitcherSelectMT",
                      choices = sort(intersect(options1, values$pitcherList)))
    updateSelectInput(inputId = "pitcherSelectSG",
                      choices = sort(unique(intersect(intersect(
                                    values$livegame$Pitcher,
                                    values$data$Pitcher),
                                    values$pitcherList)
                                )))
    updateSelectInput(inputId = "pitcherSelectPM",
                      choices =  sort(intersect(options1, values$pitcherList)))
    updateSelectInput(inputId = "pitcherSelectDV",
                      choices =  sort(intersect(options1, values$pitcherList)))

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

  observeEvent({values$allNotes
                input$pitcherSelectNV}, {
    if (input$pitcherSelectNV != "" && !(nrow(values$allNotes) == 0)) {
      temp <- data.frame(Date = values$allNotes[,2], User = values$allNotes[,3], Title = values$allNotes[,4])
      temp$User <- trimws(temp$User, which = c("both"))
      p <- strsplit(gsub(" ", "", input$pitcherSelectNV),",")[[1]]
      p <- paste(p[2], p[1])
      print(which(temp$User == p))
      temp <- temp %>% filter(User %in% c(p, "Coach Forbes", "Coach Gaines", "Coach Howell",
                                             "Coach Wierzbicki", "Da Analytics Team")) # including analytics for testing
      output$notesTable <- renderDataTable({DT::datatable(temp, options = list(dom = 't', pageLength = -1),
                                                          rownames = F, selection = list(mode = "single"))})
    }
  })

  observeEvent(input$sessionDataView_rows_selected, {
    temp <- c(values$sessionDV[input$sessionDataView_rows_selected,]$PitchUID)
    if (!(is.null(values$refData))) {
      if (nrow(values$refData) > 0) {
        currID <- unique(values$data %>% filter(Pitcher == input$pitcherSelectDV) %>%
                         select(PitcherId))

        temp2 <- values$refData %>% filter(PitcherId != currID)
        values$uids <- c(temp, as.vector(temp2[,5]))
      }
    } else {
      values$uids <- temp
    }
    values$refData <- unique(rbind(values$refData,
                            shortenRef(values$data %>% filter(PitchUID %in% values$uids))))
    values$refData <- values$refData %>% filter(values$refData[,5] %in% values$uids)
    print(values$refData)
    output$refSelectTable <- renderTable(values$refData[,1:4])
    values$stageNewRef <- T
    values$keepRefs <- T
  })

  observe({
    if (values$stageNewRef) {
      pitchUID <- values$refData[,5]
      values$refdUIDs <- append(values$refdUIDs, pitchUID)
      print(values$refdUIDs)
      values$stageNewRef <- F
    } else if ((length(input$sessionDataView_rows_selected) == 0 && !values$keepRefs)) {
      output$refSelectTable <- renderTable(data.frame())
    }

  })

  observeEvent(input$refClear, {
    values$refData <- NULL
    output$refSelectTable <- renderTable(data.frame())
  })

  observeEvent({input$noteAdd}, {
    print(paste("Login: ", values$login))
    print(paste("Date: ", Sys.Date()))
    print(paste("Title: ", input$noteTitle))
    print(paste("Message: ", input$noteMessage))
    print(values$refdUIDs)
    if (!values$gotLogin) {
      showModal(
        modalDialog(
          title = "Bullpen @ Boshamer", footer = NULL, easyClose = T,
          h4("Add a note for the 2022 ACC Champions! (Password is 'gdtbath', no quotes.)"),
          selectInput(inputId = "loginEnter", label = "Enter Login Info",
                      choices = as.vector(getUsers(con))),
          textInput(inputId = "passEnter", label = "Enter Password", value = ""),
          actionButton(inputId = "login", label = "Login", class = "btn-primary")
        )
      )
    }
  })

  observeEvent(input$login, {
    if (loginUser(con, input$passEnter)) {
      values$gotLogin <- T
      values$login <- input$loginEnter
      removeModal()
    }
  })

  observe({
    if (values$gotLogin) {
      print(values$refdUIDs)
      addNote(con, values$login, input$noteTitle, input$noteMessage, values$refdUIDs)
      values$allNotes <- getNotes(con)

      updateTextInput(session = session, "noteTitle", value = "")
      updateTextAreaInput(session = session, "noteMessage", value = "")
      values$refdUIDs <- c()
      values$keepRefs <- F
      values$refData <- NULL
      output$refSelectTable <- renderTable(data.frame())
    }
    values$gotLogin <- F
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
    temp2 <- data.frame(values$vizData %>%
                          select(TaggedPitchType,
                                 RelSpeed,
                                 SpinRate,
                                 InducedVertBreak,
                                 HorzBreak,
                                 RelHeight,
                                 Tilt))
    temp2 <- unique(temp2)
    colnames(temp2) <- c("Pitch Type", "Velo", "Spin", "Vert. Break", "Horz. Break",
                         "Release Height", "Tilt")
    output$ascPitchesTable <- renderDataTable(DT::datatable(temp2,rownames = F))
  })

  observeEvent(input$vizView, {
    print(values$vizData)
    if (!is.null(values$vizData) && nrow(values$vizData) > 0) {
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
    if (values$gotData && input$pitcherSelectHP != "") {
      values$testDataHP <- values$data %>% filter(Pitcher == input$pitcherSelectHP)

      temp <- summaryStats(values$testDataHP,
                           input$pitcherSelectHP,
                           unique(values$testDataHP$Date))
      sznTable <- kable(temp, booktabs=T) %>%
                  kable_styling(font_size = 13)

      output$summSznDisplay <- renderUI({HTML(sznTable)})


      output$mostRecentDate <- renderUI({h4(paste0("Most Recent Bullpen Session: ", max(values$testDataHP$Date)))})

      temp2 <- summaryStats(values$testDataHP %>% filter(Date == max(Date)),
                            input$pitcherSelectHP, max(values$testDataHP$Date))

      commonPTs <- intersect(temp$`Pitch Type`, temp2$`Pitch Type`)
      idx <- which(temp2$`Pitch Type` %in% commonPTs)
      veloAbvAvg <- temp2$`Avg. Velo`[idx] - temp$`Avg. Velo`[idx]
      spinAbvAvg <- temp2$`Spin Rate`[idx] - temp$`Spin Rate`[idx]

      lastTable <- kable(temp2,booktabs=T) %>%
                   kable_styling(font_size = 13) %>%
                     column_spec(3, color = "white",
                                 background = spec_color2(veloAbvAvg, palette = paletteer_d("RColorBrewer::RdBu"))) %>%
                     column_spec(4, color = "white",
                                 background = spec_color2(spinAbvAvg, palette = paletteer_d("RColorBrewer::RdBu")))


      output$summLastDisplay <-renderUI({HTML(lastTable)})
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
                             RelHeight, RelSide, Extension)}, rownames = FALSE,
                      options = list(scrollX = T))
                })

  observeEvent(input$pitcherSelectSG, {
    if (!is.null(values$livegame)) {
      liveapps <- values$livegame %>% filter(Pitcher == input$pitcherSelectSG)
      appearances <- unique(liveapps$Date)
      appearances <- appearances[which(nchar(appearances) > 0)]
      lastApp <- sort(appearances, decreasing = T)[length(appearances)]
      print(lastApp)
      lastAppDF <- liveapps %>% filter(Date == lastApp)


      output$mostRecentLA <- renderUI({h4(paste0("Last Live Appearance: ", unique(lastApp)[1], " Versus ", unique(lastAppDF$BatterTeam)))})

      if (!is.null(lastApp)) {
        seshBefore <- values$data %>% filter(Pitcher == input$pitcherSelectSG) %>%
          filter(Date < lastApp) %>% filter(Date == max(Date))
        print(seshBefore)
        seshAfter <- values$data %>% filter(Pitcher == input$pitcherSelectSG) %>%
          filter(Date > lastApp) %>% filter(Date == min(Date))

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

      testDataMT1 <- prep(values$data %>% filter(Pitcher == input$pitcherSelectMT))
      testDataMT1 <- cbind(testDataMT1,
                           data.frame(Scenario = rep.int(1,nrow(testDataMT1))))
      testDataMT2 <- prep(values$livegame %>% filter(Pitcher == input$pitcherSelectMT))
      testDataMT2 <- cbind(testDataMT2,
                           data.frame(Scenario = rep.int(0,nrow(testDataMT2))))
      values$testDataMT <- rbind(testDataMT1[,c(values$metricChoice,
                                                "Date","Pitcher","Scenario",
                                                "TaggedPitchType")],
                                 testDataMT2[,c(values$metricChoice,
                                                "Date","Pitcher","Scenario",
                                                "TaggedPitchType")])
      updateSelectInput(inputId = "datesSelectMT",
                        choices = unique(values$testDataMT$Date),
                        selected = unique(values$testDataMT$Date)[1])
    }
  })

  observeEvent(input$datesSelectMT, {
    if (!is.null(values$testDataMT)) {
      output$metricPlot1 <- renderPlot({timeGraphs(values$testDataMT,
                                       input$pitcherSelectMT,
                                       input$datesSelectMT,
                                       input$metricSelect1)})

      output$metricPlot2 <- renderPlot({timeGraphs(values$testDataMT,
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
