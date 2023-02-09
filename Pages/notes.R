notesUI <- function(id) {
  ns <- NS(id)
  tabsetPanel(
    tabPanel(h5("Add Note"),
             br(),
             sidebarLayout(
               sidebarPanel = sidebarPanel(
                 width = 4,
                 h2("Add Note"),
                 textInput(inputId = ns("noteTitle"),
                           label = h4("Title"),
                           width = "100%",
                           placeholder = "Main Point or Message"),
                 textAreaInput(inputId = ns("noteMessage"),
                               label = h4("Note"),
                               width = "100%",
                               placeholder = "Say something here. (Avoid using 'quotes' around words if possible, for now. The app currently cannot handle that type of input.)"),
                 actionButton(inputId = ns("noteAdd"), label = "Add Note", class = "btn-primary"),
                 br(),
                 h4(HTML("<b>Include Pitches</b>")),
                 h5("Any included pitches below will be included. Deselect pitches to the right
                               to un-include them. Click the 'Clear' Button to clear the table below."),
                 actionButton(inputId = ns("refClear"), label = "Clear Ref. Table", class = "btn-primary"),
                 tableOutput(outputId = ns("refSelectTable"))
               ),
               mainPanel = mainPanel(
                 h4("Include Pitches in Note: (Click Once to Select -- Click Headings to Sort By Category)"),
                 fluidRow(
                   column(width = 4,
                          selectInput(inputId = ns("pitcherSelectDV"), label = "Player Select:",
                                      choices = NULL, multiple = FALSE)),
                   column(width = 4,
                          selectInput(inputId = ns("datesSelectDV"), label = "Select Sessions:",
                                      choices = NULL, multiple = TRUE)),
                   DT::dataTableOutput(ns("sessionDataView"),width = "95%"))))),
    tabPanel(h5("View Notes"),
             h2("Team Notes"),
             selectInput(inputId = ns("pitcherSelectNV"), label = "Player Select:",
                         choices = NULL, multiple = FALSE),
             column(width = 6,
                    h3("Select Note:"),
                    fluidRow(
                      dataTableOutput(ns("notesTable")),
                      style = "overflow-y:scroll;
                               height:100px;
                               padding-right:10px;"
                    )),
             column(width = 6,
                    h3(textOutput(ns("selectedNoteTitle"))),
                    hr(),
                    textOutput(ns("selectedNoteMsg")),
                    style = "background-color:white;
                              border-radius:15px;
                              padding-left:5px;"),
             dataTableOutput(ns("ascPitchesTable"))
    )
    #         actionButton("vizView", label = "View Associated Pitches",
    #                       class = "btn-success", icon = icon("binoculars"))
  )
}

notesServer <- function(id,allNotes,
                        data,gotData,pitcherList,
                        con) {
  moduleServer(
    id, 
    function(input,output,session) {
      ns <- session$ns
      values <- reactiveValues(gotLogin = F, stageNewRef = F, keepRefs = T,
                               testDataDV = NULL,sessionDV = NULL,
                               refdUIDs = c(),login = NULL, refData = NULL)
      
      # Load the dropdowns with choices
      observeEvent(gotData,{
        options1 <- unique(data$Pitcher)
        
        updateSelectInput(inputId = "pitcherSelectDV",
                          choices = intersect(options1, pitcherList))
        updateSelectInput(inputId = "pitcherSelectNV",
                          choices = intersect(options1, pitcherList))
      })
      
      
      # Update UI according to selected pitcher for viewing their notes
      observeEvent({input$pitcherSelectNV}, {
          if (input$pitcherSelectNV != "" && !(nrow(allNotes) == 0)) {
            temp <- updateNotesTable(allNotes, input$pitcherSelectNV)
            output$notesTable <- renderDataTable({DT::datatable(temp, options = list(dom = 't', pageLength = -1),
                                               rownames = F, selection = list(mode = "single"))})
          }
        })
      
      # Update dates according to pitcher selection for viewing notes
      observeEvent(input$pitcherSelectDV, {
        if (gotData) {
          
          values$testDataDV <- prep(data %>% filter(Pitcher == input$pitcherSelectDV))
          updateSelectInput(inputId = "datesSelectDV",
                            choices = unique(values$testDataDV$Date),
                            selected = unique(values$testDataDV$Date)[1])
        }
      })
      
      # Update data according to pitcher/date selection
      observeEvent({input$pitcherSelectDV
        input$datesSelectDV}, {
          values$sessionDV <- data %>%
            filter(Pitcher == input$pitcherSelectDV) %>%
            filter(Date %in% input$datesSelectDV)
          output$sessionDataView <- renderDataTable({values$sessionDV %>%
              select(PitchNo, TaggedPitchType,RelSpeed,SpinRate,Tilt,
                     InducedVertBreak,HorzBreak,
                     RelHeight, RelSide, Extension, videoSide, videoBack) %>% 
              mutate(
                RelSpeed = round(RelSpeed, 1),
                SpinRate = round(SpinRate, 0),
                InducedVertBreak = round(InducedVertBreak, 1),
                HorzBreak = round(HorzBreak, 1),
                RelHeight = round(RelHeight, 1),
                RelSide = round(RelSide, 1),
                Extension = round(Extension, 1)
              ) %>% rename("IVB" = "InducedVertBreak", 
                                "PitchType" = "TaggedPitchType")}, rownames = FALSE,
              options = list(scrollX = T),escape = F)
        })
      
      # Temporarily add a reference to the sidebar when row selected
      observeEvent(input$sessionDataView_rows_selected, {
        temp <- c(values$sessionDV[input$sessionDataView_rows_selected,]$PlayID)
        if (!(is.null(values$refData))) {
          if (nrow(values$refData) > 0) {
            currID <- unique( data %>% filter(Pitcher == input$pitcherSelectDV) %>%
                               select(PitcherId))
            # print(nrow(currID))
            
            if (nrow(currID) > 1) {
              temp2 <-  values$refData %>% filter(!(PitcherId %in% as.vector(currID)))
              uids <- c(temp, as.vector(temp2[,5]))
            } else {
              temp2 <-  values$refData %>% filter(PitcherId != currID)
              uids <- c(temp, as.vector(temp2[,5]))
            }
          }
        } else {
           uids <- temp
        }
        values$refData <- unique(rbind(values$refData,
                                       shortenRef( data %>% filter(PlayID %in% uids))))
        values$refData <-  values$refData %>% filter( values$refData[,5] %in% uids)
        output$refSelectTable <- renderTable( values$refData[,1:4])
        values$stageNewRef <- T
        values$keepRefs <- T
      })
      
      # Keep track of UIDs from references that are staged to be added into DB
      observe({
        if (values$stageNewRef) {
          playID <-  values$refData[,5]
          values$refdUIDs <- append(values$refdUIDs, playID)
          values$stageNewRef <- F
        } else if ((length(input$sessionDataView_rows_selected) == 0 && !values$keepRefs)) {
          output$refSelectTable <- renderTable(data.frame())
        }
        
      })
      
      # Process any clearing of the smaller reference table 
      observeEvent(input$refClear, {
        values$refData <- NULL
        output$refSelectTable <- renderTable(data.frame())
      })
      
      # Show a modal to confirm the user wants to add a note, and from whom
      observeEvent({input$noteAdd}, {
        print(paste("Date: ", Sys.Date()))
        print(paste("Title: ", input$noteTitle))
        print(paste("Message: ", input$noteMessage))
        print(values$refdUIDs)
        if (!values$gotLogin) {
          showModal(
            modalDialog(
              title = "Bullpen @ Boshamer", footer = NULL, easyClose = T,
              h4("Enter Info (Password is 'gdtbath', no quotes.)"),
              selectInput(inputId = ns("loginEnter"), label = "Select Your Name",
                          choices = as.vector(getUsers(con))),
              textInput(inputId = ns("passEnter"), label = "Enter Password", value = ""),
              actionButton(inputId = ns("login"), label = "Login", class = "btn-primary")
            )
          )
        }
      })
      
      # Login any user attempting to add a note
      observeEvent(input$login, {
        if (loginUser(con, input$passEnter)) {
          values$gotLogin <- T
          values$login <- input$loginEnter
          removeModal()
        }
      })
      
      # Process successful login
      observe({
        if (values$gotLogin) {
          addNote(con, values$login, input$noteTitle, input$noteMessage, values$refdUIDs)
          allNotes <- getNotes(con)
          temp <- updateNotesTable(allNotes, input$pitcherSelectNV)
          output$notesTable <- renderDataTable({DT::datatable(temp, options = list(dom = 't', pageLength = -1),
                                                              rownames = F, selection = list(mode = "single"))})
          
          updateTextInput(session = session, "noteTitle", value = "")
          updateTextAreaInput(session = session, "noteMessage", value = "")
          values$refdUIDs <- c()
          values$keepRefs <- F
          # temp <- updateNotesTable(allNotes, input$pitcherSelectNV)
          # output$notesTable <- renderDataTable({DT::datatable(temp, options = list(dom = 't', pageLength = -1),
          #                                                     rownames = F, selection = list(mode = "single"))})
          
          values$refData <- NULL
          output$refSelectTable <- renderTable(data.frame())
        }
         values$gotLogin <- F
      })
      
      # View any notes that are selected, along with associated pitches
      observeEvent(input$notesTable_rows_selected, {
        allNotes <- getNotes(con)
        
        output$selectedNoteTitle <- renderText({ allNotes[input$notesTable_rows_selected,4]})
        output$selectedNoteMsg <- renderText({ allNotes[input$notesTable_rows_selected,5]})
        
        clickedID <-  allNotes[input$notesTable_rows_selected,1]
        refdPitches <- dbGetQuery(con, glue("SELECT * FROM Refs WHERE noteid = '{n}'", n = clickedID))$pitchuid
        
        vizData <- data.frame()
        for (uid in refdPitches) {
           vizData <- rbind( vizData,  data[ data$PlayID == uid,])
        }
        temp2 <- data.frame()
        if (nrow(vizData) > 0) {
          temp2 <- data.frame( vizData %>%
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
        }
        output$ascPitchesTable <- renderDataTable(DT::datatable(temp2,rownames = F))
      })
      
    }
  )
}