metricsUI <- function(id) {
  ns <- NS(id)
  column(width = 12,
    fluidRow(column(2,selectInput(inputId = ns("pitcherSelectMT"), label = "Player Select:",
                                  choices = NULL, multiple = FALSE)),
             column(3,dateRangeInput(inputId = ns("datesSelectMT"), label = "Select Sessions:",
                                  start = NULL, end = NULL, format = "yyyy-mm-dd")),
             column(2,selectInput(inputId = ns("metricSelect1"), label = "Choose Pitch Metric:",
                                  choices = NULL)),
             column(3,selectInput(inputId = ns("metricSelect2"), label = "Choose Another Pitch Metric:",
                                  choices = NULL)),
             column(2,selectInput(inputId = ns("pitchFilterMT"), label = "Include Pitch Types:",
                                  choices = NULL, multiple = TRUE))),
    fluidRow(column(12,plotOutput(ns("metricPlot1")))),
    br(),
    fluidRow(column(12,plotOutput(ns("metricPlot2")))),
    style="overflow-y:scroll;height=650px;"
  )
}

metricsServer <- function(id,data,livegame,gotData,pitcherList) {
  moduleServer(id,
               function(input,output,session) {
                 
                 values <- reactiveValues(metricChoice = c("RelSpeed", "SpinRate",
                                                           "SpinAxis", "RelHeight",
                                                           "RelSide", "Extension",
                                                           "HorzBreak", "InducedVertBreak",
                                                           "PlateLocHeight", "PlateLocSide"),
                                          testDataMT = NULL)
                 
                 observeEvent(gotData,{
                   options1 <- unique(data$Pitcher)
                   
                   updateSelectInput(inputId = "metricSelect1",
                                     choices = values$metricChoice)
                   
                   updateSelectInput(inputId = "pitcherSelectMT",
                                     choices = intersect(options1, pitcherList))
                 })
                 
                 observeEvent({input$pitcherSelectMT
                               input$datesSelectMT}, {
                   if (input$pitcherSelectMT != "" & length(input$datesSelectMT) > 0 & 
                       !any(is.na(input$datesSelectMT))) {
                     dates <- str_trim(strsplit(toString(seq.Date(from = as.Date(input$datesSelectMT[1]), 
                                       to = as.Date(input$datesSelectMT[2]),
                                       by = "day")),split = ",")[[1]])
                     
                     arsenal <- data %>% filter(Pitcher == input$pitcherSelectMT) %>%
                       filter(Date %in% dates) %>%
                       filter(TaggedPitchType != "Undefined") %>% select(TaggedPitchType)
                     
                     updateSelectInput(inputId = "pitchFilterMT",
                                       choices = as.vector(sort(unique(arsenal)$TaggedPitchType)),
                                       selected = as.vector(sort(unique(arsenal)$TaggedPitchType))[1])
                   }
                 })
                 
                 observeEvent(input$metricSelect1, {
                   updateSelectInput(inputId = "metricSelect2",
                                     choices = setdiff(values$metricChoice,
                                                       c(input$metricSelect1)))
                 })
                 
                 
                 observeEvent(input$pitcherSelectMT, {
                   if (gotData) {
                     
                     testDataMT1 <- prep(data %>% filter(Pitcher == input$pitcherSelectMT))
                     testDataMT1 <- cbind(testDataMT1,
                                          data.frame(Scenario = rep.int(1,nrow(testDataMT1))))
                     testDataMT2 <- prep(livegame %>% filter(Pitcher == input$pitcherSelectMT))
                     testDataMT2 <- cbind(testDataMT2,
                                          data.frame(Scenario = rep.int(0,nrow(testDataMT2))))
                     values$testDataMT <- rbind(testDataMT1[,c(values$metricChoice,
                                                               "Date","Pitcher","Scenario",
                                                               "TaggedPitchType")],
                                                testDataMT2[,c(values$metricChoice,
                                                               "Date","Pitcher","Scenario",
                                                               "TaggedPitchType")])
                     
                     dates <- sort(unique(values$testDataMT$Date),decreasing = F)
   
                     updateDateRangeInput(inputId = "datesSelectMT",
                                          start = dates[1])
                     
                   }
                 })
                 
                 observeEvent({input$datesSelectMT
                               input$pitchFilterMT}, {
                     if (!is.null(input$pitchFilterMT)) {
                   # if (!is.null(values$testDataMT) & !is.null(input$pitchFilterMT) &
                   #     !any(is.na(input$datesSelectMT))) {
                     # 
                     dates <- str_trim(strsplit(toString(seq.Date(from = as.Date(input$datesSelectMT[1]), 
                                                                   to = as.Date(input$datesSelectMT[2]),
                                                                   by = "day")),split = ",")[[1]])
                     temp <- values$testDataMT %>%
                       filter(TaggedPitchType %in% input$pitchFilterMT) %>%
                       filter(Date %in% dates)
                     
                     output$metricPlot1 <- renderPlot({timeGraphs(temp,
                                                                  input$pitcherSelectMT,
                                                                  dates,
                                                                  input$metricSelect1)})
                     
                     output$metricPlot2 <- renderPlot({timeGraphs(temp,
                                                                  input$pitcherSelectMT,
                                                                  dates,
                                                                  input$metricSelect2)})
                   }
                 })
                 
               })
}