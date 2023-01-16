metricsUI <- function(id) {
  ns <- NS(id)
  column(width = 12,
    fluidRow(column(3,selectInput(inputId = ns("pitcherSelectMT"), label = "Player Select:",
                                  choices = NULL, multiple = FALSE)),
             column(3,selectInput(inputId = ns("datesSelectMT"), label = "Select Sessions:",
                                  choices = NULL, multiple = TRUE)),
             column(3,selectInput(inputId = ns("metricSelect1"), label = "Choose Pitch Metric:",
                                  choices = NULL)),
             column(3,selectInput(inputId = ns("metricSelect2"), label = "Choose Another Pitch Metric:",
                                  choices = NULL))),
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
                                     choices = sort(intersect(options1, pitcherList)))
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
                 
               })
}