homepageUI <- function(id) {
  
  ns <- NS(id)
  
  column(width = 12,
         tags$style(type='text/css', ".selectize-input { font-size: 18px; line-height: 18px;} 
                                  .selectize-dropdown { font-size: 18px; line-height: 18px; }"),
         fluidRow(
         selectInput(inputId = ns("pitcherSelectHP"), label = h4("Player Select:"),
                     choices = NULL, multiple = FALSE),
         # uiOutput(ns("mostRecentDate"))
         ),
         fluidRow(
           # uiOutput(ns("summLastDisplay")),
           h4("Last Bullpen vs. All Bullpens This Season:"),
           uiOutput(ns("summSznDisplay"))
         ),
         style = "overflow-y:scroll;
                       height:550px;
                       border-radius:15px;
                       padding-right:40px;"
  )
}


homepageServer <- function(id,data,pitcherList,
                           gotData) {
  moduleServer(
    id, 
    function(input, output, session) {
      observeEvent(input$pitcherSelectHP, {
        ns <- session$ns
        if (gotData && input$pitcherSelectHP != "") {
          testDataHP <- data %>% filter(Pitcher == input$pitcherSelectHP)
          # 
          # temp <- summaryStats(testDataHP,
          #                      input$pitcherSelectHP,
          #                      unique(testDataHP$Date))
          # sznTable <- kable(temp, booktabs=T) %>%
          #   kable_styling(font_size = 13)
          # 
          # output$summSznDisplay <- renderUI({HTML(sznTable)})
          # 
          # 
          # output$mostRecentDate <- renderUI({h4(paste0("Most Recent Bullpen Session: ", max(testDataHP$Date)))})
          # 
          # temp2 <- summaryStats(testDataHP %>% filter(Date == max(Date)),
          #                       input$pitcherSelectHP, max(testDataHP$Date))
          # 
          # commonPTs <- intersect(temp$`Pitch Type`, temp2$`Pitch Type`)
          # idx <- which(temp2$`Pitch Type` %in% commonPTs)
          # veloAbvAvg <- temp2$`Avg. Velo`[idx] - temp$`Avg. Velo`[idx]
          # spinAbvAvg <- temp2$`Spin Rate`[idx] - temp$`Spin Rate`[idx]
          # 
          # colorFunction<- colorRampPalette(colors = c("dodgerblue","lightskyblue", "white", "indianred1", "tomato2"))
          # 
          # lastTable <- kable(temp2,booktabs=T) %>%
          #   kable_styling(font_size = 13) %>%
          #   column_spec(3, color = "black",
          #               background = spec_color2(veloAbvAvg, palette = colorFunction(50), scale_from = c(-2,2))) %>%
          #   column_spec(4, color = "black",
          #               background = spec_color2(spinAbvAvg, palette = colorFunction(50), scale_from = c(-200,200)))
          # 
          # 
          # output$summLastDisplay <-renderUI({HTML(lastTable)})
          
          output$summSznDisplay <- renderUI({HTML(SpecificTable(testDataHP %>% filter(Date == max(Date)), 
                                                                testDataHP, input$pitcherSelectHP,
                                                                forHomepage = T))})
        }
      })
      
      observeEvent(gotData,{
        options1 <- unique(data$Pitcher)
        
        updateSelectInput(inputId = "pitcherSelectHP",
                          choices = intersect(options1, pitcherList))
      })
    }
  )
}