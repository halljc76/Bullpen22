svgUI <- function(id) {
  ns <- NS(id)
  
  tabsetPanel(
    tabPanel(
      title = h5("Summary"),
      selectInput(inputId = ns("pitcherSelectSG"), label = h4("Player Select:"),
                  choices = NULL, multiple = FALSE),
      h3("Bullpen vs Games Overview"),
      uiOutput(ns("svgSummary"))
    ),
    tabPanel(title = h5("Recent Stats"),
             column(width = 12,
                    selectInput(inputId = ns("pitcherSelectSG2"), label = h4("Player Select:"),
                                choices = NULL, multiple = FALSE),
                    column(width = 7,
                           h4("Before Last Live Appearance"),
                           DT::dataTableOutput(ns("beforeLAView")),
                           uiOutput(ns("mostRecentLA")),
                           DT::dataTableOutput(ns("LAView")),
                           h4("After Last Live Appearance"),
                           DT::dataTableOutput(ns("afterLAView")),
                           style = "overflow-y:scroll;height:500px;"),
                    column(width = 5,
                           h3("Pitch Outcomes From Last Live Appearance"),
                           h4("Versus LHH"),
                           DT::dataTableOutput(ns("LALHH")),
                           h4("Versus RHH"),
                           DT::dataTableOutput(ns("LARHH"))),
                           style = "overflow-y:scroll;height:500px;")
    )
  )
}

svgServer <- function(id,data,livegame,
                      gotData,pitcherList) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      # Load the dropdown 
      observeEvent(gotData,{
        options1 <- unique(data$Pitcher)
        options2 <- unique(livegame$Pitcher)
        
        updateSelectInput(inputId = "pitcherSelectSG",
                          choices = unique(intersect(intersect(
                            livegame$Pitcher,
                            data$Pitcher),
                            pitcherList)
                          ))
        updateSelectInput(inputId = "pitcherSelectSG2",
                          choices = unique(intersect(intersect(
                            livegame$Pitcher,
                            data$Pitcher),
                            pitcherList)
                          ))
        
        
      })
      
      # Determine the tables for before last appearance, the last appearance, and after
      observeEvent(input$pitcherSelectSG2, {
        if (!is.null(livegame)) {
          liveapps <- livegame %>% filter(Pitcher == input$pitcherSelectSG2)
          appearances <- unique(liveapps$Date)
          appearances <- appearances[which(nchar(appearances) > 0)]
          lastApp <- sort(appearances, decreasing = T)[length(appearances)]
          #print(lastApp)
          lastAppDF <- liveapps %>% filter(Date == lastApp)
          
          
          output$mostRecentLA <- renderUI({h4(paste0("Last Live Appearance: ", unique(lastApp)[1], " Versus ", unique(lastAppDF$BatterTeam)))})
          
          if (!is.null(lastApp)) {

            # The *session* before the last appearance
            seshBefore <- data %>% filter(Pitcher == input$pitcherSelectSG2) %>%
              filter(Date < lastApp) %>% filter(Date == max(Date))
            #print(seshBefore)

            # The *session* after the last appearance
            seshAfter <- data %>% filter(Pitcher == input$pitcherSelectSG2) %>%
              filter(Date > lastApp) %>% filter(Date == min(Date))

            # Before Last Live App
            if (nrow(seshBefore) > 0) {
              output$beforeLAView <- renderDataTable({DT::datatable(summaryStats(seshBefore, flag = F),
                                                                    options = list(dom = 't'),rownames = F)})
            } else {
              output$beforeLAView <- renderDataTable({DT::datatable(data.frame("No Data Before Last Live Appearance"),
                                                                    options = list(dom = 't'),rownames = F)})
            }

            # Further-broken-down the last live appearance
            if (nrow(lastAppDF) > 0) {
              output$LAView <- renderDataTable({DT::datatable(summaryStats(lastAppDF, flag = F),
                                                              options = list(dom = 't'),rownames = F)})
              output$LALHH <- renderDataTable({DT::datatable(pitchOutcomes(lastAppDF, T),options = list(dom = 't'),rownames = F)})
              output$LARHH <- renderDataTable({DT::datatable(pitchOutcomes(lastAppDF, F),options = list(dom = 't'),rownames = F)})
            }

            # After Last Live App
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
      
      observeEvent(input$pitcherSelectSG, {
        if (gotData) {
          output$svgSummary <- renderUI({HTML(SpecificTable(data,livegame,input$pitcherSelectSG))})
        }
      })
      
    }
  )
}