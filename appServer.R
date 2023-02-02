server <- function(input, output, session) {
  
  con <- dbConnect(RPostgres::Postgres(), dbname = Sys.getenv("DB"),
                   host=Sys.getenv("HOSTDB"), port=26257,
                   user=Sys.getenv("COCKROACH_USER"),
                   password=Sys.getenv("COCKROACH_PASSWORD"), sslmode = NULL,
                   options = "--cluster=bullpen-notes-4213")
  
  values <- reactiveValues(gotLogin = F, gotData = F, data = NULL, login = NULL,
                           testDataPM = NULL, testDataMT = NULL,
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
      values$data <- getData() %>% filter(PitchSession != "Warmup") %>% 
        filter(Date >= "2022-09-05")
      values$data$Pitcher <- ifelse( values$data$Pitcher == 'Carlson, Maxwell', 
                                     'Carlson, Max',  ifelse(values$data$Pitcher == "Pry, Nick",
                                                             "Pry, Nik",values$data$Pitcher))
      
      forRecruiting <- "Carlson, Max"
      values$data <- values$data[order(values$data$Pitcher == "Carlson, Max",decreasing = T),]
      values$data <- applyArsenals(values$data)
      
      # values$data$Date <- strftime(as.Date(values$data$Date, format = "%m/%d/%Y"),
      #                              format = "%Y-%m-%d")
      values$gotData <- T
      values$livegame <- getOurGames() %>% applyArsenals()
      values$livegame$Pitcher <- ifelse(values$livegame$Pitcher == "Pry, Nick",
                                        "Pry, Nik",values$livegame$Pitcher)
      
      values$allNotes <- getNotes(con)
      
      # values$whiffThresh <- calThresh(values$livegame, "Whiff")
      # values$csThresh <- calThresh(prep(values$livegame))
    }
  })
  
  homepageServer("homepage", values$data,values$pitcherList,values$gotData)
  notesServer("notes",values$allNotes,values$data,
              values$gotData,values$pitcherList,con)
  svgServer("sessionvsgames",values$data,values$livegame,values$gotData,
            values$pitcherList)
  metricsServer("metrics",values$data,values$livegame,values$gotData,values$pitcherList)
}