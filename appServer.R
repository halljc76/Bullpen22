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
      values$data <- getData()
      # values$data$Date <- strftime(as.Date(values$data$Date, format = "%m/%d/%Y"),
      #                              format = "%Y-%m-%d")
      values$gotData <- T
      print(values$data)
      values$livegame <- getOurGames()
      
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