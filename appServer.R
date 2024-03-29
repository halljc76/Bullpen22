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
                           pitcherList = readLines("./2023PitcherList.txt"),
                           vids = NULL, vidsMatch = NULL)
  
  observe({
    if (!values$gotData) {
      values$data <- getData() %>% filter(PitchSession != "Warmup") %>% 
        filter(Date >= "2022-09-05")
      values$data$Pitcher <- ifelse( values$data$Pitcher == 'Carlson, Maxwell', 
                                     'Carlson, Max',  ifelse(values$data$Pitcher == "Pry, Nick",
                                                             "Pry, Nik",values$data$Pitcher))
      
      forRecruiting <- "Carlson, Max"
      values$data <- values$data[order(values$data$Pitcher == "Carlson, Max",decreasing = T),]
      
      # values$data$Date <- strftime(as.Date(values$data$Date, format = "%m/%d/%Y"),
      #                              format = "%Y-%m-%d")
      values$gotData <- T
      values$livegame <- getOurGames() 
      values$livegame$Pitcher <- ifelse(values$livegame$Pitcher == "Pry, Nick",
                                        "Pry, Nik",values$livegame$Pitcher)
      
      values$livegame <- applyArsenals(values$livegame, values$data)[[1]]
      values$data <- applyArsenals(values$livegame, values$data)[[2]]
      
      values$allNotes <- getNotes(con)
      
      vidsSide <- getVids(back = F)
      vidsBack <- getVids()
      
      values$vids <- data.frame()
      sideLinks <- c()
      backLinks <- c()
      
      for (i in 2:length(vidsSide)) {
        tstring <- paste0("https://uncbullpen.s3.us-east-1.amazonaws.com/",
                          vidsSide[i])
        sideLinks <- append(sideLinks,
                            paste0("<a href='",URLdecode(tstring),"' target='_blank'>","Link","</a>"))
        vidsSide[i] <- HTML(substr(vidsSide[i], 16, nchar(vidsSide[i])))
      }
      for (i in 2:length(vidsBack)) {
        tstring <- paste0("https://uncbullpen.s3.us-east-1.amazonaws.com/",
                          vidsBack[i])
        backLinks <- append(backLinks,
                            paste0("<a href='",URLdecode(tstring),"' target='_blank'>","Link","</a>"))
        vidsBack[i] <- HTML(substr(vidsBack[i], 16, nchar(vidsBack[i])))
      }
      
      sideLinks <- data.frame(sideLinks, vidsSide[2:length(vidsSide)])
      backLinks <- data.frame(backLinks, vidsBack[2:length(vidsBack)])
      colnames(sideLinks) <- c("videoSide", "VideoName")
      colnames(backLinks) <- c("videoBack", "VideoName")
      
      sideMatch <- left_join(sideLinks, getMatches(), by = c("VideoName"))
      backMatch <- left_join(backLinks, getMatches(), by = c("VideoName"))
      
      temp2a <- left_join(getMatches(), values$data %>% select(Pitcher, PlayID), 
                      by = c("PlayID")) %>% left_join(
                        y = sideMatch, by = c("VideoName", "PlayID")
                      ) %>% filter(!is.na(videoSide))
      
   
      temp2b <- left_join(getMatches(), values$data %>% select(Pitcher, PlayID), 
                           by = c("PlayID")) %>% left_join(
                             y = backMatch, by = c("VideoName", "PlayID")
                           ) %>% filter(!is.na(videoBack))
      
      values$vids <- full_join(temp2a, temp2b, by = c("PlayID")) %>% select(PlayID, videoSide, videoBack)
    
      values$data <- left_join(
          values$data, values$vids, by = c("PlayID")
      )
      
      
      for (i in 1:nrow(values$data)) {
        if (!is.na(values$data$Tilt[i]) & gregexpr(":",values$data$Tilt[i])[[1]][1] == -1) {
          time <- as.numeric(values$data$Tilt[i])
          if (!is.na(time)) {
            timeF <- paste0(
              toString(floor(time / 3600)), ":",
              c("00","15","30","45")[which.min(sapply(c(0,15,30,45),
                                                      function(x) {((time - (3600 * floor(time/3600))) / 3600) * 60 - x}
              ))])
            values$data$Tilt[i] <- substr(timeF,1,4)
          }
        }
      }
      
      # print(values$data %>% select(PlayID, VideoName, videoSide, videoBack))
      
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