source("global.R")
sourceAll("Pages")
source("./appUI.R")
source("./appServer.R")
source("./helpers.R")
source("./dbFuncs.R")
source("./videoFuncs.R")
shinyApp(ui = ui,server = server)





