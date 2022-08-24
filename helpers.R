getData <- function() {
  ret <- data.frame()
  keys <- unique(data.table::rbindlist(get_bucket(bucket = "uncbullpen", prefix = "Data/"))$Key)
  for (key in keys) {
    if (gregexpr(".csv", key)[[1]][1] != -1) {
      temp <- s3read_using(FUN = read.csv, bucket = "uncbullpen", object = key)
      colnames(temp)[1] <- "PitchNo"
      ret <- rbind(ret, temp)
    }
  }
  return(ret)
}

getModelTrain <- function() {
    ret <- data.frame()
    keys <- unique(data.table::rbindlist(get_bucket(bucket = "uncbullpen", prefix = "Train/"))$Key)
    for (key in keys) {
      if (gregexpr(".csv", key)[[1]][1] != -1) {
        temp <- s3read_using(FUN = read.csv, bucket = "uncbullpen", object = key)
        ret <- rbind(ret, temp[,2:ncol(temp)])
      }
    }
    return(ret)
}

loginUser <- function(conn, user, pass) {
  # Returns NA --> Username not found
  # Returns FALSE --> Wrong password
  # Returns TRUE --> Authenticated User

  pw = getPassword(user)
  print(pw)
  return(pw == pass)

  #return(unname(pass == dbGetQuery(conn, glue(
  #"SELECT L.Password FROM Logins L
  # WHERE L.Login = '{name}'", name = user)))[1])
}

getNotes <- function() {


  return(read.csv("./Notes/notes.txt", header = TRUE))
}

summaryStats <- function(data, pitcher, dates) {
  temp <- data %>% filter(Pitcher == pitcher) %>% filter(Date %in% dates) %>%
    filter(TaggedPitchType != "Undefined") %>% select(ZoneSpeed, RelSpeed, SpinRate, TaggedPitchType, Tilt)
  temp <- na.omit(temp)

  tilts <- averageTilt(temp)

  ret <- temp %>% group_by(TaggedPitchType) %>%
           summarize(Count = n(),
                     Velo = paste0(toString(round(min(ZoneSpeed), 0)), "-",
                                   toString(round(max(ZoneSpeed)), 0)),
                     AvgZoneVelo = round(mean(ZoneSpeed), 1),
                     AvgReleaseVelo = round(mean(RelSpeed), 1),
                     AvgSpeedDrop = round(mean(RelSpeed - ZoneSpeed), 1))
  ret <- left_join(ret, tilts, by = c("TaggedPitchType" = "TaggedPitchType"))
  return(ret)
}

rowNmbrFact <- function(data, factor) {
  v <- vector("numeric",length = length(unique(factor)))
  names(v) <- unique(factor)

  ret <- c()
  for (i in 1:nrow(data)) {
    v[[factor[i]]] <- v[[factor[i]]] + 1
    ret <- append(ret, v[[factor[i]]])
  }

  data$Row <- ret
  return(data)
}

averageTilt <- function(df) {
  ret <- data.frame()

  for (pt in unique(df$TaggedPitchType)) {
    if (pt != "Undefined") {
      df2 <- subset(df, df$TaggedPitchType == pt)
      m <- c()
      for (i in 1:nrow(df2)) {
        tilt <- df2$Tilt[i]
        if (!is.na(tilt) && !is.null(tilt)) {
          hr <- strtoi(substr(tilt, 1, gregexpr(":", tilt)[[1]][1] - 1))
          min <- strtoi(substr(tilt, gregexpr(":", tilt)[[1]][1] + 1, nchar(tilt)))

          m[i] <- (hr * 60) + min
        }
      }

      m <- na.omit(m)

      nearhr <- round(mean(m) / 60)
      nearmin <- round(
        round(mean(m) - round(mean(m) / 60) * 60) / 15) * 15

      if (nearmin < 0) {
        nearhr <- nearhr - 1
      }

      nearhr <- toString(nearhr)
      if (nearmin == 0) {
        nearmin <- '00'
      } else {
        if (nearmin >= 52.5 || nearmin <= 7.5) {
          nearmin <- '00'
        } else if (nearmin > 7.5 && nearmin < 22.5) {
          nearmin <- '15'
        } else if (nearmin > 22.5 && nearmin < 37.5) {
          nearmin <- '30'
        } else {
          nearmin <- '45'
        }
      }

      ret <- rbind(ret, c(pt, paste(nearhr, ":", nearmin, sep = "")))
    }
  }

  colnames(ret) <- c("TaggedPitchType", "Avg. Tilt")
  return(ret)
}


timeGraphs <- function(df, pitcher, dates, response) {
  # Make this factored by "PT, Date" together

  df <- df %>% filter(Pitcher == pitcher) %>%
    filter(TaggedPitchType != "Undefined") %>%
    filter(!is.na(ZoneSpeed))

  ptWithDate <- c()
  for (i in 1:nrow(df)) {
    ptWithDate <- append(ptWithDate,
                         paste(df$TaggedPitchType[i], substr(df$Date[i], 1, 4)))
  }
  df$ptWithDate <- ptWithDate

  df <- rowNmbrFact(df, df$ptWithDate)
  df <- df %>% filter(Date %in% dates)

  if (response %in% c("ZoneSpeed", "RelSpeed")) {
    suffix <- "MPH"
  } else if (response %in% c("SpinRate")) {
    suffix <- "RPM"
  } else if (response %in% c("SpinAxis", "HorzRelAngle", "VertRelAngle")) {
    suffix <- "deg"
  } else if (response %in% c("HorzBreak", "InducedVertBreak", "PlateLocSide",
                             "PlateLocHeight", "RelHeight", "RelSide")) {
    suffix <- "in."
  } else if (response %in% c("Extension")) {
    suffix <- "ft."
  }

  plot_ly(data = df %>% group_by(df$ptWithDate),
          x = ~df$Row,
          y = ~df[,response],
          type = "scatter",
          color = ~df$ptWithDate,
          mode = "lines+markers",
          hoverinfo = "text",
          text = ~paste0(df$Row, ": ", round(df[,response], 1), " ", suffix)) %>%
    layout(xaxis = list(
      tickvals = ~df$Row,
      tickangle = 0,
      title = "Pitch #"),
      yaxis = (
        list(title = paste0(response, "(", suffix, ")"))
      )
    )
}

useModels <- function(test, pitcherSide, batterSide, whiffOrCS) {
  modDir <- ifelse(whiffOrCS, "WhiffModels", "CSModels")
  modStr <- paste0(ifelse(pitcherSide == "Left", "l", "r"),
               ifelse(batterSide == "Left", "l", "r"))

  fbNBC <- readRDS(paste0("./", modDir, "/fbMod", modStr, ".rds"))
  chNBC <- readRDS(paste0("./", modDir, "/chMod", modStr, ".rds"))
  brNBC <- readRDS(paste0("./", modDir, "/brMod", modStr, ".rds"))

  pWhiff <- c()
  predWhiff <- c()
  for (i in 1:nrow(test)) {
    pt <- test$updPT[i]
    if (pt == "FB") {
      pred <- runNBC(fbNBC, test[i,])[1]
      predC <- runNBC(fbNBC, test[i,], class = T)
    } else if (pt == "CH") {
      pred <- runNBC(chNBC, test[i,])[1]
      predC <- runNBC(chNBC, test[i,], class = T)
    } else {
      pred <- runNBC(brNBC, test[i,])[1]
      predC <- runNBC(brNBC, test[i,], class = T)
    }

    pWhiff <- append(pWhiff, 1 - pred)
    predWhiff <- append(predWhiff, as.numeric(predC) - 1)
  }
  #tryCatch(
  #  expr = {
  #    pWhiff <- rfUtilities::probability.calibration(y = predWhiff, p = pWhiff,
  #                                      regularization = T)},
  #  warning = function(w) {
  #    pWhiff <- pWhiff
  #  },
  #  error = function(e) {
  #    pWhiff <- pWhiff
  #  }
  #)
  return(pWhiff)
}

prep <- function(data) {
  sz <- data.frame(xmin = -0.95,
                   xmax = 0.95,
                   ymin = 1.525,
                   ymax = 3.316)

  data <- data %>% filter(PlateLocSide > sz$xmin - 0.25) %>%
    filter(PlateLocSide < sz$xmax + 0.25) %>%
    filter(PlateLocHeight > sz$ymin - 0.25) %>%
    filter(PlateLocHeight < sz$ymax + 0.25)

  data <- data %>% filter(!(TaggedPitchType %in% c("Undefined", "", NA)))

  data$updPT <- ifelse(data$TaggedPitchType == "Fastball", "FB",
                       ifelse(data$TaggedPitchType == "ChangeUp", "CH",
                              ifelse(data$TaggedPitchType == "Curveball", "BR",
                                     ifelse(data$TaggedPitchType == "Slider", "BR",
                                            ifelse(data$TaggedPitchType == "Splitter", "CH",
                                                   ifelse(data$TaggedPitchType == "Cutter", "BR",
                                                      ifelse(data$TaggedPitchType == "Sinker", "BR",
                                                          data$TaggedPitchType)))))))

  return(data)
}

runNBC <- function(nbc, data, class = F) {
  if (class) {
    return(predict(nbc, newdata = data, type = "class"))
  } else {
    return(predict(nbc, newdata = data, type = "raw"))
  }
}

makeWhiffPlot <- function(data, batterSide, minWF, maxWF) {
  if (batterSide == "Left") {
    data <- data %>% filter(minWF <= pWhiffL) %>% filter(pWhiffL <= maxWF)
  } else {
    data <- data %>% filter(minWF <= pWhiffR) %>% filter(pWhiffR <= maxWF)
  }

  fig <- plot_ly(
      type = "scatter",
      mode = "markers",
      x = ~data$PlateLocSide,
      y = ~data$PlateLocHeight,
      color = ~as.factor(data$TaggedPitchType),
      hoverinfo = "text",
      text = ~paste("Whiff% vs. LHH: ", round(100 * data$pWhiffL, 2),
                    "\n Whiff% vs. RHH: ", round(100 * data$pWhiffR, 2),
                    "\n Velo: ", round(data$ZoneSpeed, 1), " MPH",
                    "\n Spin: ", round(data$SpinRate, 1), " RPM",
                    "\n Tilt: ", data$Tilt, sep = "")
    ) %>% layout(
      shapes = list(type = "rect",
                    line = list(color = "blue"),
                    x0 = -0.95,
                    x1 = 0.95,
                    xref = "x",
                    y0 = 1.525,
                    y1 = 3.316,
                    yref = "y")
    )
  return(fig)
}

makeCSPlot <- function(data, batterSide, minCS, maxCS) {
  if (batterSide == "Left") {
    data <- data %>% filter(minCS <= pCSL) %>% filter(pCSL <= maxCS)
  } else {
    data <- data %>% filter(minCS <= pCSR) %>% filter(pCSR <= maxCS)
  }

  fig <- plot_ly(
    type = "scatter",
    mode = "markers",
    x = ~data$PlateLocSide,
    y = ~data$PlateLocHeight,
    color = ~as.factor(data$TaggedPitchType),
    hoverinfo = "text",
    text = ~paste("CS% vs. LHH: ", round(100 * data$pCSL, 2),
                  "\n CS% vs. RHH: ", round(100 * data$pCSR, 2),
                  "\n Velo: ", round(data$ZoneSpeed, 1), " MPH",
                  "\n Spin: ", round(data$SpinRate, 1), " RPM",
                  "\n Tilt: ", data$Tilt, sep = "")
  ) %>% layout(
    shapes = list(type = "rect",
                  line = list(color = "blue"),
                  x0 = -0.95,
                  x1 = 0.95,
                  xref = "x",
                  y0 = 1.525,
                  y1 = 3.316,
                  yref = "y")
  )
  return(fig)
}

