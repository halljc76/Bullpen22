#' @title Collect Bullpen Session Data
#' @description This function uses our AWS system to grab all stored bullpen sessions
#'              from the 'Data' folder of the 'uncbullpen' S3 bucket.
#'              NOTE: The function assumes the files are all stored as .csv.
getData <- function() {
  ret <- data.frame() # Output df

  # Get all the unique filenames in the bucket
  keys <- unique(data.table::rbindlist(get_bucket(bucket = "uncbullpen", prefix = "Data/"))$Key)

  # For each file...
  for (key in keys) {
    # Ensure it's a csv
    if (gregexpr(".csv", key)[[1]][1] != -1) {
      # Read it in
      temp <- s3read_using(FUN = read.csv, bucket = "uncbullpen", object = key)
      colnames(temp)[1] <- "PitchNo" # Fix a little read-in error
      ret <- rbind(ret, temp) # Bind everything
    }
  }
  return(ret) # Return the data
}

#' @title Collect Live Game Data
#' @description This function uses our AWS system to grab all stored bullpen sessions
#'              from the 'UNCLiveGame' folder of the 'uncbullpen' S3 bucket.
#'              NOTE: The function assumes the files are all stored as .fst.
#'              Consider using the read.csv and write.fst functions in base R
#'              to convert any necessary .csv files to .fst. (Use in conjunction
#'              with list.files in order to do this for a large host of files.)
getOurGames <- function() {
  ret <- data.frame() # Output df
  # Get filenames
  keys <- unique(data.table::rbindlist(get_bucket(bucket = "uncbullpen", prefix = "UNCLiveGame/"))$Key)

  # For each file...
  for (key in keys) {
    # Ensure this is an .fst file
    if (gregexpr(".fst", key)[[1]][1] != -1) {
      # Read in
      temp <- s3read_using(FUN = read.fst, bucket = "uncbullpen", object = key)
      ret <- rbind(ret, temp) # Bind together
    }
  }
  return(ret) # Output live game df
}

#' @title Get Summary Statistics
#' @description Given a dataframe of appearance data or sessions, obtain the
#'              relevant summary statistics: RelSpeed, SpinRate, TaggedPitchType,
#'              InducedVertBreak, HorzBreak, RelHeight, Tilt.
#' @param data The relevant data (usually just a subset of available TM data).
#' @param pitcher The pitcher for which we are determining these stats (usually given in-app).
#' @param dates A vector of dates for which we filter the data (usually given in-app if needed).
#' @param flag A boolean value that allows the function to be used for getting summary stats
#'             before some day, rather than having the program find those days. (Specified in backend code)
summaryStats <- function(data, pitcher = NA, dates = NA, flag = T) {
  if (flag) {
    # Get relevant data from appearances/sessions
    temp <- data %>% filter(Pitcher == pitcher) %>% filter(Date %in% dates) %>%
      filter(TaggedPitchType != "Undefined") %>% select(RelSpeed, SpinRate, TaggedPitchType, InducedVertBreak,
                                                        HorzBreak, RelHeight, Tilt)
  } else {
    # Get relevant data
    temp <- data %>% filter(TaggedPitchType != "Undefined") %>%
      select(RelSpeed, SpinRate, TaggedPitchType, InducedVertBreak, HorzBreak, RelHeight, Tilt)
  }

  # Remove NAs from calcs to force answers to be numeric
  temp <- na.omit(temp)

  tilts <- averageTilt(temp)

  # use dplyr functions to quickly create many of the metrics
  ret <- temp %>% group_by(TaggedPitchType) %>%
    summarize(Count = n(),
              `Max Velo` = round(max(RelSpeed), 2),
              `Avg. Velo` = round(mean(RelSpeed), 2),
              `Spin Rate` = round(mean(SpinRate), 2),
              `Avg. Vert. Brk` = round(mean(InducedVertBreak), 2),
              `Avg. Horz. Brk` = round(mean(HorzBreak), 2),
              `Avg. Rel. Height` = round(mean(RelHeight), 2))

  # Join df with tilts created in averageTilt function
  ret <- left_join(ret, tilts, by = c("TaggedPitchType" = "TaggedPitchType"))
  # Make the names pretty
  colnames(ret) <- c("Pitch Type", "Count", "Max. Velo",
                     "Avg. Velo", "Spin Rate", "Avg. Vert. Brk",
                     "Avg. Horz. Brk", "Avg. Rel. Height", "Avg. Tilt")
  return(ret) # Return df
}

#' @title Determine the i-th Observation of Each Factor in DF (DEPRECATED)
#' @description This function does not appear to be used in the app. It was at
#'              one point... so might be best to leave it.
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

#' @title Computing Average Tilts
#' @description Because tilts are strings in TM Data, averaging them is hard.
#'              This function takes the tilets and determines, to the nearest
#'              quarter-hour, the average tilt. Used in summaryStats function.
averageTilt <- function(df) {
  ret <- data.frame()

  for (pt in unique(df$TaggedPitchType)) {
    if (pt != "Undefined") {
      df2 <- subset(df, df$TaggedPitchType == pt)
      m <- c()
      for (i in 1:nrow(df2)) {
        tilt <- gsub(".* ", "", df2$Tilt[i])
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


#' @title Plot Metrics Over Time
#' @description Given a dataframe of a pitcher's session, plot metrics over time.
#'              (This is likely one of the things we want to optimize. This code is from
#'              Winter of 2020.)
#' @param df Dataframe of interest that contains session data
#' @param pitcher Pitcher that we want to condition on
#' @param dates Dates of interest
#' @param response The metric we want to plot
timeGraphs <- function(df, pitcher, dates, response) {

  df <- df %>% filter(Pitcher == pitcher) %>%
    filter(TaggedPitchType != "Undefined") %>%
    filter(!is.na(ZoneSpeed))

  ptWithDate <- c()
  for (i in 1:nrow(df)) {
    ptWithDate <- append(ptWithDate,
                         paste(df$TaggedPitchType[i], substr(df$Date[i], 6, 10)))
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

#' @title Use Already-Crated Naive Bayes Models
#' @description For the NBC Performance Models, this function uses them!
#'              (May be optimized in future if we decided to switch to
#'               Random Forests or a Neural Network approach.)
#' @param test A 'test' dataframe for which we want to predict probabilities.
#' @param pitcherSide String informing if Pitcher is Righty/Lefty
#' @param batterSide 'pitcherSide'... but for hitters.
#' @param whiffOrCS Boolean that lets code know which models to use
useModels <- function(test, pitcherSide, batterSide, whiffOrCS, thresh) {
  modDir <- ifelse(whiffOrCS, "WhiffModels", "CSModels") # Which models do I use?
  modStr <- paste0(ifelse(pitcherSide == "Left", "l", "r"), # Which handedness do I consider?
                   ifelse(batterSide == "Left", "l", "r"))

  fbNBC <- readRDS(paste0("./", modDir, "/fbMod", modStr, ".rds")) # Get me FB mod
  chNBC <- readRDS(paste0("./", modDir, "/chMod", modStr, ".rds")) # Get me CH mod
  brNBC <- readRDS(paste0("./", modDir, "/brMod", modStr, ".rds")) # Get me BR mod

  pWhiff <- c()
  predWhiff <- c()

  # Predict accordingly using the runNBC function to run the models
  for (i in 1:nrow(test)) {
    pt <- test$updPT[i]
    if (pt == "FB") {
      pred <- runNBC(fbNBC, test[i,])[1]
      #predC <- runNBC(fbNBC, test[i,], class = T)
    } else if (pt == "CH") {
      pred <- runNBC(chNBC, test[i,])[1]
      #predC <- runNBC(chNBC, test[i,], class = T)
    } else {
      pred <- runNBC(brNBC, test[i,])[1]
      #predC <- runNBC(brNBC, test[i,], class = T)
    }

    pWhiff <- append(pWhiff, 1 - pred)
    #predWhiff <- append(predWhiff, as.numeric(predC) - 1)
  }

  for (p in pWhiff) {
    if (p >= thresh) {
      predWhiff <- append(predWhiff, 1)
    } else {
      predWhiff <- append(predWhiff, 0)
    }
  }

  tryCatch(
    expr = {pWhiff <- probability.calibration(y = predWhiff, p = pWhiff,
                                              regularization = T)},
    error = function(e) {pWhiff <- pWhiff}
  )

  return(pWhiff)
}

#' @title Prepare Data for Modeling
#' @description To assist the modeling process, we generally only consider
#'              pitches within a certain range of the plate. This function
#'              filters other pitches out, then categorizes the TaggedPitchType
#'              so that the modeling process goes even smoother.
prep <- function(data) {
  sz <- data.frame(xmin = -1.2,
                   xmax = 1.2,
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

#' @title Run an NBC Model on Data
#' @description Predict using an NBC data either the outcome 'class' or probability 'raw.'
runNBC <- function(nbc, data, class = F) {
  if (class) {
    return(predict(nbc, newdata = data, type = "class"))
  } else {
    return(predict(nbc, newdata = data, type = "raw"))
  }
}

#' @title Make a Strikezone Plot Filtered by Predicted Whiff Prob.
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
                  "\n Velo: ", round(data$RelSpeed, 1), " MPH",
                  "\n Spin: ", round(data$SpinRate, 1), " RPM",
                  "\n Tilt: ", data$Tilt, sep = "")
  ) %>% layout(
    shapes = list(type = "rect",
                  line = list(color = "blue"),
                  x0 = -1.2,
                  x1 = 1.2,
                  xref = "x",
                  y0 = 1.525,
                  y1 = 3.316,
                  yref = "y"),
    xaxis = list(title = "Plate Side"),
    yaxis = list(title = "Plate Height")
  )
  return(fig)
}

#' @title Make a Strikezone Plot Filtered by Predicted Called-Strike Prob.
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
                  "\n Velo: ", round(data$RelSpeed, 1), " MPH",
                  "\n Spin: ", round(data$SpinRate, 1), " RPM",
                  "\n Tilt: ", data$Tilt, sep = "")
  ) %>% layout(
    shapes = list(type = "rect",
                  line = list(color = "blue"),
                  x0 = -1.2,
                  x1 = 1.2,
                  xref = "x",
                  y0 = 1.525,
                  y1 = 3.316,
                  yref = "y"),
    xaxis = list(title = "Plate Side"),
    yaxis = list(title = "Plate Height")
  )
  return(fig)
}

revRow <- function(df) {
  ret <- data.frame()
  for (i in 1:nrow(df)) {
    ret <- rbind(ret, df[nrow(df) - i + 1,])
  }
  return(ret)
}

#' @title Shorten a Pitch Reference for Ease-of-Viewing in App
shortenRef <- function(refs) {
  ret <- data.frame()
  for (i in 1:nrow(refs)) {
    name <- refs$Pitcher[i]
    lastName <- substr(name, 1, gregexpr(",", name)[[1]][1] - 1)
    date <- refs$Date[i]
    pitchType <- refs$TaggedPitchType[i]
    velo <- round(as.numeric(refs$ZoneSpeed[i]), 2)
    ret <- rbind(ret, c(lastName,date,pitchType,toString(velo),refs$PitchUID[i]))
  }
  colnames(ret) <- c("LastName", "Date", "PitchType", "ZoneSpeed", "PitchUID")
  return(ret)
}

#' @title Construct a Visual to Accompany a Note Left in the App
constructViz <- function(df) {
  plot_ly(type = "scatter",
          mode = "markers",
          x = ~df$PlateLocSide,
          y = ~df$PlateLocHeight,
          color = ~as.factor(df$TaggedPitchType),
          width = 500, height = 450,
          hoverinfo = "text",
          text = ~paste("Name:", df$Pitcher,
                        "\nVelo:", round(df$RelSpeed, 2),
                        "\nSpin:", round(df$SpinRate, 2),
                        "\nInd. Vert. Brk:", round(df$InducedVertBreak, 2),
                        "\nHorz. Brk", round(df$HorzBreak, 2),
                        "\nTilt:", df$Tilt)) %>% layout(
                          shapes = list(type = "rect",
                                        line = list(color = "blue"),
                                        x0 = -0.95,
                                        x1 = 0.95,
                                        xref = "x",
                                        y0 = 1.525,
                                        y1 = 3.316,
                                        yref = "y"),
                          showlegend = T,
                          xaxis = list(title = "Plate Side"),
                          yaxis = list(title = "Plate Height"))
}

#' @title Create the 'Pitch Outcomes' Table from the Main App
pitchOutcomes <- function(df, flag = T) {
  if (flag) {
    ret <- df %>% filter(BatterSide == "Left") %>% group_by(TaggedPitchType) %>%
      summarise(`Count` = n(),
                `# Swings` = sum(PitchCall %in% c("StrikeSwinging", "FoulBall", "InPlay")),
                `Swing %` = round(sum(PitchCall %in% c("StrikeSwinging", "FoulBall", "InPlay")) / n(),2),
                `St. Swing %` = round(sum(PitchCall %in% c("StrikeSwinging", "FoulBall", "InPlay")) / n(),2),
                `Whiff %` = round(sum(PitchCall %in% c("StrikeSwinging")) / n(),2))
  } else {
    ret <- df %>% filter(BatterSide == "Right") %>% group_by(TaggedPitchType) %>%
      summarise(`Count` = n(),
                `# Swings` = sum(PitchCall %in% c("StrikeSwinging", "FoulBall", "InPlay")),
                `Swing %` = round(sum(PitchCall %in% c("StrikeSwinging", "FoulBall", "InPlay")) / n(),2),
                `St. Swing %` = round(sum(PitchCall %in% c("StrikeSwinging", "FoulBall", "InPlay")) / n(),2),
                `Whiff %` = round(sum(PitchCall %in% c("StrikeSwinging")) / n(),2))
  }
  return(ret)
}

#' @title Historical Thresholds for Probability Calibration
#' @description Because it's very hard to evaluate models that predict probabilities with no
#'              true outcomes given, this function returns historically-seen rates of event
#'              occurrence, such that predicted probabilities are then calculated and calibrated in
#'              'useModels'.
calThresh <- function(games, resp = "") {
  if (resp == "Whiff") {
    swings <- games %>% filter(PitchCall %in% c("StrikeSwinging", "InPlay", "FoulBall"))
    swingRates <- table(swings$PitchCall) / nrow(swings)
    return(unname(swingRates[which(names(swingRates) == "StrikeSwinging")]))
  } else {
    swings <- games %>% filter(PitchCall %in% c("StrikeSwinging", "InPlay", "FoulBall",
                                                "BallCalled", "StrikeCalled"))
    generalRates <- table(games$PitchCall) / nrow(games)
    return(unname(generalRates[which(names(generalRates) == "StrikeCalled")]))
  }
}
