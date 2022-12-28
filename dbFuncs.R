getUsers <- function(conn) {
  dbGetQuery(conn, "SELECT L.name FROM Logins L;")
}

loginUser <- function(conn, pass) {
  return(pass == 'gdtbath')
}

getName <- function(conn, login) {
  return(dbGetQuery(conn, glue("SELECT L.name FROM Logins L WHERE L.login = '{l}'",
                               l = login))[[1]])
}

getPitcher <- function(conn, login) {
  if (login == "analytics") {
    return("Buchner, Bradley")
  }

  ret <- getName(conn, login)
  names <- strsplit(ret, " ")
  return(paste(names[[1]][2], names[[1]][1], sep = ", "))
}

addNote <- function(conn, login, title, message, listUID) {
  q1 <- dbGetQuery(conn, "SELECT COUNT(*) FROM Notes;")[[1]] + 1
  if (is.na(q1)) {
    q1 <- 1
  } else {
    q1 <- unname(q1) # Yes, this is ugly code.
    noteID <- q1      # It works. :)
    print(paste("NoteID", noteID))
  }

  message <- cleanString(message)
  title <- cleanString(title)

  print(paste0("Cleaned Title: ", title))
  print(paste0("Cleaned Message: ", message))

  prep <- paste0("INSERT INTO Notes VALUES (", q1, ",'",
           as.character.Date(Sys.Date()), "','", login, "',", "'", title, "',", "'", message, "'", ");")
  print(prep)

  suppressWarnings(dbGetQuery(conn, prep))

  for (uid in as.list(listUID)) {
    q2 <- dbGetQuery(conn, "SELECT COUNT(*) FROM Refs;")[[1]] + 1
    if (is.na(q2)) {
      q2 <- 1
    } else {
      q2 <- unname(q2) # Yes, this is ugly code.
      refID <- q2      # It works. :)
      print(paste("RefID", refID))
    }

    prep <- paste0("INSERT INTO Refs VALUES (", refID, ",", toString(q1), ",",
                                             "'", uid, "'", ");")
    print(prep)
    suppressWarnings(dbGetQuery(conn, prep))
  }

  print(dbGetQuery(conn, "SELECT * FROM Notes;"))
  print(dbGetQuery(conn, "SELECT * FROM Refs;"))
}

cleanString <- function(msg) {
  idx0 <- gregexpr("''",msg)[[1]][1]
  if (idx0 != -1) {
    before <- substr(msg,1,idx0-1)
    after <- substr(msg,idx0+2,nchar(msg))
    return(paste0(before,"'",after))
  }

  idx <- gregexpr("'",msg)[[1]][1]
  if (idx != -1) {
    before <- substr(msg,1,idx-1)
    after <- substr(msg,idx+1,nchar(msg))
    return(paste0(before,"''",after))
  } else {
    return(msg)
  }
}

getNotes <- function(conn) {
  return(dbGetQuery(conn, "SELECT * FROM Notes ORDER BY noteid DESC;"))
}

getSpecRefs <- function(conn, noteid) {
  ret <- dbGetQuery(conn, glue("SELECT R.pitchuid FROM Refs L WHERE R.noteid = '{n}'",
                               n = noteid))[[1]]
}

