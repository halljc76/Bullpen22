loginUser <- function(conn, user, pass) {
  # Returns NA --> Username not found
  # Returns FALSE --> Wrong password
  # Returns TRUE --> Authenticated User

  ret <- unname(pass == dbGetQuery(conn, glue(
    "SELECT L.Password FROM Logins L
   WHERE L.Login = '{name}'", name = user)))[1]
  if (is.na(ret)) {
    return(F)
  } else {
    return(ret)
  }
}

getPitcher <- function(conn, login) {
  if (login == "analytics") {
    return("Buchner, Bradley")
  }

  ret <- dbGetQuery(conn, glue("SELECT L.name FROM Logins L WHERE L.login = '{l}'",
                               l = login))[[1]]
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

getNotes <- function(conn) {
  print(dbGetQuery(conn, "SELECT * FROM Notes ORDER BY noteid DESC;"))
  return(dbGetQuery(conn, "SELECT * FROM Notes ORDER BY noteid DESC;"))
}

getSpecRefs <- function(conn, noteid) {
  ret <- dbGetQuery(conn, glue("SELECT R.pitchuid FROM Refs L WHERE R.noteid = '{n}'",
                               n = noteid))[[1]]
}
