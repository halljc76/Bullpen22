#' @title Collect Videos
#' @description This function uses our AWS system to grab all stored TM Portable videos.
#' @params back := `T` if desiring 'backview' vidoes (from behind pitcher), `F` if desiring 'sideview' videos (beside pitcher)
getVids <- function(back = T) {
  if (back) {
    # Get all the unique filenames in the bucket
    return(as.vector(unique(data.table::rbindlist(get_bucket(bucket = "uncbullpen", prefix = "video/backview/"))$Key)))
  } else {
    # Get all the unique filenames in the bucket
    return(as.vector(unique(data.table::rbindlist(get_bucket(bucket = "uncbullpen", prefix = "video/sideview/"))$Key)))
  }
}

getMatches <- function() {
  ret <- data.frame() # Output df
  
  # Get all the unique filenames in the bucket
  keys <- unique(data.table::rbindlist(get_bucket(bucket = "uncbullpen", prefix = "video/matching"))$Key)
  
  # For each file...
  for (key in keys) {
    # Ensure it's a csv
    if (gregexpr(".fst", key)[[1]][1] != -1) {
      # Read it in
      temp <- s3read_using(FUN = read.fst, bucket = "uncbullpen", object = key)
      # colnames(temp)[1] <- "PitchNo" # Fix a little read-in error
      ret <- rbind(ret, temp[,c("PlayID","VideoName")]) # Bind everything
    }
  }
  return(ret) # Return the data
}

