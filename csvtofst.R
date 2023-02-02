### CSV to FST SCRIPT ###

# 1. Load packages and AWS credentials

library(aws.s3)
library(fst)
library(data.table)
readRenviron("./.Renviron")

# 2. Specify where are the csv files you downloaded from FileZilla

path <- "./tempDir" # Change this to wherever .csv files are!
keys <- list.files(path)

# 3. Run loop

# For each file...
for (key in keys) {
  # Ensure it's a csv
  if (gregexpr(".csv", key)[[1]][1] != -1) {
    # Read it in
    temp <- read.csv()
    colnames(temp)[1] <- "PitchNo" # Fix a little read-in error -- happened on some files 
    
    fp <- paste0("Data/",substr(key, 1, nchar(key)-4),".fst") # Place into 'Data' folder in bucket
    s3write_using(
      x = temp,
      FUN = write.fst,
      bucket = "uncbullpen",
      object = fp
    )
  }
}

# 4. Check the updated files are properly in AWS! Check no .csvs have been uploaded, either.