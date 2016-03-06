# Coursera Data Science Specialization
# Reproducible Research
# Peer Assessment Project 1
# Author: John James
# Date: March 5, 2016
# load.R

## ---- loadDataFunc
loadZipData <- function(url, dir, zip, csv ) {
  # This function downloads a zipped file from the web and stores it in the requested library. Note: This script requires that the current working directory be
  # the top directory for the project.
  # Args:
  #   url - the url for website from which the file will be downloaded
  #   dir - the redirectory to which the file will be stored
  #   zip - the name of the zip file
  #   csv - the name of the csv file
  #
  # Response: This function will load the file into a data frame and return it to the calling script
  
  # Validate arguments
  if (missing(url)) { stop("Missing url for data") }
  if (missing(dir)) { stop("Directory to which the data is to be stored, must be specified") }
  if (missing(zip)) { stop("The name of the zip file must be specified") }
  if (missing(csv)) { stop("The name of the csv file must be specified") }

  # Create raw data directory if does not already exist
  homeDir <- getwd()
  if (!dir.exists(file.path(homeDir, dir))) {
    dir.create(file.path(homeDir, dir), recursive = TRUE)
  }
  setwd(file.path(homeDir, dir))
  
  # Download the raw data file if it doesn't exist or it wasn't downloaded today.
  today <- Sys.Date()
  cDate <- as.Date(file.info(zip)$ctime)
  if ((!file.exists(zip)) | (cDate != today)){
    download(url, dest = zip, mode="wb")
    unzip(zip)
  }
  
  # Read csv into activities data frame
  df <- read.csv(csv)
  
  # Reset working directory back to project home directory
  setwd(homeDir)
  
  # Return the dataframe
  return(df)

}
# ---- end