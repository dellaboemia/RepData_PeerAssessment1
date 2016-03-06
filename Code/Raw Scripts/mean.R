# Coursera Data Science Specialization
# Reproducible Research
# Peer Assessment Project 1
# Load.R 

# This script downloads the activity data set from the course website, stores it in the raw data directory, then loads
# it into a data frame for further processing.

library(downloader)

# Set Url for data, directory and file names
homeDir <- getwd()
rawDir <- "/Data/Raw Data"
dataUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
dataZipFile <- "activity.zip"
dataCsvFile <- "activity.csv"

# Create raw data directory if does not already exist 
if (!dir.exists(file.path(homeDir, rawDir))) {
  dir.create(file.path(homeDir, rawDir), recursive = TRUE)
}
setwd(file.path(homeDir, rawDir))

# Download the raw data file if it doesn't exist or it wasn't downloaded today.
today <- Sys.Date()
cDate <- as.Date(file.info(dataZipFile)$ctime)
if ((!file.exists(dataZipFile)) | (cDate != today)){
  download(dataUrl, dest = dataZipFile, mode="wb")
  unzip(dataZipFile)
  activities <- read.csv(dataCsvFile)
}

#Reset home directory
setwd(homeDir)